# ============================================================
# Modelos de rezagos distribuidos
# ============================================================

add_lags <- function(df, vars, lags = 0:6) {
  out <- df
  for (v in vars) {
    if (!v %in% names(out)) next
    for (l in lags) {
      out[[paste0(v, "_l", l)]] <- dplyr::lag(out[[v]], l)
    }
  }
  out
}

nw_tidy <- function(model, lag = 6) {
  vc <- sandwich::NeweyWest(model, lag = lag, prewhite = FALSE, adjust = TRUE)
  broom::tidy(lmtest::coeftest(model, vcov. = vc))
}

has_non_na <- function(dat, nm) {
  nm %in% names(dat) && any(!is.na(dat[[nm]]))
}

macro_controls_formula <- function(dat) {
  controls <- c()
  if (has_non_na(dat, "infl_yoy")) controls <- c(controls, "infl_yoy")
  if (has_non_na(dat, "dlog_tc_12")) controls <- c(controls, "dlog_tc_12")
  if (has_non_na(dat, "imacec_yoy")) controls <- c(controls, "imacec_yoy")
  controls
}

curve_controls_formula <- function(dat, product_name) {
  controls <- c()

  # Para vivienda UF, la curva real es más coherente que BCP nominal.
  if (product_name == "vivienda_uf") {
    if (has_non_na(dat, "dbcu_5y")) controls <- c(controls, "dbcu_5y")
    if (has_non_na(dat, "dbcu_10y")) controls <- c(controls, "dbcu_10y")
    return(controls)
  }

  # Para captaciones, NO se agregan BCP/BCU por defecto: son demasiado cercanas
  # al mecanismo de transmisión y pueden absorber el efecto de la TPM.
  if (grepl("^cap_", product_name)) {
    return(controls)
  }

  # Para consumo/comercial, BCP 2y y 5y quedan como robustez, no como base.
  if (has_non_na(dat, "dbcp_2y")) controls <- c(controls, "dbcp_2y")
  if (has_non_na(dat, "dbcp_5y")) controls <- c(controls, "dbcp_5y")
  controls
}

build_control_terms <- function(dat, product_name, spec = c("base", "macro", "curve", "macro_curve")) {
  spec <- match.arg(spec)

  controls <- c()

  if (spec %in% c("macro", "macro_curve")) {
    controls <- c(controls, macro_controls_formula(dat))
  }

  if (spec %in% c("curve", "macro_curve")) {
    controls <- c(controls, curve_controls_formula(dat, product_name))
  }

  unique(controls)
}

rhs_join <- function(...) {
  terms <- unlist(list(...), use.names = FALSE)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  paste(unique(terms), collapse = " + ")
}

estimate_dlm_product <- function(df, product_name, k = 6, asymmetric = FALSE,
                                 spec = c("macro", "base", "curve", "macro_curve")) {
  spec <- match.arg(spec)

  dat <- df |>
    dplyr::filter(product == product_name) |>
    dplyr::arrange(date)

  # Compatibilidad: si el panel fue generado con una versión antigua, se crean las nuevas variables.
  if (!"dtpm_up" %in% names(dat)) dat$dtpm_up <- pmax(dat$dtpm, 0)
  if (!"dtpm_down" %in% names(dat)) dat$dtpm_down <- pmax(-dat$dtpm, 0)

  dat <- dat |>
    add_lags(
      vars = if (asymmetric) c("dtpm_up", "dtpm_down") else c("dtpm"),
      lags = 0:k
    ) |>
    dplyr::mutate(
      month_fe = factor(lubridate::month(date)),
      drate_l1 = dplyr::lag(drate, 1)
    )

  if (!asymmetric) {
    rhs_tpm <- paste0("dtpm_l", 0:k, collapse = " + ")
  } else {
    rhs_tpm <- paste(
      paste0("dtpm_up_l", 0:k, collapse = " + "),
      paste0("dtpm_down_l", 0:k, collapse = " + "),
      sep = " + "
    )
  }

  controls <- build_control_terms(dat, product_name, spec = spec)
  rhs <- rhs_join(rhs_tpm, "drate_l1", controls, "month_fe")
  fml <- stats::as.formula(paste("drate ~", rhs))

  model <- stats::lm(fml, data = dat)

  list(
    product = product_name,
    model = model,
    tidy = nw_tidy(model, lag = k),
    data = dat,
    formula = fml,
    spec = spec
  )
}

extract_cumulative_pt <- function(est_obj, k = 6, asymmetric = FALSE) {
  coefs <- stats::coef(est_obj$model)

  get_coef <- function(nm) {
    val <- unname(coefs[nm])
    if (length(val) == 0 || is.na(val)) 0 else val
  }

  if (!asymmetric) {
    vals <- tibble::tibble(
      horizon = 0:k,
      beta = purrr::map_dbl(0:k, ~ get_coef(paste0("dtpm_l", .x))),
      beta_signed = beta,
      type = "total",
      reported_scale = "signed"
    ) |>
      dplyr::mutate(cumulative = cumsum(beta))
  } else {
    vals_up <- tibble::tibble(
      horizon = 0:k,
      beta_signed = purrr::map_dbl(0:k, ~ get_coef(paste0("dtpm_up_l", .x))),
      beta = beta_signed,
      type = "alza_tpm",
      reported_scale = "signed"
    )

    vals_down <- tibble::tibble(
      horizon = 0:k,
      # dtpm_down es la magnitud positiva de la baja. El coeficiente firmado esperado es negativo.
      beta_signed = purrr::map_dbl(0:k, ~ get_coef(paste0("dtpm_down_l", .x))),
      # Para comparar intensidad de transmisión, se reporta la magnitud en la dirección esperada.
      beta = -beta_signed,
      type = "baja_tpm",
      reported_scale = "magnitude_expected_direction"
    )

    vals <- dplyr::bind_rows(vals_up, vals_down) |>
      dplyr::group_by(type) |>
      dplyr::mutate(cumulative = cumsum(beta)) |>
      dplyr::ungroup()
  }

  vals |>
    dplyr::mutate(
      product = est_obj$product,
      spec = est_obj$spec %||% NA_character_
    )
}

estimate_all_dlm <- function(model_data, k = 6, asymmetric = FALSE,
                             spec = c("macro", "base", "curve", "macro_curve")) {
  spec <- match.arg(spec)
  products <- sort(unique(model_data$product))
  purrr::map(products, ~ estimate_dlm_product(model_data, .x, k = k, asymmetric = asymmetric, spec = spec))
}
