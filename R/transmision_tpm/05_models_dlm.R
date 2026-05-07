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

available_controls_formula <- function(dat) {
  controls <- c()
  if ("dbcp_2y" %in% names(dat) && any(!is.na(dat$dbcp_2y))) controls <- c(controls, "dbcp_2y")
  if ("dbcp_5y" %in% names(dat) && any(!is.na(dat$dbcp_5y))) controls <- c(controls, "dbcp_5y")
  if ("dbcp_10y" %in% names(dat) && any(!is.na(dat$dbcp_10y))) controls <- c(controls, "dbcp_10y")

  if (length(controls) == 0) return("1")
  paste(controls, collapse = " + ")
}

estimate_dlm_product <- function(df, product_name, k = 6, asymmetric = FALSE) {
  dat <- df |>
    dplyr::filter(product == product_name) |>
    dplyr::arrange(date) |>
    add_lags(
      vars = if (asymmetric) c("dtpm_pos", "dtpm_neg") else c("dtpm"),
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
      paste0("dtpm_pos_l", 0:k, collapse = " + "),
      paste0("dtpm_neg_l", 0:k, collapse = " + "),
      sep = " + "
    )
  }

  controls <- available_controls_formula(dat)
  fml <- stats::as.formula(paste("drate ~", rhs_tpm, "+ drate_l1 +", controls, "+ month_fe"))

  model <- stats::lm(fml, data = dat)

  list(
    product = product_name,
    model = model,
    tidy = nw_tidy(model, lag = k),
    data = dat,
    formula = fml
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
      type = "total"
    ) |>
      dplyr::mutate(cumulative = cumsum(beta))
  } else {
    vals_pos <- tibble::tibble(
      horizon = 0:k,
      beta = purrr::map_dbl(0:k, ~ get_coef(paste0("dtpm_pos_l", .x))),
      type = "alza_tpm"
    )

    vals_neg <- tibble::tibble(
      horizon = 0:k,
      beta = purrr::map_dbl(0:k, ~ get_coef(paste0("dtpm_neg_l", .x))),
      type = "baja_tpm"
    )

    vals <- dplyr::bind_rows(vals_pos, vals_neg) |>
      dplyr::group_by(type) |>
      dplyr::mutate(cumulative = cumsum(beta)) |>
      dplyr::ungroup()
  }

  vals |>
    dplyr::mutate(product = est_obj$product)
}

estimate_all_dlm <- function(model_data, k = 6, asymmetric = FALSE) {
  products <- sort(unique(model_data$product))
  purrr::map(products, ~ estimate_dlm_product(model_data, .x, k = k, asymmetric = asymmetric))
}
