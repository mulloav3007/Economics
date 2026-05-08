# ============================================================
# 02_models.R
# Modelos de normalización por fundamentos y construcción del índice.
# ============================================================

estres_check_min_obs <- function(model_data, model_name, min_obs = 250) {
  if (nrow(model_data) < min_obs) {
    stop(
      "Muy pocas observaciones para estimar ", model_name, ": ", nrow(model_data),
      ". Revisa datos faltantes o fecha de inicio.",
      call. = FALSE
    )
  }
  invisible(model_data)
}

estres_estimate_fx_model <- function(market_data) {
  model_data <- market_data |>
    dplyr::select(
      date, clp, l_clp, trend,
      l_pcu, l_wti, l_vix, l_dtw, l_cny, l_eq_nsq, l_eq_cny
    ) |>
    tidyr::drop_na()

  estres_check_min_obs(model_data, "modelo de tipo de cambio")

  model <- stats::lm(
    l_clp ~ trend + l_pcu + l_wti + l_vix + l_dtw + l_cny + l_eq_nsq + l_eq_cny,
    data = model_data
  )

  fitted_data <- model_data |>
    dplyr::mutate(
      fitted_l_clp = stats::fitted(model),
      fitted_clp = exp(.data$fitted_l_clp),
      res_fx = stats::resid(model),
      z_fx = estres_zscore(.data$res_fx)
    ) |>
    dplyr::select(date, clp, fitted_clp, res_fx, z_fx)

  list(model = model, fitted = fitted_data)
}

estres_estimate_y10_model <- function(market_data) {
  model_data <- market_data |>
    dplyr::select(
      date, y10_clp, y10_tsy, trend,
      l_vix, l_dtw, l_cny, l_eq_nsq
    ) |>
    tidyr::drop_na()

  estres_check_min_obs(model_data, "modelo de tasa soberana 10Y")

  model <- stats::lm(
    y10_clp ~ trend + y10_tsy + l_vix + l_dtw + l_cny + l_eq_nsq,
    data = model_data
  )

  fitted_data <- model_data |>
    dplyr::mutate(
      fitted_y10_clp = stats::fitted(model),
      res_y10 = stats::resid(model),
      z_y10 = estres_zscore(.data$res_y10)
    ) |>
    dplyr::select(date, y10_clp, fitted_y10_clp, res_y10, z_y10)

  list(model = model, fitted = fitted_data)
}

estres_classify_regime <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 1.5 ~ "Estrés alto",
    x >= 0.75 ~ "Estrés moderado",
    x <= -0.75 ~ "Condiciones benignas",
    TRUE ~ "Condiciones neutrales"
  )
}

estres_construct_index <- function(fx_fit, y10_fit) {
  fx_fit$fitted |>
    dplyr::inner_join(y10_fit$fitted, by = "date") |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      stress_market = (.data$z_fx + .data$z_y10) / 2,
      stress_market_30d = estres_roll_mean(.data$stress_market, width = 30, min_obs = 20),
      stress_fx_30d = estres_roll_mean(.data$z_fx, width = 30, min_obs = 20),
      stress_y10_30d = estres_roll_mean(.data$z_y10, width = 30, min_obs = 20),
      regime = estres_classify_regime(.data$stress_market_30d)
    )
}

estres_model_coefficients <- function(fx_fit, y10_fit) {
  dplyr::bind_rows(
    broom::tidy(fx_fit$model, conf.int = TRUE) |>
      dplyr::mutate(model = "Tipo de cambio USD/CLP", dependent_variable = "log(USDCLP)", .before = 1),
    broom::tidy(y10_fit$model, conf.int = TRUE) |>
      dplyr::mutate(model = "Tasa soberana 10Y CLP", dependent_variable = "10YCLP", .before = 1)
  )
}

estres_model_diagnostics <- function(fx_fit, y10_fit) {
  fx_glance <- broom::glance(fx_fit$model)
  y10_glance <- broom::glance(y10_fit$model)

  tibble::tibble(
    model = c("Tipo de cambio USD/CLP", "Tasa soberana 10Y CLP"),
    dependent_variable = c("log(USDCLP)", "10YCLP"),
    n_obs = c(stats::nobs(fx_fit$model), stats::nobs(y10_fit$model)),
    r_squared = c(fx_glance$r.squared, y10_glance$r.squared),
    adj_r_squared = c(fx_glance$adj.r.squared, y10_glance$adj.r.squared),
    sample_start = c(min(fx_fit$fitted$date, na.rm = TRUE), min(y10_fit$fitted$date, na.rm = TRUE)),
    sample_end = c(max(fx_fit$fitted$date, na.rm = TRUE), max(y10_fit$fitted$date, na.rm = TRUE)),
    residual_sd = c(stats::sd(fx_fit$fitted$res_fx, na.rm = TRUE), stats::sd(y10_fit$fitted$res_y10, na.rm = TRUE))
  )
}

estres_detect_episodes <- function(index_data, n = 15, min_distance_days = 30) {
  candidates <- index_data |>
    dplyr::filter(!is.na(.data$stress_market_30d)) |>
    dplyr::arrange(dplyr::desc(.data$stress_market_30d))

  selected <- vector("list", 0)

  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i, ]
    candidate_date <- candidate$date[[1]]

    far_enough <- if (length(selected) == 0) {
      TRUE
    } else {
      selected_dates <- as.Date(vapply(selected, function(x) as.character(x$date[[1]]), character(1)))
      all(abs(as.numeric(candidate_date - selected_dates)) >= min_distance_days)
    }

    if (far_enough) selected[[length(selected) + 1]] <- candidate
    if (length(selected) >= n) break
  }

  dplyr::bind_rows(selected) |>
    dplyr::select(date, stress_market_30d, stress_market, z_fx, z_y10, regime) |>
    dplyr::arrange(.data$date)
}

estres_write_outputs <- function(index_data, fx_fit, y10_fit, root = estres_project_root()) {
  estres_make_dirs(root)

  processed_dir <- file.path(root, "data/processed/estres_financiero")
  table_dir <- file.path(root, "outputs/tables/estres_financiero")

  readr::write_csv(index_data, file.path(processed_dir, "stress_index_chile.csv"))
  readr::write_csv(index_data, file.path(root, "data/processed/estres_financiero_chile.csv"))
  readr::write_csv(estres_model_coefficients(fx_fit, y10_fit), file.path(processed_dir, "model_coefficients.csv"))
  readr::write_csv(estres_model_diagnostics(fx_fit, y10_fit), file.path(processed_dir, "model_diagnostics.csv"))

  latest <- index_data |>
    dplyr::filter(
      !is.na(.data$stress_market),
      !is.na(.data$stress_market_30d),
      !is.na(.data$z_fx),
      !is.na(.data$z_y10)
    ) |>
    dplyr::slice_tail(n = 1)

  readr::write_csv(latest, file.path(processed_dir, "latest_snapshot.csv"))
  readr::write_csv(estres_detect_episodes(index_data), file.path(table_dir, "episodios_estres.csv"))

  invisible(index_data)
}
