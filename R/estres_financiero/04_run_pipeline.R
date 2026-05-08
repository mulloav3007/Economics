# ============================================================
# 04_run_pipeline.R
# Pipeline completo del monitor de estrés financiero para Chile.
# ============================================================

run_estres_financiero_pipeline <- function(root = estres_project_root()) {
  estres_load_packages()
  estres_make_dirs(root)

  raw <- estres_read_raw_market_data(
    raw_file = file.path(root, "data/raw/estres_financiero/merged_full_dataset.csv")
  )

  market_data <- estres_prepare_market_data(raw)
  estres_write_market_data(market_data, root = root)

  fx_fit <- estres_estimate_fx_model(market_data)
  y10_fit <- estres_estimate_y10_model(market_data)
  index_data <- estres_construct_index(fx_fit, y10_fit)

  estres_write_outputs(index_data, fx_fit, y10_fit, root = root)
  estres_save_figures(index_data, root = root)

  latest <- index_data |>
    dplyr::filter(
      !is.na(.data$stress_market),
      !is.na(.data$stress_market_30d),
      !is.na(.data$z_fx),
      !is.na(.data$z_y10)
    ) |>
    dplyr::slice_tail(n = 1)

  message("Pipeline terminado.")
  message("Última observación completa: ", as.character(latest$date[[1]]))
  message("Índice 30d: ", round(latest$stress_market_30d[[1]], 3), " | Régimen: ", latest$regime[[1]])

  invisible(list(
    market_data = market_data,
    fx_fit = fx_fit,
    y10_fit = y10_fit,
    index_data = index_data,
    latest = latest
  ))
}
