# ============================================================
# Construcción de base mensual
# ============================================================

safe_last <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  dplyr::last(x)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

to_monthly <- function(df, method = c("mean", "last")) {
  method <- match.arg(method)

  df |>
    dplyr::mutate(date = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(name, series_id, title, frequency, block, date) |>
    dplyr::summarise(
      value = if (method == "mean") safe_mean(value) else safe_last(value),
      .groups = "drop"
    )
}

build_monthly_panel <- function(raw_data, dict) {
  if (nrow(raw_data) == 0) stop("raw_data está vacío. Revisa credenciales y códigos de series.", call. = FALSE)

  raw_data <- raw_data |>
    dplyr::filter(!is.na(value), !is.na(date)) |>
    dplyr::distinct(name, date, .keep_all = TRUE)

  daily_names <- dict |>
    dplyr::filter(frequency == "DAILY") |>
    dplyr::pull(name)

  daily_tpm <- raw_data |>
    dplyr::filter(name == "tpm") |>
    to_monthly(method = "last")

  daily_other <- raw_data |>
    dplyr::filter(name %in% setdiff(daily_names, "tpm")) |>
    to_monthly(method = "mean")

  daily_monthly <- dplyr::bind_rows(daily_tpm, daily_other)

  monthly_original <- raw_data |>
    dplyr::filter(!name %in% daily_names) |>
    dplyr::mutate(date = lubridate::floor_date(date, "month")) |>
    dplyr::select(name, series_id, title, frequency, block, date, value)

  panel <- dplyr::bind_rows(daily_monthly, monthly_original) |>
    dplyr::select(date, name, value) |>
    dplyr::arrange(date, name) |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::arrange(date)

  # Completa meses intermedios para rezagos consistentes.
  full_dates <- tibble::tibble(
    date = seq(min(panel$date, na.rm = TRUE), max(panel$date, na.rm = TRUE), by = "month")
  )

  full_dates |>
    dplyr::left_join(panel, by = "date") |>
    dplyr::arrange(date)
}

make_model_data <- function(monthly_panel) {
  product_cols <- intersect(lending_products, names(monthly_panel))
  if (!"tpm" %in% names(monthly_panel)) stop("No existe columna tpm en monthly_panel.", call. = FALSE)
  if (length(product_cols) == 0) stop("No hay productos de tasas disponibles para modelar.", call. = FALSE)

  control_cols <- intersect(market_controls, names(monthly_panel))

  base <- monthly_panel |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      dtpm = tpm - dplyr::lag(tpm),

      # Cambios de TPM separados por signo.
      # dtpm_up mide alzas en puntos porcentuales.
      # dtpm_down mide bajas como magnitud positiva: una baja de -1 pp queda como 1.
      dtpm_up = pmax(dtpm, 0),
      dtpm_down = pmax(-dtpm, 0),
      dtpm_pos = dtpm_up,
      dtpm_neg = pmin(dtpm, 0)
    )

  # Controles de curva: se crean, pero NO se usan en el modelo principal.
  # Quedan disponibles para especificaciones de robustez.
  for (cc in control_cols) {
    base[[paste0("d", cc)]] <- base[[cc]] - dplyr::lag(base[[cc]])
  }

  # Controles macro mínimos.
  # infl_yoy: inflación anual aproximada como diferencia logarítmica *100.
  # dlog_tc_12: depreciación anual del peso respecto al dólar observado.
  # imacec_yoy: crecimiento anual del IMACEC si la serie está disponible.
  if ("ipc" %in% names(base)) {
    base <- base |>
      dplyr::mutate(
        log_ipc = 100 * log(ipc),
        infl_yoy = log_ipc - dplyr::lag(log_ipc, 12),
        infl_mom = log_ipc - dplyr::lag(log_ipc, 1)
      )
  }

  if ("tc_obs" %in% names(base)) {
    base <- base |>
      dplyr::mutate(
        log_tc = 100 * log(tc_obs),
        dlog_tc = log_tc - dplyr::lag(log_tc, 1),
        dlog_tc_12 = log_tc - dplyr::lag(log_tc, 12)
      )
  }

  if ("imacec" %in% names(base)) {
    base <- base |>
      dplyr::mutate(
        log_imacec = 100 * log(imacec),
        imacec_yoy = log_imacec - dplyr::lag(log_imacec, 12)
      )
  }

  out <- base |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(product_cols),
      names_to = "product",
      values_to = "rate"
    ) |>
    dplyr::group_by(product) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      drate = rate - dplyr::lag(rate),
      spread_tpm = rate - tpm
    ) |>
    dplyr::ungroup()

  out |>
    dplyr::filter(!is.na(rate), !is.na(drate), !is.na(dtpm))
}
