# ============================================================
# 01_data.R
# Limpieza de datos de mercado para el monitor Chile.
# ============================================================

estres_market_columns <- c(
  "clp", "y10_clp", "y10_tsy", "vix", "dtw",
  "pcu", "wti", "cny", "eq_cny", "eq_nsq"
)

estres_log_columns <- c("clp", "pcu", "wti", "vix", "dtw", "cny", "eq_cny", "eq_nsq")

estres_exchange_column_map <- c(
  "Unnamed: 0" = "date",
  "PCU" = "pcu",
  "AUX" = "aux",
  "WTI" = "wti",
  "BRL" = "brl",
  "CLP" = "clp",
  "CNY" = "cny",
  "COL" = "cop",
  "MXN" = "mxn",
  "PEN" = "pen",
  "EQBRL" = "eq_brl",
  "EQCLP" = "eq_clp",
  "EQCNY" = "eq_cny",
  "EQCOL" = "eq_col",
  "EQDJI" = "eq_dji",
  "EQNSQ" = "eq_nsq",
  "EQMXN" = "eq_mxn",
  "EQPEN" = "eq_pen",
  "10YBRL" = "y10_brl",
  "10YCLP" = "y10_clp",
  "10YCOL" = "y10_col",
  "10YTSY" = "y10_tsy",
  "10YMXN" = "y10_mxn",
  "10YPEN" = "y10_pen",
  "VIX" = "vix",
  "DTW" = "dtw"
)

estres_import_exchange_excel <- function(
    xlsx_file,
    output_csv = estres_path("data/raw/estres_financiero/merged_full_dataset.csv")
) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Para importar directamente el Excel original instala readxl: install.packages('readxl').",
      call. = FALSE
    )
  }

  raw <- readxl::read_excel(xlsx_file)
  old_names <- names(raw)
  new_names <- old_names

  for (old in names(estres_exchange_column_map)) {
    new_names[old_names == old] <- unname(estres_exchange_column_map[[old]])
  }

  raw <- raw |>
    rlang::set_names(new_names) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::arrange(.data$date)

  estres_make_dirs()
  readr::write_csv(raw, output_csv)
  invisible(raw)
}

estres_read_raw_market_data <- function(
    raw_file = estres_path("data/raw/estres_financiero/merged_full_dataset.csv")
) {
  if (!file.exists(raw_file)) {
    stop(
      "No existe el archivo raw: ", raw_file,
      "\nCopia data/raw/estres_financiero/merged_full_dataset.csv o importa el Excel original con estres_import_exchange_excel().",
      call. = FALSE
    )
  }

  readr::read_csv(raw_file, show_col_types = FALSE) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::arrange(.data$date)
}

estres_prepare_market_data <- function(raw_data, start_date = as.Date("2012-10-05")) {
  missing_cols <- setdiff(c("date", estres_market_columns), names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas en el archivo raw: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  raw_data |>
    dplyr::select(dplyr::all_of(c("date", estres_market_columns))) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(estres_market_columns),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(estres_market_columns),
        ~ estres_interpolate_numeric(.x, .data$date)
      )
    ) |>
    dplyr::filter(.data$date >= start_date) |>
    dplyr::mutate(
      trend = dplyr::row_number(),
      l_clp = estres_safe_log(.data$clp),
      l_pcu = estres_safe_log(.data$pcu),
      l_wti = estres_safe_log(.data$wti),
      l_vix = estres_safe_log(.data$vix),
      l_dtw = estres_safe_log(.data$dtw),
      l_cny = estres_safe_log(.data$cny),
      l_eq_cny = estres_safe_log(.data$eq_cny),
      l_eq_nsq = estres_safe_log(.data$eq_nsq)
    )
}

estres_write_market_data <- function(market_data, root = estres_project_root()) {
  estres_make_dirs(root)

  readr::write_csv(
    market_data,
    file.path(root, "data/processed/estres_financiero/market_data_chile.csv")
  )

  invisible(market_data)
}
