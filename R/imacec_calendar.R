# ============================================================
# imacec_calendar.R
# Lectura y construcción de variable de días hábiles
# ============================================================

read_calendar <- function(path = cal_path) {
  if (!file.exists(path)) {
    stop(
      "No se encontró el calendario de días hábiles en: ", path, "\n",
      "Copia el archivo cal_1985_2030.xlsx en data/raw/ o define IMACEC_CAL_PATH en .Renviron."
    )
  }

  cal_raw <- readxl::read_excel(path)

  if (!"mes" %in% names(cal_raw)) names(cal_raw)[1] <- "mes"

  periodo_try <- suppressWarnings(lubridate::ymd(cal_raw$mes))
  if (all(is.na(periodo_try))) periodo_try <- suppressWarnings(lubridate::dmy(cal_raw$mes))
  if (all(is.na(periodo_try)) && is.numeric(cal_raw$mes)) {
    periodo_try <- as.Date(cal_raw$mes, origin = "1899-12-30")
  }

  required_cols <- c("lun", "mar", "mie", "jue", "vie", "lun_f", "mar_f", "mie_f", "jue_f", "vie_f")
  missing_cols <- setdiff(required_cols, names(cal_raw))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas en calendario: ", paste(missing_cols, collapse = ", "))
  }

  cal_raw |>
    dplyr::mutate(Periodo = lubridate::floor_date(periodo_try, "month")) |>
    dplyr::mutate(
      dias_habiles_nivel = (lun + mar + mie + jue + vie) -
        (lun_f + mar_f + mie_f + jue_f + vie_f)
    ) |>
    dplyr::arrange(Periodo) |>
    dplyr::mutate(dias_habiles = dias_habiles_nivel - dplyr::lag(dias_habiles_nivel, 12)) |>
    dplyr::select(Periodo, dias_habiles)
}
