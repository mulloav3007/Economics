# ============================================================
# imacec_utils.R
# Utilidades generales: API BCCh, transformaciones y fechas
# ============================================================

assert_bcch_credentials <- function(user = USER_BCCH, pass = PASS_BCCH) {
  if (identical(user, "") || identical(pass, "")) {
    stop(
      "Faltan credenciales BCCh. Define BCCH_USER y BCCH_PASS en tu .Renviron local. ",
      "No escribas credenciales directamente en scripts públicos."
    )
  }
  invisible(TRUE)
}

bcch_url <- function(series_code, firstdate, lastdate, user = USER_BCCH, pass = PASS_BCCH) {
  assert_bcch_credentials(user, pass)
  base <- "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"
  paste0(
    base,
    "?user=", URLencode(user, reserved = TRUE),
    "&pass=", URLencode(pass, reserved = TRUE),
    "&firstdate=", firstdate,
    "&lastdate=", lastdate,
    "&timeseries=", series_code,
    "&function=GetSeries"
  )
}

fetch_series <- function(code, firstdate = first_date, lastdate = last_date) {
  if (is.null(code) || identical(code, "")) stop("Código de serie vacío o NULL.")

  j <- rjson::fromJSON(file = bcch_url(code, firstdate, lastdate))

  pull_obs <- function(node) {
    if (is.null(node) || is.null(node$Obs) || length(node$Obs) == 0) return(NULL)

    tibble::tibble(
      date  = lubridate::dmy(vapply(node$Obs, function(x) x[["indexDateString"]], character(1))),
      value = suppressWarnings(as.numeric(vapply(node$Obs, function(x) x[["value"]], character(1))))
    )
  }

  out <- list()
  if (!is.null(j$Series))    out[[length(out) + 1]] <- pull_obs(j$Series)
  if (!is.null(j$DAILY))     out[[length(out) + 1]] <- pull_obs(j$DAILY)
  if (!is.null(j$MONTHLY))   out[[length(out) + 1]] <- pull_obs(j$MONTHLY)
  if (!is.null(j$WEEKLY))    out[[length(out) + 1]] <- pull_obs(j$WEEKLY)
  if (!is.null(j$QUARTERLY)) out[[length(out) + 1]] <- pull_obs(j$QUARTERLY)
  if (!is.null(j$ANNUAL))    out[[length(out) + 1]] <- pull_obs(j$ANNUAL)

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(tibble::tibble(date = as.Date(character()), value = numeric()))

  dplyr::bind_rows(out) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::arrange(date)
}

yoy <- function(x) {
  (x / dplyr::lag(x, 12) - 1) * 100
}

last_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(utils::tail(x, 1))
}

month_label_es <- function(fecha) {
  # Evita depender de locale del sistema operativo.
  meses <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )
  paste(meses[lubridate::month(fecha)], lubridate::year(fecha))
}

clean_period <- function(x) {
  as.Date(lubridate::floor_date(as.Date(x), "month"))
}
