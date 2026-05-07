# ============================================================
# Funciones API BDE Banco Central de Chile
# ============================================================

bde_base <- "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"

parse_bde_date <- function(x) {
  x <- as.character(x)

  out <- suppressWarnings(lubridate::dmy(x))
  idx <- is.na(out)
  if (any(idx)) out[idx] <- suppressWarnings(lubridate::ymd(x[idx]))
  idx <- is.na(out)
  if (any(idx)) out[idx] <- suppressWarnings(lubridate::mdy(x[idx]))

  as.Date(out)
}

parse_bde_value <- function(x) {
  x <- as.character(x)
  # La BDE a veces devuelve "NaN" en días sin dato. Lo tratamos como NA
  # para evitar miles de warnings de parse_number().
  x[stringr::str_to_lower(stringr::str_trim(x)) %in% c("", "na", "nan", "null")] <- NA_character_

  val_dot <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ".")))
  val_comma <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ",")))
  dplyr::coalesce(val_dot, val_comma)
}

check_bde_credentials <- function() {
  user <- Sys.getenv("BCCH_USER")
  pass <- Sys.getenv("BCCH_PASS")
  if (identical(user, "") || identical(pass, "")) {
    stop(
      "Faltan credenciales BDE. Copia .Renviron.example a .Renviron y define BCCH_USER/BCCH_PASS.",
      call. = FALSE
    )
  }
  list(user = user, pass = pass)
}

get_bde_series <- function(series_id, first_date = "2002-01-01", last_date = Sys.Date()) {
  cred <- check_bde_credentials()

  req <- httr2::request(bde_base) |>
    httr2::req_url_query(
      user = cred$user,
      pass = cred$pass,
      firstdate = as.character(first_date),
      lastdate = as.character(last_date),
      timeseries = series_id,
      `function` = "GetSeries"
    ) |>
    httr2::req_timeout(60)

  res <- httr2::req_perform(req)
  obj <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = TRUE)

  if (!is.null(obj$Codigo) && !identical(as.character(obj$Codigo), "0")) {
    stop("Error BDE serie ", series_id, ": ", obj$Descripcion %||% "sin descripción", call. = FALSE)
  }

  obs <- obj$Series$Obs

  if (is.null(obs) || length(obs) == 0 || nrow(obs) == 0) {
    return(tibble::tibble(
      date = as.Date(character()),
      value = numeric(),
      status = character(),
      series_id = series_id,
      title = obj$Series$descripEsp %||% NA_character_
    ))
  }

  tibble::tibble(
    date = parse_bde_date(obs$indexDateString),
    value = parse_bde_value(obs$value),
    status = as.character(obs$statusCode %||% "OK"),
    series_id = obj$Series$seriesId %||% series_id,
    title = obj$Series$descripEsp %||% NA_character_
  ) |>
    dplyr::filter(!is.na(date), !is.na(value), status == "OK") |>
    dplyr::arrange(date)
}

search_bde_catalog <- function(frequency = c("DAILY", "MONTHLY", "QUARTERLY", "ANNUAL")) {
  frequency <- match.arg(frequency)
  cred <- check_bde_credentials()

  req <- httr2::request(bde_base) |>
    httr2::req_url_query(
      user = cred$user,
      pass = cred$pass,
      frequency = frequency,
      `function` = "SearchSeries"
    ) |>
    httr2::req_timeout(60)

  res <- httr2::req_perform(req)
  obj <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = TRUE)

  if (!is.null(obj$Codigo) && !identical(as.character(obj$Codigo), "0")) {
    stop("Error BDE SearchSeries: ", obj$Descripcion %||% "sin descripción", call. = FALSE)
  }

  out <- tibble::as_tibble(obj$SeriesInfos)
  names(out) <- make.names(names(out))
  out |>
    dplyr::mutate(frequency = frequency)
}

fetch_bde_many <- function(series_tbl, first_date = "2002-01-01", last_date = Sys.Date(), stop_if_required_missing = TRUE) {
  pieces <- vector("list", nrow(series_tbl))
  log_rows <- vector("list", nrow(series_tbl))

  for (i in seq_len(nrow(series_tbl))) {
    nm <- series_tbl$name[[i]]
    sid <- series_tbl$series_id[[i]]
    req <- isTRUE(series_tbl$required[[i]])

    message("  - ", nm, " [", sid, "]")

    ans <- tryCatch(
      {
        dat <- get_bde_series(sid, first_date = first_date, last_date = last_date)
        dat$name <- nm
        dat$frequency <- series_tbl$frequency[[i]]
        dat$block <- series_tbl$block[[i]]
        list(ok = TRUE, data = dat, error = NA_character_, n = nrow(dat))
      },
      error = function(e) {
        list(ok = FALSE, data = tibble::tibble(), error = conditionMessage(e), n = 0)
      }
    )

    pieces[[i]] <- ans$data
    log_rows[[i]] <- tibble::tibble(
      name = nm,
      series_id = sid,
      required = req,
      ok = ans$ok,
      n_obs = ans$n,
      error = ans$error
    )

    if (!ans$ok && req && stop_if_required_missing) {
      stop("Falló serie requerida: ", nm, " / ", sid, "\n", ans$error, call. = FALSE)
    }
  }

  list(
    data = dplyr::bind_rows(pieces),
    log = dplyr::bind_rows(log_rows)
  )
}
