# ============================================================
# imacec_data.R
# Descarga y construcción de la base con dos cortes informativos
# ============================================================

fetch_series_optional <- function(code, var_name = NULL) {
  tryCatch(
    fetch_series(code),
    error = function(e) {
      warning("No se pudo descargar ", var_name %||% code, ": ", conditionMessage(e), call. = FALSE)
      tibble::tibble(date = as.Date(character()), value = numeric())
    }
  )
}

monthly_series <- function(code, name, transform = identity) {
  fetch_series(code) |>
    dplyr::mutate(Periodo = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(Periodo) |>
    dplyr::summarise(value = dplyr::last(value), .groups = "drop") |>
    dplyr::arrange(Periodo) |>
    dplyr::mutate(value = transform(value)) |>
    dplyr::rename(!!name := value)
}

get_eee_expectations <- function() {
  series <- list(
    eee_imacec = codes$eee_imacec,
    eee_imacec_nm = codes$eee_imacec_nm
  )
  raw <- purrr::imap_dfr(series, function(code, name) {
    fetch_series_optional(code, name) |>
      dplyr::mutate(variable = name)
  })
  if (!nrow(raw)) {
    return(tibble::tibble(
      survey_period = as.Date(character()), Periodo = as.Date(character()),
      eee_imacec = numeric(), eee_imacec_nm = numeric()
    ))
  }

  out <- raw |>
    dplyr::filter(!is.na(date), !is.na(value)) |>
    dplyr::mutate(
      survey_period = lubridate::floor_date(date, "month"),
      # La EEE publicada en M pregunta por el IMACEC de M-1.
      Periodo = survey_period %m-% lubridate::period(months = 1)
    ) |>
    dplyr::group_by(variable, survey_period, Periodo) |>
    dplyr::summarise(value = dplyr::last(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::arrange(Periodo)
  if (!"eee_imacec" %in% names(out)) out$eee_imacec <- NA_real_
  if (!"eee_imacec_nm" %in% names(out)) out$eee_imacec_nm <- NA_real_
  out
}

get_uf_monthly <- function() {
  fetch_series(codes$uf_diaria) |>
    dplyr::mutate(Periodo = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(Periodo) |>
    dplyr::slice_max(date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(Periodo, uf_nivel = value)
}

normalize_ivs_text <- function(x) {
  x <- iconv(tolower(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]+", " ", x)
}

parse_ivs_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "..", "...")] <- NA_character_
  both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
  x[both] <- gsub(".", "", x[both], fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_ivs_period <- function(x) {
  if (inherits(x, "Date")) return(lubridate::floor_date(as.Date(x), "month"))
  if (inherits(x, "POSIXt")) return(lubridate::floor_date(as.Date(x), "month"))

  raw <- as.character(x)
  num <- suppressWarnings(as.numeric(raw))
  out <- as.Date(rep(NA_character_, length(raw)))
  excel <- !is.na(num) & num > 20000 & num < 80000
  out[excel] <- as.Date(num[excel], origin = "1899-12-30")

  z <- normalize_ivs_text(raw)
  months_es <- c(
    ene = "01", enero = "01", feb = "02", febrero = "02", mar = "03", marzo = "03",
    abr = "04", abril = "04", may = "05", mayo = "05", jun = "06", junio = "06",
    jul = "07", julio = "07", ago = "08", agosto = "08", sep = "09", sept = "09",
    septiembre = "09", oct = "10", octubre = "10", nov = "11", noviembre = "11",
    dic = "12", diciembre = "12"
  )
  for (i in which(is.na(out))) {
    parts <- unlist(strsplit(z[i], " "))
    parts <- parts[nzchar(parts)]
    year_part <- parts[grepl("^[0-9]{2,4}$", parts)]
    month_part <- parts[parts %in% names(months_es)]
    if (length(year_part) && length(month_part)) {
      yy <- as.integer(year_part[length(year_part)])
      if (yy < 100) yy <- 2000 + yy
      out[i] <- as.Date(sprintf("%04d-%s-01", yy, months_es[[month_part[1]]]))
    } else {
      out[i] <- suppressWarnings(lubridate::floor_date(lubridate::ymd(raw[i]), "month"))
    }
  }
  lubridate::floor_date(out, "month")
}

download_ine_excel <- function(url, destination, label = "INE") {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  response <- httr::RETRY(
    "GET", url, times = 4, pause_base = 1, pause_cap = 8,
    terminate_on = c(400, 401, 403, 404),
    httr::timeout(120), httr::user_agent("Economics-IMACEC/3.0")
  )
  httr::stop_for_status(response)
  content <- httr::content(response, as = "raw")
  if (length(content) < 5000L) stop("El archivo ", label, " descargado no parece un Excel válido.")

  is_xlsx <- length(content) >= 2L && rawToChar(content[1:2]) == "PK"
  ole_signature <- as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1))
  is_xls <- length(content) >= 8L && identical(content[1:8], ole_signature)
  if (!is_xlsx && !is_xls) stop("La descarga ", label, " no devolvió un Excel .xls o .xlsx válido.")

  extension <- if (is_xlsx) ".xlsx" else ".xls"
  stem <- sub("[.](xlsx|xls)$", "", destination, ignore.case = TRUE)
  resolved_destination <- paste0(stem, extension)
  writeBin(content, resolved_destination)
  resolved_destination
}

extract_excel_urls <- function(page) {
  page <- gsub("\\\\/", "/", page)
  page <- gsub("&amp;", "&", page, fixed = TRUE)
  page <- gsub("\\\\u0026", "&", page, fixed = TRUE)
  pattern <- paste0(
    "(https?:)?//[^\\\"'<>[:space:]]+\\.(xlsx|xls)(\\?[^\\\"'<>[:space:]]*)?|",
    "/[^\\\"'<>[:space:]]+\\.(xlsx|xls)(\\?[^\\\"'<>[:space:]]*)?"
  )
  hits <- regmatches(page, gregexpr(pattern, page, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  hits <- unique(URLdecode(hits))
  hits <- gsub("^//", "https://", hits)
  relative <- startsWith(hits, "/")
  hits[relative] <- paste0("https://www.ine.gob.cl", hits[relative])
  hits
}

discover_ine_excel_urls <- function(page_url, include_patterns, fallback_url, override = "") {
  if (nzchar(override)) return(unique(c(override, fallback_url)))

  links <- tryCatch({
    response <- httr::RETRY(
      "GET", page_url, times = 3, pause_base = 1,
      httr::timeout(60), httr::user_agent("Economics-IMACEC/3.0")
    )
    httr::stop_for_status(response)
    page <- httr::content(response, as = "text", encoding = "UTF-8")
    extract_excel_urls(page)
  }, error = function(e) {
    warning("No se pudo descubrir Excel desde ", page_url, ": ", conditionMessage(e), call. = FALSE)
    character()
  })

  if (length(links)) {
    normalized <- normalize_ivs_text(URLdecode(links))
    keep <- vapply(seq_along(links), function(i) {
      all(vapply(include_patterns, function(p) grepl(p, normalized[i], perl = TRUE), logical(1)))
    }, logical(1))
    selected <- links[keep]
    if (length(selected)) {
      score <- 10L * grepl("2018", normalized[keep]) +
        6L * grepl("serie", normalized[keep]) +
        3L * grepl("xlsx", tolower(selected), fixed = TRUE)
      links <- selected[order(score, decreasing = TRUE)]
    }
  }

  unique(c(links, fallback_url))
}

resolve_ine_product_file <- function(urls, destination, label) {
  # Siempre intenta primero la publicación remota: un archivo cacheado del mes
  # anterior no debe impedir que M8P detecte el nuevo corte.
  for (url in urls) {
    downloaded <- tryCatch(
      download_ine_excel(url, destination, label),
      error = function(e) {
        warning("Falló descarga ", label, " desde ", url, ": ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(downloaded)) return(downloaded)
  }

  candidates <- unique(c(
    destination,
    sub("[.]xls$", ".xlsx", destination, ignore.case = TRUE),
    sub("[.]xlsx$", ".xls", destination, ignore.case = TRUE)
  ))
  local <- candidates[file.exists(candidates)]
  if (length(local)) {
    warning("Se usa copia local de respaldo para ", label, ": ", local[1], call. = FALSE)
    return(local[1])
  }
  stop("No fue posible obtener el Excel oficial ", label, " ni existe copia local.")
}

read_index_series_from_excel <- function(path, target_patterns, label) {
  sheets <- readxl::excel_sheets(path)
  candidates <- list()

  for (sheet in sheets) {
    raw <- tryCatch(
      suppressMessages(readxl::read_excel(
        path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"
      )),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) < 20 || ncol(raw) < 2) next

    top_n <- min(16L, nrow(raw))
    sheet_text <- normalize_ivs_text(paste(c(sheet, unlist(raw[seq_len(top_n), , drop = FALSE])), collapse = " "))
    target_hits <- sum(vapply(
      target_patterns, function(p) grepl(p, sheet_text, perl = TRUE), logical(1)
    ))
    if (target_hits == 0L) next

    periods_by_col <- lapply(raw, parse_ivs_period)
    period_counts <- vapply(periods_by_col, function(x) {
      sum(!is.na(x) & x >= as.Date("2000-01-01") & x <= (Sys.Date() %m+% lubridate::years(2)))
    }, integer(1))
    period_col <- which.max(period_counts)
    if (!length(period_col) || period_counts[period_col] < 24L) next

    periods <- periods_by_col[[period_col]]
    valid_period <- !is.na(periods) & periods >= as.Date("2000-01-01")
    headers <- vapply(seq_len(ncol(raw)), function(j) {
      normalize_ivs_text(paste(raw[[j]][seq_len(top_n)], collapse = " "))
    }, character(1))

    value_scores <- rep(-Inf, ncol(raw))
    value_counts <- integer(ncol(raw))
    for (j in seq_len(ncol(raw))) {
      if (j == period_col) next
      values <- parse_ivs_number(raw[[j]])
      value_counts[j] <- sum(valid_period & is.finite(values))
      if (value_counts[j] < 24L) next

      header <- headers[j]
      target_header_hits <- sum(vapply(
        target_patterns, function(p) grepl(p, header, perl = TRUE), logical(1)
      ))
      penalty <- 0
      if (grepl("variacion|acumulad|12 meses|porcent", header, perl = TRUE)) penalty <- penalty + 120
      if (grepl("desestacional|tendencia ciclo", header, perl = TRUE)) penalty <- penalty + 80
      adjacency <- if (j == period_col + 1L) 35 else max(0, 10 - 2 * abs(j - period_col))
      value_scores[j] <- 120 * target_header_hits + 20 * grepl("indice", header) +
        15 * target_hits + adjacency + min(value_counts[j], 120L) / 10 - penalty
    }

    value_col <- which.max(value_scores)
    if (!length(value_col) || !is.finite(value_scores[value_col])) next
    values <- parse_ivs_number(raw[[value_col]])

    out <- tibble::tibble(Periodo = periods, value = values) |>
      dplyr::filter(
        !is.na(Periodo), is.finite(value),
        Periodo >= as.Date("2017-01-01"),
        Periodo <= (Sys.Date() %m+% lubridate::years(1))
      ) |>
      dplyr::distinct(Periodo, .keep_all = TRUE) |>
      dplyr::arrange(Periodo)
    if (nrow(out) < 24L) next

    candidates[[length(candidates) + 1L]] <- list(
      data = out,
      score = value_scores[value_col] + as.numeric(max(out$Periodo)) / 1e5,
      sheet = sheet,
      period_col = period_col,
      value_col = value_col
    )
  }

  if (!length(candidates)) {
    stop("No se encontró de forma robusta la serie '", label, "' en ", basename(path), ".")
  }
  best <- candidates[[which.max(vapply(candidates, function(x) x$score, numeric(1)))]]
  message(
    "INE directo · ", label, " · hoja '", best$sheet,
    "' · última observación ", format(max(best$data$Periodo), "%Y-%m")
  )
  best$data
}

find_ivs_urls <- function() {
  discover_ine_excel_urls(
    page_url = ivs_page,
    include_patterns = c("ventas", "servicios", "serie"),
    fallback_url = official_ivs_url,
    override = ivs_url
  )
}

resolve_ivs_file <- function() {
  resolve_ine_product_file(find_ivs_urls(), ivs_path, "IVS")
}

read_ivs_official <- function(path = resolve_ivs_file()) {
  sheets <- readxl::excel_sheets(path)
  if (!"2" %in% sheets) stop("El Excel IVS no contiene la hoja oficial '2'.")
  raw <- suppressMessages(readxl::read_excel(path, sheet = "2", col_names = FALSE, .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 23) stop("La hoja '2' del IVS cambió de estructura.")

  headers <- normalize_ivs_text(vapply(c(2L, unname(ivs_columns)), function(j) raw[[j]][6], character(1)))
  expected <- c("mes", "transporte", "alojamiento", "informacion", "inmobiliarias", "profesionales", "administrativos")
  if (!all(vapply(seq_along(expected), function(i) grepl(expected[i], headers[i], fixed = TRUE), logical(1)))) {
    stop("Los encabezados de la hoja '2' no coinciden con las columnas oficiales B, C, G, K, O, S y W.")
  }

  rows <- 7:nrow(raw)
  out <- tibble::tibble(Periodo = parse_ivs_period(raw[[2]][rows]))
  for (name in names(ivs_columns)) out[[name]] <- parse_ivs_number(raw[[ivs_columns[[name]]]][rows])
  out |>
    dplyr::filter(!is.na(Periodo)) |>
    dplyr::distinct(Periodo, .keep_all = TRUE) |>
    dplyr::arrange(Periodo)
}

read_ivs_optional <- function() {
  tryCatch(
    read_ivs_official(),
    error = function(e) {
      warning(
        "El IVS oficial no está disponible; el ciclo EEE/M4 continuará y M8P quedará pendiente: ",
        conditionMessage(e), call. = FALSE
      )
      out <- tibble::tibble(Periodo = as.Date(character()))
      for (name in names(ivs_columns)) out[[name]] <- numeric()
      out
    }
  )
}

get_base_levels <- function() {
  series <- list(
    imacec_total_nivel = codes$imacec_total,
    imacec_no_minero_nivel = codes$imacec_no_minero,
    venta_minorista = codes$venta_minorista,
    credito_monto_nivel = codes$credito_monto,
    credito_cantidad_nivel = codes$credito_cantidad,
    avisos_laborales_nivel = codes$avisos_laborales,
    ipc_servicios_nivel = codes$ipc_servicios
  )
  purrr::imap(series, monthly_series) |>
    purrr::reduce(dplyr::full_join, by = "Periodo") |>
    dplyr::left_join(get_uf_monthly(), by = "Periodo") |>
    dplyr::arrange(Periodo)
}

get_ine_levels_direct <- function() {
  ipi_urls <- discover_ine_excel_urls(
    page_url = ipi_page,
    include_patterns = c("indice de produccion industrial|indice-de-produccion-industrial", "serie"),
    fallback_url = official_ipi_url,
    override = ipi_url_override
  )
  commerce_urls <- discover_ine_excel_urls(
    page_url = commerce_page,
    include_patterns = c("actividad mensual del comercio|actividad-mensual-del-comercio", "serie"),
    fallback_url = official_commerce_url,
    override = commerce_url_override
  )

  ipi_file <- resolve_ine_product_file(ipi_urls, ipi_path, "IPI")
  commerce_file <- resolve_ine_product_file(commerce_urls, commerce_path, "comercio")

  mineria <- read_index_series_from_excel(
    ipi_file, c("indice de produccion minera", "produccion minera", "ipmin"), "Índice de Producción Minera"
  ) |>
    dplyr::rename(mineria = value)
  manufactura <- read_index_series_from_excel(
    ipi_file, c("indice de produccion manufacturera", "produccion manufacturera", "ipman"),
    "Índice de Producción Manufacturera"
  ) |>
    dplyr::rename(manufactura = value)
  electricidad <- read_index_series_from_excel(
    ipi_file,
    c("indice de produccion de electricidad gas y agua", "electricidad gas y agua", "ipega"),
    "Índice de Producción de Electricidad, Gas y Agua"
  ) |>
    dplyr::rename(electricidad = value)
  comercio <- read_index_series_from_excel(
    commerce_file,
    c("indice de actividad del comercio al por menor", "actividad del comercio al por menor"),
    "Índice de Actividad del Comercio al por Menor"
  ) |>
    dplyr::rename(comercio = value)

  purrr::reduce(
    list(mineria, manufactura, comercio, electricidad),
    dplyr::full_join, by = "Periodo"
  ) |>
    dplyr::arrange(Periodo)
}

get_ine_levels_bde <- function() {
  purrr::imap(codes_ine, monthly_series) |>
    purrr::reduce(dplyr::full_join, by = "Periodo") |>
    dplyr::arrange(Periodo)
}

get_ine_levels <- function() {
  direct <- tryCatch(
    get_ine_levels_direct(),
    error = function(e) {
      warning("INE directo no disponible: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  bde <- tryCatch(
    get_ine_levels_bde(),
    error = function(e) {
      warning("Respaldo BDE no disponible para sectores INE: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (is.null(direct) && is.null(bde)) {
    stop("No existe ninguna fuente disponible para los indicadores sectoriales de M8P.")
  }
  if (is.null(direct)) return(bde)
  if (is.null(bde)) return(direct)

  joined <- dplyr::full_join(direct, bde, by = "Periodo", suffix = c("_ine", "_bde"))
  for (name in names(codes_ine)) {
    joined[[name]] <- dplyr::coalesce(joined[[paste0(name, "_ine")]], joined[[paste0(name, "_bde")]])
  }
  joined |>
    dplyr::select(Periodo, dplyr::all_of(names(codes_ine))) |>
    dplyr::arrange(Periodo)
}

apply_ine_yoy_fallback <- function(data, path = "data/raw/imacec_ine_yoy_fallback.csv", as_of = Sys.Date()) {
  data$ine_fallback <- ""
  if (!file.exists(path)) return(data)
  fallback <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(Periodo = as.Date(Periodo), publication_date = as.Date(publication_date)) |>
    dplyr::filter(publication_date <= as.Date(as_of))
  stopifnot(!anyDuplicated(fallback[c("Periodo", "variable")]),
    all(fallback$variable %in% names(codes_ine)), all(is.finite(fallback$value)))
  for (i in seq_len(nrow(fallback))) {
    variable <- fallback$variable[i]
    hit <- which(data$Periodo == fallback$Periodo[i] & is.na(data[[variable]]))
    if (length(hit)) {
      data[[variable]][hit] <- fallback$value[i]
      data$ine_fallback[hit] <- paste(data$ine_fallback[hit], variable)
    }
  }
  data$ine_fallback <- trimws(data$ine_fallback)
  data
}

build_imacec_dataset <- function() {
  ivs_names <- names(ivs_columns)
  data <- get_base_levels() |>
    dplyr::full_join(get_ine_levels(), by = "Periodo") |>
    dplyr::full_join(read_ivs_optional(), by = "Periodo") |>
    dplyr::left_join(read_calendar(), by = "Periodo") |>
    dplyr::arrange(Periodo) |>
    dplyr::mutate(
      imacec_total = yoy(imacec_total_nivel),
      imacec_no_minero = yoy(imacec_no_minero_nivel),
      monto_credito = yoy(credito_monto_nivel),
      cantidad_credito = yoy(credito_cantidad_nivel),
      monto_credito_real = yoy(credito_monto_nivel / uf_nivel),
      avisos_laborales = yoy(avisos_laborales_nivel),
      mineria = yoy(mineria),
      manufactura = yoy(manufactura),
      comercio = yoy(comercio),
      electricidad = yoy(electricidad)
    ) |>
    apply_ine_yoy_fallback(as_of = as.Date(last_date))

  real_names <- sub("_nivel$", "_real", ivs_names)
  for (i in seq_along(ivs_names)) data[[real_names[i]]] <- yoy(data[[ivs_names[i]]] / data$ipc_servicios_nivel)

  data |>
    dplyr::mutate(
      factor_ivs_real = ifelse(
        rowSums(!is.na(dplyr::pick(dplyr::all_of(real_names)))) == length(real_names),
        rowMeans(dplyr::pick(dplyr::all_of(real_names)), na.rm = TRUE),
        NA_real_
      ),
      imacec_total_lag1 = dplyr::lag(imacec_total),
      imacec_no_minero_lag1 = dplyr::lag(imacec_no_minero),
      avisos_laborales_lag1 = dplyr::lag(avisos_laborales),
      mes_numero = lubridate::month(Periodo),
      mes_factor = factor(mes_numero, levels = 1:12, labels = c(
        "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
      )),
      efecto_bisiesto_yoy = as.integer(mes_numero == 2 & lubridate::leap_year(Periodo)) -
        as.integer(mes_numero == 2 & lubridate::leap_year(Periodo %m-% lubridate::years(1))),
      dummy_covid = as.integer(Periodo >= as.Date("2020-03-01") & Periodo <= as.Date("2021-12-01"))
    ) |>
    dplyr::filter(Periodo >= model_start_date)
}
