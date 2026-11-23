# Ajuste estacional del nowcast. No cambia las especificaciones M4/M8P.
# X-13 sobre los niveles originales extendidos con un pronóstico de un mes.
# No reproduce la agregación indirecta utilizada por el BCCh.

seasonal_codes <- list(
  total_original = codes$imacec_total,
  no_minero_original = codes$imacec_no_minero,
  minero_original = "F032.IMC.IND.Z.Z.EP18.03.Z.0.M",
  total_sa = "F032.IMC.IND.Z.Z.EP18.Z.Z.1.M",
  no_minero_sa = "F032.IMC.IND.Z.Z.EP18.N03.Z.1.M",
  minero_sa = "F032.IMC.IND.Z.Z.EP18.03.Z.1.M"
)

seasonal_level_at <- function(levels, variable, period) {
  if (!variable %in% names(levels)) return(NA_real_)
  hit <- levels[[variable]][levels$Periodo == as.Date(period)]
  if (length(hit) != 1L || !is.finite(hit)) return(NA_real_)
  as.numeric(hit)
}

fetch_imacec_seasonal_levels <- function(start = "2012-01-01") {
  pieces <- purrr::imap(seasonal_codes, function(code, label) {
    raw <- tryCatch(
      fetch_series(code, firstdate = start, lastdate = last_date),
      error = function(e) {
        warning("Serie de validación estacional ", label, " no disponible: ",
                conditionMessage(e), call. = FALSE)
        tibble::tibble(date = as.Date(character()), value = numeric())
      }
    )
    if (!nrow(raw)) {
      out <- tibble::tibble(Periodo = as.Date(character()))
      out[[label]] <- numeric()
      return(out)
    }
    raw |>
      dplyr::mutate(Periodo = lubridate::floor_date(date, "month")) |>
      dplyr::group_by(Periodo) |>
      dplyr::summarise(value = dplyr::last(value), .groups = "drop") |>
      dplyr::rename(!!label := value)
  })
  purrr::reduce(pieces, dplyr::full_join, by = "Periodo") |>
    dplyr::arrange(Periodo)
}

# Regresores de calendario nacional para X-13, sin introducir información
# económica del futuro. Se extraen del calendario mensual ya versionado
# (lun-vie menos feriados lun_f-vie_f), no del calendario estadounidense.
chile_easter_date <- function(year) {
  year <- as.integer(year)
  a <- year %% 19L
  b <- year %/% 100L
  c <- year %% 100L
  d <- b %/% 4L
  e <- b %% 4L
  f <- (b + 8L) %/% 25L
  g <- (b - f + 1L) %/% 3L
  h <- (19L * a + b - d - g + 15L) %% 30L
  i <- c %/% 4L
  k <- c %% 4L
  l <- (32L + 2L * e + 2L * i - h - k) %% 7L
  m <- (a + 11L * h + 22L * l) %/% 451L
  month <- (h + l - 7L * m + 114L) %/% 31L
  day <- (h + l - 7L * m + 114L) %% 31L + 1L
  as.Date(sprintf("%04d-%02d-%02d", year, month, day))
}

read_chile_x13_calendar <- function(path = cal_path) {
  if (!file.exists(path)) stop("No se encuentra el calendario chileno: ", path)
  raw <- readxl::read_excel(path)
  if (!"mes" %in% names(raw)) names(raw)[1] <- "mes"
  needed <- c("lun", "mar", "mie", "jue", "vie",
              "lun_f", "mar_f", "mie_f", "jue_f", "vie_f")
  if (!all(needed %in% names(raw))) {
    stop("Calendario de Chile sin columnas requeridas: ",
         paste(setdiff(needed, names(raw)), collapse = ", "))
  }
  if (inherits(raw$mes, c("Date", "POSIXct", "POSIXt"))) {
    dates <- as.Date(raw$mes)
  } else {
    dates <- suppressWarnings(lubridate::ymd(raw$mes))
    if (all(is.na(dates))) dates <- suppressWarnings(lubridate::dmy(raw$mes))
    if (all(is.na(dates)) && is.numeric(raw$mes)) {
      dates <- as.Date(raw$mes, origin = "1899-12-30")
    }
  }
  if (anyNA(dates)) stop("El calendario chileno tiene meses inválidos.")
  periods <- as.Date(lubridate::floor_date(dates, "month"))
  values <- lapply(raw[needed], as.numeric)
  if (any(!is.finite(unlist(values)))) {
    stop("El calendario chileno tiene conteos de días/feriados inválidos.")
  }
  workdays <- rowSums(as.data.frame(values[1:5])) -
    rowSums(as.data.frame(values[6:10]))
  if (any(workdays < 15 | workdays > 24) || anyDuplicated(periods)) {
    stop("El calendario chileno contiene meses duplicados o días laborales imposibles.")
  }
  # Proporción de ocho días anteriores al Domingo de Resurrección que cae
  # dentro de cada mes: capta su desplazamiento entre marzo y abril.
  years <- unique(lubridate::year(periods))
  easter <- vapply(years, function(y) as.character(chile_easter_date(y)), character(1))
  easter <- as.Date(easter)
  easter_window <- lapply(easter, function(d) seq(d - 8L, d - 1L, by = "day"))
  march_april <- vapply(periods, function(p) {
    year_pos <- match(lubridate::year(p), years)
    dates8 <- easter_window[[year_pos]]
    mean(lubridate::floor_date(dates8, "month") == p)
  }, numeric(1))
  cal <- tibble::tibble(Periodo = periods, dias_laborales = workdays,
                        semana_santa = march_april) |>
    dplyr::arrange(Periodo) |>
    dplyr::mutate(
      mes = lubridate::month(Periodo)
    ) |>
    dplyr::group_by(mes) |>
    dplyr::mutate(
      td_chile = dias_laborales - mean(dias_laborales),
      pascua_chile = semana_santa - mean(semana_santa)
    ) |>
    dplyr::ungroup()
  cal
}

chile_x13_regressors <- function(periods, method = c("chile", "chile_td"),
                                path = cal_path) {
  method <- match.arg(method)
  periods <- as.Date(periods)
  if (!length(periods) || anyNA(periods) || anyDuplicated(periods)) {
    stop("Los períodos requeridos para X-13 no son válidos.")
  }
  expected <- seq(periods[1], by = "month", length.out = length(periods))
  if (!identical(periods, expected)) {
    stop("Los períodos del X-13 deben ser mensuales, continuos y ordenados.")
  }
  # X-13 extrapola su regARIMA hasta 36 meses: los xreg deben cubrir también
  # ese horizonte. El calendario futuro es determinístico, no una proyección
  # económica; se conoce en el archivo nacional hasta diciembre de 2030.
  extended <- seq(periods[1], by = "month", length.out = length(periods) + 36L)
  cal <- read_chile_x13_calendar(path)
  matched <- match(extended, cal$Periodo)
  if (anyNA(matched)) {
    stop("Calendario chileno sin meses requeridos: ",
         paste(format(extended[is.na(matched)], "%Y-%m"), collapse = ", "))
  }
  selection <- if (method == "chile_td") "td_chile" else c("td_chile", "pascua_chile")
  mat <- as.matrix(cal[matched, selection, drop = FALSE])
  xreg <- stats::ts(mat,
     start = c(lubridate::year(periods[1]), lubridate::month(periods[1])),
     frequency = 12)
  xreg
}

# Se usan SIEMPRE los dos últimos valores ajustados en una misma corrida X-13.
# La tasa resultante se aplica al último nivel SA oficial conocido.
seasonal_one_step <- function(levels, raw_name, sa_name, period, yoy_forecast,
                              calendar_method = c("none", "chile_td", "chile")) {
  calendar_method <- match.arg(calendar_method)
  period <- as.Date(period)
  previous <- period %m-% lubridate::period(months = 1)
  lag12 <- period %m-% lubridate::period(months = 12)
  hist <- levels |>
    dplyr::filter(Periodo <= previous, !is.na(.data[[raw_name]])) |>
    dplyr::select(Periodo, value = dplyr::all_of(raw_name)) |>
    dplyr::arrange(Periodo)
  expected <- seq(min(hist$Periodo), previous, by = "month")
  if (nrow(hist) < 84L || !identical(as.Date(hist$Periodo), as.Date(expected))) {
    stop("Historia mensual insuficiente o discontinua para ", raw_name)
  }
  lag_level <- seasonal_level_at(levels, raw_name, lag12)
  sa_prev <- seasonal_level_at(levels, sa_name, previous)
  if (!is.finite(lag_level) || lag_level <= 0 ||
      !is.finite(sa_prev) || sa_prev <= 0 ||
      !is.finite(yoy_forecast) || yoy_forecast <= -100) {
    stop("Niveles originales/desestacionalizados incompletos para ", raw_name)
  }
  level_forecast <- lag_level * (1 + yoy_forecast / 100)
  x <- stats::ts(
    c(hist$value, level_forecast),
    start = c(lubridate::year(hist$Periodo[1]), lubridate::month(hist$Periodo[1])),
    frequency = 12
  )
  # Evitar el test automático de feriados estadounidenses; la aproximación
  # directa no reproduce la parametrización nacional del Banco Central.
  fit <- if (calendar_method == "none") {
    seasonal::seas(x, regression.aictest = NULL)
  } else {
    periods <- c(hist$Periodo, period)
    xreg <- chile_x13_regressors(periods,
      method = if (calendar_method == "chile_td") "chile_td" else "chile")
    # usertype=td/holiday elimina los efectos calendario de la serie final.
    # lpyear corrige febrero bisiesto; no se utilizan feriados de EE.UU.
    types <- if (calendar_method == "chile_td") "td" else c("td", "holiday")
    seasonal::seas(
      x, xreg = xreg, regression.usertype = types,
      regression.variables = "lpyear", regression.aictest = NULL
    )
  }
  adjusted <- as.numeric(seasonal::final(fit))
  n <- length(adjusted)
  if (n != length(x) || any(!is.finite(adjusted[(n-1L):n])) ||
      adjusted[n-1L] <= 0 || adjusted[n] <= 0) {
    stop("X-13 sin niveles finales: n_ajustado=", n,
         ", n_original=", length(x),
         ", últimos=", paste(utils::tail(adjusted, 3L), collapse = ","))
  }
  growth <- 100 * (adjusted[n] / adjusted[n - 1L] - 1)
  list(growth = growth, level_original = level_forecast,
       level_sa = sa_prev * adjusted[n] / adjusted[n - 1L])
}

# Para minería el factor oficial de t-12 reprodujo mejor la tasa SA que
# X-13 directo en el contraste retrospectivo. No supone identidad contable.
seasonal_mining_one_step <- function(levels, period, yoy_forecast) {
  period <- as.Date(period)
  previous <- period %m-% lubridate::period(months = 1)
  lag12 <- period %m-% lubridate::period(months = 12)
  sa_lag12 <- seasonal_level_at(levels, "minero_sa", lag12)
  sa_prev <- seasonal_level_at(levels, "minero_sa", previous)
  if (!is.finite(sa_lag12) || sa_lag12 <= 0 ||
      !is.finite(sa_prev) || sa_prev <= 0 ||
      !is.finite(yoy_forecast) || yoy_forecast <= -100) {
    stop("Niveles SA oficiales insuficientes para minería implícita.")
  }
  sa_hat <- sa_lag12 * (1 + yoy_forecast / 100)
  list(growth = 100 * (sa_hat / sa_prev - 1), level_sa = sa_hat)
}

# Proyección minera implícita; mínimo cuadrado de la identidad aproximada
# g_total - g_no_minero = w*(g_minero - g_no_minero).
# Sin ponderaciones manuales ni información posterior al mes objetivo.
seasonal_mining_weight <- function(levels, previous, window = 60L) {
  hist <- levels |>
    dplyr::filter(Periodo <= previous) |>
    dplyr::arrange(Periodo) |>
    dplyr::mutate(
      total = 100 * (total_original / dplyr::lag(total_original, 12L) - 1),
      nm = 100 * (no_minero_original / dplyr::lag(no_minero_original, 12L) - 1),
      mineria = 100 * (minero_original / dplyr::lag(minero_original, 12L) - 1)
    ) |>
    dplyr::filter(is.finite(total), is.finite(nm), is.finite(mineria)) |>
    dplyr::slice_tail(n = window)
  if (nrow(hist) < 36L) return(NA_real_)
  x <- hist$mineria - hist$nm
  y <- hist$total - hist$nm
  weight <- sum(x * y) / sum(x^2)
  if (!is.finite(weight) || weight <= 0.03 || weight >= 0.35) return(NA_real_)
  weight
}

seasonal_table_row <- function(period, key, label, tipo, model_key,
                               yoy = NA_real_, mom = NA_real_, note = "") {
  tibble::tibble(
    Periodo = as.Date(period), target_key = key, variable = label,
    tipo = tipo, model_key = model_key,
    original_aa = as.numeric(yoy), desestacionalizado_mm = as.numeric(mom),
    nota = note
  )
}

build_imacec_seasonal_table <- function(levels, projections, cycle) {
  target <- as.Date(cycle$periodo_objetivo[1])
  last_actual <- as.Date(cycle$ultima_observacion_imacec[1])
  model <- as.character(cycle$modelo_principal[1])
  observed <- identical(as.character(cycle$ciclo_estado[1]), "official_review") ||
    target <= last_actual
  keys <- c(total = "IMACEC",
            minero = if (observed) "IMACEC minero" else "Minería implícita",
            no_minero = "IMACEC no minero")
  previous <- target %m-% lubridate::period(months = 1)
  selected <- projections |>
    dplyr::filter(Periodo == target, model_key == model)
  output <- list()
  forecast <- list()
  if (!observed) {
    for (key in c("total", "no_minero")) {
      hit <- selected$forecast[selected$target_key == key]
      forecast[[key]] <- if (length(hit) == 1L && is.finite(hit)) as.numeric(hit) else NA_real_
    }
    weight <- seasonal_mining_weight(levels, last_actual)
    forecast$minero <- if (is.finite(weight) &&
                           all(is.finite(unlist(forecast[c("total", "no_minero")])))) {
      (forecast$total - (1 - weight) * forecast$no_minero) / weight
    } else {
      NA_real_
    }
    if (is.finite(forecast$minero) && abs(forecast$minero) > 60) forecast$minero <- NA_real_
  }

  for (key in names(keys)) {
    raw_name <- paste0(key, "_original")
    sa_name <- paste0(key, "_sa")
    note <- if (key == "minero") {
      "Residuo aproximado total/no minero; m/m con factor estacional oficial t-12; mayor incertidumbre."
    } else {
      "X-13 directo sobre índice proyectado; no equivale al ajuste indirecto BCCh."
    }
    if (observed) {
      lag12 <- target %m-% lubridate::period(months = 12)
      aa <- 100 * (seasonal_level_at(levels, raw_name, target) /
                     seasonal_level_at(levels, raw_name, lag12) - 1)
      mm <- 100 * (seasonal_level_at(levels, sa_name, target) /
                     seasonal_level_at(levels, sa_name, previous) - 1)
      if (!is.finite(aa)) aa <- NA_real_
      if (!is.finite(mm)) mm <- NA_real_
      output[[key]] <- seasonal_table_row(
        target, key, keys[[key]], "Efectivo BCCh", "oficial", aa, mm, "Niveles oficiales BCCh."
      )
      next
    }
    aa <- forecast[[key]]
    mm <- NA_real_
    if (is.finite(aa) && identical(previous, last_actual)) {
      prediction <- tryCatch(
        if (key == "minero") {
          seasonal_mining_one_step(levels, target, aa)
        } else {
          seasonal_one_step(levels, raw_name, sa_name, target, aa)
        },
        error = function(e) {
          warning("Ajuste estacional ", key, ": ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (!is.null(prediction)) mm <- prediction$growth
    } else if (!identical(previous, last_actual)) {
      note <- paste0(note, " Sin dato del mes anterior para calcular m/m.")
    }
    if (!is.finite(aa)) note <- paste0(note, " No hay pronóstico válido para este corte.")
    if (key == "minero" && !is.finite(aa)) {
      note <- "Minería implícita no identificable con la información disponible."
    }
    output[[key]] <- seasonal_table_row(
      target, key, keys[[key]], "Proyección", model, aa, mm, note
    )
  }
  dplyr::bind_rows(output)
}

export_imacec_seasonal_table <- function(projections, cycle,
                                         output_dir = "data/processed") {
  result <- tryCatch({
    levels <- fetch_imacec_seasonal_levels()
    build_imacec_seasonal_table(levels, projections, cycle)
  }, error = function(e) {
    warning("Tabla desestacionalizada no disponible: ", conditionMessage(e), call. = FALSE)
    purrr::imap_dfr(
      c(total = "IMACEC", minero = "Minería implícita", no_minero = "IMACEC no minero"),
      function(label, key) seasonal_table_row(
        as.Date(cycle$periodo_objetivo[1]), key, label, "No disponible",
        as.character(cycle$modelo_principal[1]),
        note = "Error de adquisición o cálculo: no se publica una cifra sin validar."
      )
    )
  })
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(result, file.path(output_dir, "imacec_seasonal_table.csv"))
  invisible(result)
}
