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

# Se usan SIEMPRE los dos últimos valores ajustados en una misma corrida X-13.
# La tasa resultante se aplica al último nivel SA oficial conocido.
seasonal_one_step <- function(levels, raw_name, sa_name, period, yoy_forecast) {
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
  fit <- seasonal::seas(x, regression.aictest = NULL)
  adjusted <- as.numeric(seasonal::final(fit))
  n <- length(adjusted)
  if (n != length(x) || any(!is.finite(adjusted[(n-1L):n])) ||
      adjusted[n-1L] <= 0 || adjusted[n] <= 0) {
    stop("X-13 no produjo dos niveles SA finales válidos.")
  }
  growth <- 100 * (adjusted[n] / adjusted[n - 1L] - 1)
  list(growth = growth, level_original = level_forecast,
       level_sa = sa_prev * adjusted[n] / adjusted[n - 1L])
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
      "Residuo aproximado del total y no minero; mayor incertidumbre."
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
        seasonal_one_step(levels, raw_name, sa_name, target, aa),
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
