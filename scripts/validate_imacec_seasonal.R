# Contraste retrospectivo del ajuste directo vs. las tasas SA oficiales BCCh.
# Usa tasas a/a realizadas para aislar el error del ajuste; NO es OOS del nowcast.
source("R/imacec_run_all.R", encoding = "UTF-8")
levels <- fetch_imacec_seasonal_levels()
dates <- levels$Periodo[is.finite(levels$total_original) & is.finite(levels$total_sa) &
                         is.finite(levels$no_minero_original) & is.finite(levels$no_minero_sa)]
periods <- utils::tail(dates, 24L)
if (length(periods) < 24L) stop("Muestra oficial insuficiente para contraste SA.")
table <- readr::read_csv("data/processed/imacec_seasonal_table.csv", show_col_types = FALSE)
required <- table[table$target_key %in% c("total", "no_minero", "minero"), ]
if (nrow(required) != 3L || !all(is.finite(required$original_aa)) ||
    !all(is.finite(required$desestacionalizado_mm))) {
  stop("La tabla de producción debe contener tasas válidas para las tres series.")
}
rows <- list()
for (i in seq_along(periods)) {
  date <- as.Date(periods[i])
  previous <- date %m-% lubridate::period(months = 1)
  lag12 <- date %m-% lubridate::period(months = 12)
  prior <- levels[levels$Periodo <= previous, , drop = FALSE]
  realized <- list()
  for (key in c("total", "no_minero", "minero")) {
    original <- paste0(key, "_original")
    sa <- paste0(key, "_sa")
    current <- seasonal_level_at(levels, original, date)
    past <- seasonal_level_at(levels, original, lag12)
    sa_now <- seasonal_level_at(levels, sa, date)
    sa_prev <- seasonal_level_at(levels, sa, previous)
    if (!all(is.finite(c(current, past, sa_now, sa_prev)))) next
    growth <- 100 * (current / past - 1)
    realized[[key]] <- growth
    estimated <- seasonal_one_step(prior, original, sa, date, growth)$growth
    official <- 100 * (sa_now / sa_prev - 1)
    lag_sa <- seasonal_level_at(levels, sa, lag12)
    lag12_est <- 100 * ((current / past) * lag_sa / sa_prev - 1)
    prior_factors <- vapply(1:3, function(k) {
      month <- date %m-% lubridate::period(months = 12 * k)
      raw <- seasonal_level_at(levels, original, month)
      adjusted <- seasonal_level_at(levels, sa, month)
      if (is.finite(raw) && is.finite(adjusted) && adjusted > 0) raw / adjusted else NA_real_
    }, numeric(1))
    factor3 <- if (all(is.finite(prior_factors))) {
      stats::weighted.mean(prior_factors, w = c(.55, .30, .15))
    } else {
      NA_real_
    }
    factor3_est <- if (is.finite(factor3)) {
      100 * ((current / factor3) / sa_prev - 1)
    } else {
      NA_real_
    }
    candidates <- c(x13_directo = estimated, factor_12m = lag12_est,
                    factor_3anios = factor3_est)
    if (key %in% c("total", "no_minero")) {
      candidates <- c(
        candidates,
        x13_chile_td = seasonal_one_step(
          prior, original, sa, date, growth, calendar_method = "chile_td"
        )$growth,
        x13_chile_completo = seasonal_one_step(
          prior, original, sa, date, growth, calendar_method = "chile"
        )$growth
      )
    }
    for (method in names(candidates)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        Periodo = date, serie = key, metodo = method,
        estimado_mm = candidates[[method]],
        oficial_mm = official, error_pp = candidates[[method]] - official
      )
    }
  }
  if (all(c("total", "no_minero", "minero") %in% names(realized))) {
    w <- seasonal_mining_weight(prior, previous)
    if (is.finite(w)) {
      implied <- (realized$total - (1 - w) * realized$no_minero) / w
      message("Minería implícita ", format(date, "%Y-%m"),
              " · error a/a: ", round(implied - realized$minero, 2), " pp")
    }
  }
}
comparison <- dplyr::bind_rows(rows)
summary <- comparison |>
  dplyr::group_by(serie, metodo) |>
  dplyr::summarise(
    N = dplyr::n(), MAE = mean(abs(error_pp)),
    RMSE = sqrt(mean(error_pp^2)),
    sesgo = mean(error_pp), .groups = "drop"
  )
print(summary, n = Inf)
if (!all(c("total", "no_minero") %in% summary$serie) ||
    any(!is.finite(summary$MAE))) {
  stop("Validación insuficiente del ajuste estacional.")
}
for (key in c("total", "no_minero")) {
  candidate <- summary |>
    dplyr::filter(serie == key, metodo %in% c("x13_directo", "x13_chile_td", "x13_chile_completo")) |>
    dplyr::arrange(MAE)
  print(candidate, n = Inf)
  message("Mejor método ex post para ", key, ": ", candidate$metodo[1],
          " · MAE=", round(candidate$MAE[1], 3), " pp")
}
message("Contraste BCCh completado: 24 meses, tasas a/a efectivas (NO backtest del nowcast ni vintages oficiales en tiempo real).")
