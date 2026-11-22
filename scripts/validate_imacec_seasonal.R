# Contraste retrospectivo del ajuste directo vs. las tasas SA oficiales BCCh.
# Usa tasas a/a realizadas para aislar el error del ajuste; NO es OOS del nowcast.
source("R/imacec_run_all.R", encoding = "UTF-8")
levels <- fetch_imacec_seasonal_levels()
dates <- levels$Periodo[is.finite(levels$total_original) & is.finite(levels$total_sa) &
                         is.finite(levels$no_minero_original) & is.finite(levels$no_minero_sa)]
periods <- utils::tail(dates, 6L)
if (length(periods) < 6L) stop("Muestra oficial insuficiente para contraste SA.")
rows <- list()
for (date in periods) {
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
    rows[[length(rows) + 1L]] <- tibble::tibble(
      Periodo = date, serie = key, estimado_mm = estimated,
      oficial_mm = official, error_pp = estimated - official
    )
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
  dplyr::group_by(serie) |>
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
message("Contraste BCCh completado: evaluación ex post de X-13 directo, no OOS del nowcast.")
