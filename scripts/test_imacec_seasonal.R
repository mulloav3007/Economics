# Pruebas sintéticas sin credenciales BCCh: identidad minera, corte y X-13.
source("R/imacec_run_all.R", encoding = "UTF-8")
dates <- seq(as.Date("2012-01-01"), by = "month", length.out = 180L)
t <- seq_along(dates)
set.seed(611)
nm_trend <- 93 + 0.12 * t
m_trend <- 102 + 0.05 * t
nm_raw <- nm_trend * (1 + 0.05 * sin(2 * pi * t / 12)) + stats::rnorm(length(t), 0, 0.6)
m_raw <- m_trend * (1 + 0.11 * cos(2 * pi * t / 12)) + stats::rnorm(length(t), 0, 1.0)
weight <- 0.12
levels <- tibble::tibble(
  Periodo = dates,
  total_original = (1 - weight) * nm_raw + weight * m_raw,
  no_minero_original = nm_raw, minero_original = m_raw,
  total_sa = (1 - weight) * nm_trend + weight * m_trend,
  no_minero_sa = nm_trend, minero_sa = m_trend
)
last_actual <- dates[length(dates) - 1L]
target <- tail(dates, 1)
yoy_at <- function(x) 100 * (x[length(x)] / x[length(x)-12L] - 1)
projected <- tibble::tibble(
  Periodo = target, target_key = c("total", "no_minero"),
  model_key = "m8p", forecast = c(yoy_at(levels$total_original), yoy_at(levels$no_minero_original))
)
cycle <- tibble::tibble(
  periodo_objetivo = target, ultima_observacion_imacec = last_actual,
  ciclo_estado = "ine", modelo_principal = "m8p"
)
w_hat <- seasonal_mining_weight(levels[levels$Periodo <= last_actual, ], last_actual)
stopifnot(is.finite(w_hat), w_hat > 0.03, w_hat < 0.35)
table <- build_imacec_seasonal_table(levels[levels$Periodo <= last_actual, ], projected, cycle)
stopifnot(identical(table$target_key, c("total", "minero", "no_minero")))
stopifnot(all(table$tipo == "Proyección"),
          all(is.finite(table$original_aa)),
          all(is.finite(table$desestacionalizado_mm)))
stopifnot(abs(table$original_aa[table$target_key == "minero"] -
              yoy_at(levels$minero_original)) < 3)
stopifnot(all(abs(table$desestacionalizado_mm) < 15))

closed <- tibble::tibble(
  periodo_objetivo = last_actual,
  ultima_observacion_imacec = last_actual,
  ciclo_estado = "official_review", modelo_principal = "summary"
)
observed <- build_imacec_seasonal_table(levels, projected, closed)
stopifnot(all(observed$tipo == "Efectivo BCCh"),
          all(is.finite(observed$desestacionalizado_mm)))

# No calcular m/m si falta el índice oficial del mes anterior.
gap <- cycle
gap$periodo_objetivo <- target %m+% lubridate::period(months = 1)
gap_proj <- projected
gap_proj$Periodo <- gap$periodo_objetivo
incomplete <- build_imacec_seasonal_table(levels[levels$Periodo <= last_actual, ], gap_proj, gap)
stopifnot(all(is.na(incomplete$desestacionalizado_mm)))
message("OK: X-13 sobre nivel proyectado, minería implícita y control de meses faltantes.")
