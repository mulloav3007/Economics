# ============================================================
# Diccionario de series recomendadas
# ============================================================

series_dictionary <- tibble::tribble(
  ~name,                 ~series_id,                      ~frequency,  ~block,      ~product_group, ~required, ~description,
  "tpm",                "F022.TPM.TIN.D001.NO.Z.D",       "DAILY",     "policy",   "policy",       TRUE,      "Tasa de Política Monetaria diaria",

  # Colocaciones: productos bancarios
  "consumo_total",      "F022.CON.TIP.Z.NO.Z.M",          "MONTHLY",   "lending",  "consumo",      TRUE,      "Tasa promedio colocación consumo total",
  "consumo_cuotas",     "F022.CONCUOT.TIP.Z.NO.Z.M",      "MONTHLY",   "lending",  "consumo",      FALSE,     "Tasa promedio créditos de consumo en cuotas",
  "consumo_tarj_rot",   "F022.CONTARJR.TIP.Z.NO.Z.M",     "MONTHLY",   "lending",  "consumo",      FALSE,     "Tasa tarjeta de crédito rotativo",
  "consumo_tarj_cuota", "F022.CONTARJC.TIP.Z.NO.Z.M",     "MONTHLY",   "lending",  "consumo",      FALSE,     "Tasa tarjeta de crédito en cuotas",

  "comercial_total",    "F022.COM.TIP.Z.NO.Z.M",          "MONTHLY",   "lending",  "comercial",    TRUE,      "Tasa promedio créditos comerciales total",
  "comercial_cuotas",   "F022.COMCUOT.TIP.Z.NO.Z.M",      "MONTHLY",   "lending",  "comercial",    FALSE,     "Tasa créditos comerciales en cuotas",
  "comercial_sobregiro","F022.COMSOBR.TIP.Z.NO.Z.M",      "MONTHLY",   "lending",  "comercial",    FALSE,     "Tasa créditos comerciales sobregiros",

  "vivienda_uf",        "F022.VIV.TIP.MA03.UF.Z.M",       "MONTHLY",   "lending",  "vivienda",     TRUE,      "Tasa vivienda UF mayor a 3 años",

  # Captaciones: costo de fondeo bancario
  "cap_30_89",          "F022.CAP.TIP.D089.NO.Z.D",       "DAILY",     "funding",  "captacion",    FALSE,     "Tasa captación pesos 30-89 días",
  "cap_90_1y",          "F022.CAP.TIP.AN01.NO.Z.D",       "DAILY",     "funding",  "captacion",    TRUE,      "Tasa captación pesos 90 días a 1 año",

  # Curva de mercado: NO se usa en el modelo principal, solo en robustez.
  "bcp_2y",             "F022.BCLP.TIS.AN02.NO.Z.D",      "DAILY",     "market",   "curva",        FALSE,     "BCP 2 años",
  "bcp_5y",             "F022.BCLP.TIS.AN05.NO.Z.D",      "DAILY",     "market",   "curva",        FALSE,     "BCP 5 años",
  "bcp_10y",            "F022.BCLP.TIS.AN10.NO.Z.D",      "DAILY",     "market",   "curva",        FALSE,     "BCP 10 años",
  "bcu_5y",             "F022.BUF.TIS.AN05.UF.Z.D",       "DAILY",     "market",   "curva_real",   FALSE,     "BCU 5 años",
  "bcu_10y",            "F022.BUF.TIS.AN10.UF.Z.D",       "DAILY",     "market",   "curva_real",   FALSE,     "BCU 10 años",

  # Controles macro mínimos.
  # Si algún código falla, el pipeline sigue porque required = FALSE.
  "ipc",                "G073.IPC.IND.2023.M",            "MONTHLY",   "macro",    "precios",      FALSE,     "IPC nivel, base 2023=100",
  "tc_obs",             "F073.TCO.PRE.Z.M",               "MONTHLY",   "macro",    "tipo_cambio",  FALSE,     "Tipo de cambio dólar observado, promedio mensual",
  "imacec",             "F032.IMC.IND.Z.Z.EP18.Z.Z.0.M",  "MONTHLY",   "macro",    "actividad",    FALSE,     "IMACEC índice mensual, candidato; verificar en BDE si falla"
)

lending_products <- series_dictionary |>
  dplyr::filter(block %in% c("lending", "funding")) |>
  dplyr::pull(name)

market_controls <- c("bcp_2y", "bcp_5y", "bcp_10y", "bcu_5y", "bcu_10y")
macro_raw_series <- c("ipc", "tc_obs", "imacec")
macro_controls <- c("infl_yoy", "dlog_tc_12", "imacec_yoy")
