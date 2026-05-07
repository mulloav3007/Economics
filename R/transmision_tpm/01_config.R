# ============================================================
# Configuración general: transmisión de TPM
# ============================================================

cfg <- list(
  first_date = "2002-01-01",
  last_date = as.character(Sys.Date()),
  dlm_lags = 6,
  lp_hmax = 12,
  raw_dir = "data/raw/transmision_tpm",
  processed_dir = "data/processed/transmision_tpm",
  metadata_dir = "data/metadata/transmision_tpm",
  figures_dir = "assets/img/transmision_tpm",
  tables_dir = "outputs/tables/transmision_tpm",
  model_dir = "outputs/model_objects/transmision_tpm"
)

ensure_dirs <- function(cfg) {
  dirs <- unlist(cfg[c("raw_dir", "processed_dir", "metadata_dir", "figures_dir", "tables_dir", "model_dir")])
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

ensure_dirs(cfg)

bcch_user <- Sys.getenv("BCCH_USER")
bcch_pass <- Sys.getenv("BCCH_PASS")

if (identical(bcch_user, "") || identical(bcch_pass, "")) {
  warning(
    "No se encontraron BCCH_USER/BCCH_PASS en .Renviron. ",
    "El pipeline de descarga no funcionará hasta configurar credenciales BDE."
  )
}

product_labels <- c(
  consumo_total = "Consumo total",
  consumo_cuotas = "Consumo: cuotas",
  consumo_tarj_rot = "Tarjeta: rotativo",
  consumo_tarj_cuota = "Tarjeta: cuota",
  comercial_total = "Comercial total",
  comercial_cuotas = "Comercial: cuotas",
  comercial_sobregiro = "Comercial: sobregiro",
  vivienda_uf = "Vivienda UF >3 años",
  cap_30_89 = "Captación 30-89d",
  cap_90_1y = "Captación 90d-1a"
)
