# ============================================================
# 06_update_estres_financiero.R
# Actualiza el proyecto de estrés financiero para Chile.
#
# Uso recomendado desde la raíz del repositorio:
#   source("scripts/06_update_estres_financiero.R")
#
# También funciona si se ejecuta desde otra subcarpeta del repo, siempre
# que exista R/estres_financiero/00_setup.R en la raíz.
# ============================================================

find_estres_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    setup_file <- file.path(path, "R", "estres_financiero", "00_setup.R")
    if (file.exists(setup_file)) return(path)

    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }

  stop(
    "No pude identificar la raíz del repositorio Economics. ",
    "Ejecuta este script desde la raíz o verifica que exista R/estres_financiero/00_setup.R.",
    call. = FALSE
  )
}

repo_root <- find_estres_project_root()

source(file.path(repo_root, "R", "estres_financiero", "00_setup.R"), encoding = "UTF-8")
estres_load_packages()

source(estres_path("R", "estres_financiero", "01_data.R", root = repo_root), encoding = "UTF-8")
source(estres_path("R", "estres_financiero", "02_models.R", root = repo_root), encoding = "UTF-8")
source(estres_path("R", "estres_financiero", "03_plots.R", root = repo_root), encoding = "UTF-8")
source(estres_path("R", "estres_financiero", "04_run_pipeline.R", root = repo_root), encoding = "UTF-8")

run_estres_financiero_pipeline(root = repo_root)
