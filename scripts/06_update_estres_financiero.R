# ============================================================
# 06_update_estres_financiero.R
# Actualiza el proyecto de estrés financiero para Chile.
#
# Uso desde la raíz del repositorio:
#   source("scripts/06_update_estres_financiero.R")
#
# Luego, para actualizar la web:
#   quarto render
# ============================================================

source("R/estres_financiero/00_setup.R", encoding = "UTF-8")
estres_load_packages()

source("R/estres_financiero/01_data.R", encoding = "UTF-8")
source("R/estres_financiero/02_models.R", encoding = "UTF-8")
source("R/estres_financiero/03_plots.R", encoding = "UTF-8")
source("R/estres_financiero/04_run_pipeline.R", encoding = "UTF-8")

run_estres_financiero_pipeline()
