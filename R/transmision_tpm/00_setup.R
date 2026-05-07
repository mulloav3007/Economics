# ============================================================
# Paquetes
# ============================================================

required_packages <- c(
  "httr2", "jsonlite", "dplyr", "tidyr", "purrr", "readr",
  "lubridate", "stringr", "zoo", "ggplot2", "scales",
  "broom", "lmtest", "sandwich", "knitr", "rlang", "plotly"
)

install_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Instalando paquetes faltantes: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_missing(required_packages)

invisible(lapply(required_packages, library, character.only = TRUE))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
