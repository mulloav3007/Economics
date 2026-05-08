# ============================================================
# 00_setup.R
# Proyecto: Índice de estrés financiero de mercado para Chile
# Autor: Mauricio Ulloa
# Objetivo: carga de paquetes, rutas y utilidades generales.
# ============================================================

estres_required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "zoo", "scales",
  "broom", "purrr", "stringr", "tibble", "rlang"
)

estres_load_packages <- function(packages = estres_required_packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "Faltan paquetes requeridos: ", paste(missing, collapse = ", "),
      "\nInstala con: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

estres_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    has_quarto <- file.exists(file.path(path, "_quarto.yml"))
    has_estres_setup <- file.exists(file.path(path, "R", "estres_financiero", "00_setup.R"))
    has_project_dirs <- dir.exists(file.path(path, "proyectos")) && dir.exists(file.path(path, "scripts"))

    if (has_estres_setup || (has_quarto && has_project_dirs)) return(path)

    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }

  stop(
    "No pude identificar la raíz del proyecto. ",
    "Ejecuta desde la carpeta del repositorio Economics o verifica que exista R/estres_financiero/00_setup.R.",
    call. = FALSE
  )
}

estres_path <- function(..., root = estres_project_root()) {
  file.path(root, ...)
}

estres_make_dirs <- function(root = estres_project_root()) {
  dirs <- c(
    "data/raw/estres_financiero",
    "data/processed/estres_financiero",
    "assets/img/estres_financiero",
    "outputs/tables/estres_financiero"
  )

  purrr::walk(file.path(root, dirs), dir.create, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}

estres_safe_log <- function(x) {
  dplyr::if_else(!is.na(x) & x > 0, log(x), NA_real_)
}

estres_zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sig <- stats::sd(x, na.rm = TRUE)

  if (is.na(sig) || sig == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sig
}

estres_interpolate_numeric <- function(x, date) {
  if (all(is.na(x))) return(x)

  zoo::na.approx(
    object = x,
    x = as.numeric(date),
    na.rm = FALSE
  )
}

estres_roll_mean <- function(x, width = 30, min_obs = 20) {
  zoo::rollapplyr(
    data = x,
    width = width,
    FUN = function(z) {
      if (sum(!is.na(z)) < min_obs) return(NA_real_)
      mean(z, na.rm = TRUE)
    },
    fill = NA_real_,
    partial = TRUE
  )
}

estres_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", colour = "#1f2a35", size = base_size + 4),
      plot.subtitle = ggplot2::element_text(colour = "#66717f", size = base_size),
      axis.title = ggplot2::element_text(colour = "#66717f"),
      axis.text = ggplot2::element_text(colour = "#4d5662"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e4dfd6", linewidth = 0.35),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "#4d5662"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}
