# ============================================================
# Figuras
# ============================================================

theme_bcch_like <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

label_product <- function(x) {
  dplyr::recode(x, !!!product_labels, .default = x)
}

plot_rates_tpm <- function(monthly_panel) {
  keep <- intersect(c("tpm", "consumo_total", "comercial_total", "vivienda_uf", "cap_90_1y"), names(monthly_panel))

  monthly_panel |>
    dplyr::select(date, dplyr::all_of(keep)) |>
    tidyr::pivot_longer(-date, names_to = "serie", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      serie = dplyr::recode(
        serie,
        tpm = "TPM",
        consumo_total = "Consumo total",
        comercial_total = "Comercial total",
        vivienda_uf = "Vivienda UF",
        cap_90_1y = "Captación 90d-1a",
        .default = serie
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(date, value, group = serie, linetype = serie)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(
      title = "TPM y tasas bancarias seleccionadas",
      subtitle = "Tasas anuales, porcentaje",
      x = NULL,
      y = "Porcentaje"
    ) +
    theme_bcch_like()
}

plot_cumulative_pt <- function(pt_tbl) {
  pt_tbl |>
    dplyr::mutate(product_lab = label_product(product)) |>
    ggplot2::ggplot(ggplot2::aes(horizon, cumulative, group = product_lab, linetype = product_lab)) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(
      title = "Pass-through acumulado de la TPM",
      subtitle = "Coeficiente acumulado de rezagos distribuidos. Línea horizontal = traspaso uno a uno.",
      x = "Meses desde el cambio de TPM",
      y = "Pass-through acumulado"
    ) +
    theme_bcch_like()
}

plot_asymmetric_pt <- function(pt_asym_tbl) {
  pt_asym_tbl |>
    dplyr::mutate(
      product_lab = label_product(product),
      type_lab = dplyr::recode(type, alza_tpm = "Alzas de TPM", baja_tpm = "Bajas de TPM", .default = type)
    ) |>
    ggplot2::ggplot(ggplot2::aes(horizon, cumulative, group = type_lab, linetype = type_lab)) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ product_lab, scales = "free_y") +
    ggplot2::labs(
      title = "Pass-through asimétrico: alzas vs bajas de TPM",
      subtitle = "Coeficientes acumulados por producto. Interpretar con cautela: los ciclos no son simétricos.",
      x = "Meses desde el cambio de TPM",
      y = "Pass-through acumulado"
    ) +
    theme_bcch_like()
}

plot_lp_irf <- function(lp_tbl) {
  lp_tbl |>
    dplyr::mutate(product_lab = label_product(product)) |>
    ggplot2::ggplot(ggplot2::aes(horizon, estimate)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf_low, ymax = conf_high), alpha = 0.18) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ product_lab, scales = "free_y") +
    ggplot2::labs(
      title = "Respuesta dinámica de tasas bancarias a cambios de TPM",
      subtitle = "Local projections con errores Newey-West",
      x = "Horizonte mensual",
      y = "Respuesta acumulada"
    ) +
    theme_bcch_like()
}
