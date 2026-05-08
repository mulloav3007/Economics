# ============================================================
# 03_plots.R
# Figuras del índice de estrés financiero de mercado para Chile.
# ============================================================

estres_plot_index <- function(index_data) {
  ggplot2::ggplot(index_data, ggplot2::aes(x = .data$date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$stress_market, colour = "Índice diario"),
      linewidth = 0.45,
      alpha = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$stress_market_30d, colour = "Media móvil 30 días"),
      linewidth = 1.05,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c8c0b3", linewidth = 0.45) +
    ggplot2::geom_hline(yintercept = 0.75, colour = "#c8c0b3", linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 1.50, colour = "#c8c0b3", linewidth = 0.35, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = -0.75, colour = "#c8c0b3", linewidth = 0.35, linetype = "dashed") +
    ggplot2::scale_colour_manual(values = c(
      "Media móvil 30 días" = "#27384a",
      "Índice diario" = "#8a94a3"
    )) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Índice de estrés financiero de mercado para Chile",
      subtitle = "Promedio de residuos estandarizados de USD/CLP y tasa soberana 10Y",
      x = NULL,
      y = "Z-score agregado"
    ) +
    estres_theme()
}

estres_plot_components <- function(index_data) {
  index_data |>
    dplyr::select(date, stress_fx_30d, stress_y10_30d) |>
    tidyr::pivot_longer(
      cols = c("stress_fx_30d", "stress_y10_30d"),
      names_to = "component",
      values_to = "z_score"
    ) |>
    dplyr::mutate(
      component = dplyr::recode(
        .data$component,
        stress_fx_30d = "Estrés cambiario",
        stress_y10_30d = "Estrés tasa 10Y"
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$z_score, colour = .data$component)) +
    ggplot2::geom_line(linewidth = 0.95, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c8c0b3", linewidth = 0.45) +
    ggplot2::geom_hline(yintercept = 1.5, colour = "#c8c0b3", linewidth = 0.35, linetype = "dotted") +
    ggplot2::scale_colour_manual(values = c(
      "Estrés cambiario" = "#7b5e42",
      "Estrés tasa 10Y" = "#4d6277"
    )) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Componentes del estrés financiero",
      subtitle = "Cada componente corresponde a un residuo de modelo expresado como z-score",
      x = NULL,
      y = "Z-score, media móvil 30 días"
    ) +
    estres_theme()
}

estres_plot_fx_fit <- function(index_data) {
  index_data |>
    dplyr::select(date, clp, fitted_clp) |>
    tidyr::pivot_longer(
      cols = c("clp", "fitted_clp"),
      names_to = "series",
      values_to = "value"
    ) |>
    dplyr::mutate(
      series = dplyr::recode(
        .data$series,
        clp = "USD/CLP observado",
        fitted_clp = "USD/CLP ajustado por fundamentos"
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$value, colour = .data$series)) +
    ggplot2::geom_line(linewidth = 0.85, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c(
      "USD/CLP observado" = "#27384a",
      "USD/CLP ajustado por fundamentos" = "#7b5e42"
    )) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(
      title = "Tipo de cambio: observado versus ajustado",
      subtitle = "El residuo positivo indica un CLP más depreciado que lo explicado por fundamentos externos",
      x = NULL,
      y = "Pesos por dólar"
    ) +
    estres_theme()
}

estres_plot_y10_fit <- function(index_data) {
  index_data |>
    dplyr::select(date, y10_clp, fitted_y10_clp) |>
    tidyr::pivot_longer(
      cols = c("y10_clp", "fitted_y10_clp"),
      names_to = "series",
      values_to = "value"
    ) |>
    dplyr::mutate(
      series = dplyr::recode(
        .data$series,
        y10_clp = "Tasa 10Y observada",
        fitted_y10_clp = "Tasa 10Y ajustada"
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$value, colour = .data$series)) +
    ggplot2::geom_line(linewidth = 0.85, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c(
      "Tasa 10Y observada" = "#27384a",
      "Tasa 10Y ajustada" = "#7b5e42"
    )) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ",", suffix = "%")) +
    ggplot2::labs(
      title = "Tasa soberana 10Y: observada versus ajustada",
      subtitle = "El residuo positivo indica una tasa local más alta que la predicha por condiciones globales",
      x = NULL,
      y = "Porcentaje"
    ) +
    estres_theme()
}

estres_save_figures <- function(index_data, root = estres_project_root(), width = 10.8, height = 5.6, dpi = 180) {
  estres_make_dirs(root)
  img_dir <- file.path(root, "assets/img/estres_financiero")

  figures <- list(
    stress_index_chile = estres_plot_index(index_data),
    components_zscores_chile = estres_plot_components(index_data),
    fx_fit_chile = estres_plot_fx_fit(index_data),
    y10_fit_chile = estres_plot_y10_fit(index_data)
  )

  purrr::iwalk(
    figures,
    ~ ggplot2::ggsave(
      filename = file.path(img_dir, paste0(.y, ".png")),
      plot = .x,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  )

  invisible(figures)
}
