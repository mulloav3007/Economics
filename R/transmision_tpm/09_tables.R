# ============================================================
# Tablas resumen
# ============================================================

make_pass_through_summary <- function(pt_tbl, horizons = c(1, 3, 6)) {
  pt_tbl |>
    dplyr::filter(type == "total", horizon %in% horizons) |>
    dplyr::mutate(
      horizon = paste0("h", horizon),
      product_label = label_product(product)
    ) |>
    dplyr::select(product, product_label, horizon, cumulative) |>
    tidyr::pivot_wider(names_from = horizon, values_from = cumulative) |>
    dplyr::arrange(product)
}

estimate_sample_robustness <- function(model_data, k = 6) {
  samples <- list(
    full = c(as.Date("1900-01-01"), as.Date("2100-01-01")),
    post_2010 = c(as.Date("2010-01-01"), as.Date("2100-01-01")),
    excl_2020_2023 = c(as.Date("1900-01-01"), as.Date("2100-01-01"))
  )

  out <- list()

  for (nm in names(samples)) {
    dat <- model_data

    if (nm == "post_2010") {
      dat <- dat |>
        dplyr::filter(date >= as.Date("2010-01-01"))
    }

    if (nm == "excl_2020_2023") {
      dat <- dat |>
        dplyr::filter(!(date >= as.Date("2020-01-01") & date <= as.Date("2023-12-01")))
    }

    if (nrow(dat) < 60) next

    mods <- tryCatch(
      estimate_all_dlm(dat, k = k, asymmetric = FALSE),
      error = function(e) list()
    )

    if (length(mods) == 0) next

    pt <- purrr::map_dfr(mods, ~ extract_cumulative_pt(.x, k = k, asymmetric = FALSE)) |>
      dplyr::filter(type == "total", horizon == k) |>
      dplyr::mutate(sample = nm) |>
      dplyr::select(sample, product, horizon, cumulative)

    out[[nm]] <- pt
  }

  dplyr::bind_rows(out) |>
    dplyr::mutate(product_label = label_product(product)) |>
    dplyr::select(sample, product, product_label, horizon, cumulative) |>
    dplyr::arrange(product, sample)
}
