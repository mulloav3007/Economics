# ============================================================
# Local projections
# ============================================================

estimate_lp_product <- function(df, product_name, h_max = 12, shock_var = "dtpm",
                                spec = c("macro", "base", "curve", "macro_curve")) {
  spec <- match.arg(spec)

  dat0 <- df |>
    dplyr::filter(product == product_name) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      month_fe = factor(lubridate::month(date)),
      drate_l1 = dplyr::lag(drate, 1),
      dtpm_l1 = dplyr::lag(dtpm, 1),
      dtpm_l2 = dplyr::lag(dtpm, 2)
    )

  controls <- build_control_terms(dat0, product_name, spec = spec)
  rhs_controls <- rhs_join(shock_var, "drate_l1", "dtpm_l1", "dtpm_l2", controls, "month_fe")

  purrr::map_dfr(0:h_max, function(h) {
    dat <- dat0 |>
      dplyr::mutate(y_h = dplyr::lead(rate, h) - dplyr::lag(rate, 1))

    fml <- stats::as.formula(paste("y_h ~", rhs_controls))

    mod <- stats::lm(fml, data = dat)
    vc <- sandwich::NeweyWest(mod, lag = max(4, h + 1), prewhite = FALSE, adjust = TRUE)

    broom::tidy(lmtest::coeftest(mod, vcov. = vc)) |>
      dplyr::filter(term == shock_var) |>
      dplyr::transmute(
        product = product_name,
        horizon = h,
        estimate = estimate,
        std_error = std.error,
        conf_low = estimate - 1.96 * std.error,
        conf_high = estimate + 1.96 * std.error,
        spec = spec
      )
  })
}

estimate_all_lp <- function(model_data, h_max = 12, shock_var = "dtpm",
                            spec = c("macro", "base", "curve", "macro_curve")) {
  spec <- match.arg(spec)
  products <- sort(unique(model_data$product))
  purrr::map_dfr(products, ~ estimate_lp_product(model_data, .x, h_max = h_max, shock_var = shock_var, spec = spec))
}
