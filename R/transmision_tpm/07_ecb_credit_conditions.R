# ============================================================
# Encuesta de Crédito Bancario: búsqueda y preparación
# ============================================================

find_first_existing_col <- function(df, candidates) {
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) return(NULL)
  hits[[1]]
}

discover_ecb_candidates <- function(save = TRUE) {
  out <- list()

  for (freq in c("QUARTERLY", "MONTHLY")) {
    ans <- tryCatch(
      search_bde_catalog(freq),
      error = function(e) {
        warning("No se pudo buscar catálogo ", freq, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )

    if (nrow(ans) == 0) next

    text_cols <- names(ans)[stringr::str_detect(names(ans), "Title|title|spanish|Spanish|descrip|Descripcion|Description")]
    id_col <- find_first_existing_col(ans, c("seriesId", "SeriesId", "seriesID", "id", "codigo"))

    if (length(text_cols) == 0) text_cols <- names(ans)

    # Con el pipe base (|>) no existe el pronombre "." de magrittr dentro de mutate().
    # Por eso construimos search_text fuera del pipeline, de forma explícita y robusta.
    ans2 <- ans
    txt_df <- dplyr::select(ans2, dplyr::all_of(text_cols))
    ans2$search_text <- apply(txt_df, 1, function(row) paste(row, collapse = " "))

    ans2 <- ans2 |>
      dplyr::filter(
        stringr::str_detect(search_text, stringr::regex("Encuesta de Crédito Bancario|Credito Bancario|Crédito Bancario|ECB|estándares|estandares|demanda", ignore_case = TRUE)) |
          if (!is.null(id_col)) stringr::str_detect(.data[[id_col]], "F089\\.ECB") else FALSE
      )

    out[[freq]] <- ans2
  }

  candidates <- dplyr::bind_rows(out)

  if (save) {
    readr::write_csv(candidates, file.path(cfg$metadata_dir, "ecb_catalog_candidates.csv"))
  }

  candidates
}

# Esta función está preparada para cuando selecciones manualmente códigos ECB desde
# data/metadata/ecb_catalog_candidates.csv.
build_quarterly_ecb_dataset <- function(monthly_panel, ecb_series_tbl = NULL) {
  monthly_q <- monthly_panel |>
    dplyr::mutate(quarter = zoo::as.yearqtr(date)) |>
    dplyr::group_by(quarter) |>
    dplyr::summarise(
      tpm = safe_last(tpm),
      consumo_total = if ("consumo_total" %in% names(dplyr::cur_data())) safe_mean(consumo_total) else NA_real_,
      comercial_total = if ("comercial_total" %in% names(dplyr::cur_data())) safe_mean(comercial_total) else NA_real_,
      vivienda_uf = if ("vivienda_uf" %in% names(dplyr::cur_data())) safe_mean(vivienda_uf) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(quarter) |>
    dplyr::mutate(dtpm = tpm - dplyr::lag(tpm))

  if (is.null(ecb_series_tbl) || nrow(ecb_series_tbl) == 0) {
    return(monthly_q)
  }

  ecb_raw <- fetch_bde_many(ecb_series_tbl, first_date = cfg$first_date, last_date = cfg$last_date, stop_if_required_missing = FALSE)$data

  ecb_q <- ecb_raw |>
    dplyr::mutate(quarter = zoo::as.yearqtr(date)) |>
    dplyr::select(quarter, name, value) |>
    tidyr::pivot_wider(names_from = name, values_from = value)

  dplyr::left_join(monthly_q, ecb_q, by = "quarter")
}
