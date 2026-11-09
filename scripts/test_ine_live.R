# Prueba de integración de la descarga del INE con los lectores R de producción.
# No necesita credenciales del Banco Central. Ejecutar después de Python --strict.
Sys.setenv(
  IMACEC_INE_PREPARED = "1",
  IMACEC_INE_MANIFEST = "data/raw/ine_source_manifest.json"
)
source("R/imacec_run_all.R", encoding = "UTF-8")

ipi <- get_combined_ipi_levels()
stopifnot(nrow(ipi) >= 24L, all(c("mineria", "manufactura", "electricidad") %in% names(ipi)))
end_ipi <- max(ipi$Periodo)
stopifnot(all(is.finite(unlist(ipi[ipi$Periodo == end_ipi,
  c("mineria", "manufactura", "electricidad")], use.names = FALSE))))

commerce_file <- ine_prepared_excel(commerce_path)
stopifnot(!is.null(commerce_file))
commerce <- read_index_series_from_excel(
  commerce_file,
  c("indice de actividad del comercio al por menor", "actividad del comercio al por menor"),
  "Índice de Actividad del Comercio al por Menor"
)
stopifnot(nrow(commerce) >= 24L, all(is.finite(commerce$value)))

ivs_file <- ine_prepared_excel(ivs_path)
stopifnot(!is.null(ivs_file))
ivs <- read_ivs_official(ivs_file)
stopifnot(nrow(ivs) >= 24L, all(names(ivs_columns) %in% names(ivs)))
stopifnot(all(is.finite(unlist(ivs[ivs$Periodo == max(ivs$Periodo),
  names(ivs_columns)], use.names = FALSE))))

message("INE integración OK · IPI=", format(end_ipi, "%Y-%m"),
        " · Comercio=", format(max(commerce$Periodo), "%Y-%m"),
        " · IVS=", format(max(ivs$Periodo), "%Y-%m"))
