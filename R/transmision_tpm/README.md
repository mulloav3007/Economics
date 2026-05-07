# Scripts R: transmisión TPM

Estos scripts construyen el pipeline del proyecto `Transmisión de la TPM a tasas de mercado`.

Orden de ejecución:

1. `00_setup.R`: paquetes requeridos.
2. `01_config.R`: rutas y parámetros.
3. `02_fetch_bde.R`: funciones para API BDE del BCCh.
4. `03_series_dictionary.R`: diccionario de series.
5. `04_build_monthly_data.R`: base mensual.
6. `05_models_dlm.R`: modelos de rezagos distribuidos.
7. `06_models_local_projections.R`: local projections.
8. `07_ecb_credit_conditions.R`: búsqueda preliminar de series ECB.
9. `08_figures.R`: figuras de respaldo.
10. `09_tables.R`: tablas resumen.

La ejecución mensual recomendada es:

```r
source("scripts/05_update_transmision_tpm.R")
```

o desde PowerShell con `Rscript`.
