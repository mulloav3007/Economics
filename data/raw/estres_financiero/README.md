# Fuente de datos raw

`merged_full_dataset.csv` es una conversión depurada del archivo `ExchangeReg/Base/merged_full_dataset.xlsx` recibido en el ZIP original.

Cambios aplicados:

- La primera columna de fechas fue renombrada a `date`.
- Los nombres de columnas fueron estandarizados a minúsculas y formato `snake_case`.
- No se agregaron credenciales, claves API ni descargas automáticas.

La cobertura efectiva usada por los modelos empieza en 2012-10-05, por disponibilidad conjunta de cobre, USD/CLP, tasa soberana chilena 10Y y variables externas.
