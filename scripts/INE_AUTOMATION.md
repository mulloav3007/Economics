# Descarga automatizada de los indicadores sectoriales INE

## Objetivo y fuentes

El nowcast IMACEC debe incorporar los indicadores del INE tan pronto sean publicados, sin esperar necesariamente a su replicación en la BDE del Banco Central. La fuente preferente es el Excel oficial del INE para:

- **IPI combinado**: IPMin (minería), IPMan (manufactura) e IPEGA (electricidad, gas y agua). El nombre del archivo incorpora el último mes publicado y, por tanto, cambia. No se construye ni se presume el nombre del mes siguiente.
- **Comercio**: Índice de Actividad del Comercio al por Menor. El INE publica un archivo «desde enero de 2018 a la fecha».
- **IVS**: series de los seis sectores de ventas de servicios utilizados para construir \`factor_ivs_real\`. El nombre del archivo también es «a la fecha».

Las páginas de referencia son, respectivamente, las páginas del INE sobre Índice de Producción Industrial, Actividad Mensual del Comercio y Ventas Mensuales de Servicios.

## Descubrimiento y validaciones

\`scripts/fetch_ine_releases.py\` primero intenta leer el HTML del portal oficial. Cuando los enlaces están en una interfaz dinámica, Playwright abre Chrome y selecciona «Cuadros estadísticos» y la base 2018. Para comercio e IVS hay además un respaldo con las rutas estables oficiales, mientras que **IPI no usa un respaldo mensual fijo**.

Los candidatos se restringen a URLs oficiales del INE. El IPI exige el archivo base 2018 y un mes identificable en el nombre o etiqueta. Las descargas son atómicas; se rechazan respuestas HTTP erróneas, HTML en lugar de Excel, archivos de menos de 5 KB y archivos demasiado grandes. Se registra URL, método de descubrimiento, período anunciado (cuando existe), tamaño, SHA-256 y MD5 en \`data/raw/ine_source_manifest.json\`.

La cadena \`R/imacec_data.R\` lee primero los Excel del manifiesto cuando \`IMACEC_INE_PREPARED=1\`. Verifica que el archivo y su MD5 coinciden con el registro, busca las series y valida que la publicación es reciente. Para IPI contrasta explícitamente el período anunciado con una observación válida para las tres series. Si una fuente directa falla, conserva el respaldo original BDE, pero el M8P solo debe generarse cuando todos los regresores del mes objetivo estén disponibles. La ausencia de un dato no se sustituye por el dato del mes anterior.

## Ejecución manual desde la raíz del repositorio

Requiere Python 3.12+, Chrome, R y dependencias indicadas en los workflows. No hace falta una clave INE.

\`\`\`bash
python -m pip install requests beautifulsoup4 playwright
python -m unittest discover -s tests -p 'test_fetch_ine_releases.py' -v
python scripts/fetch_ine_releases.py --strict
Rscript scripts/test_ine_live.R
\`\`\`

Para ejecutar además el nowcast, configure \`BCCH_USER\` y \`BCCH_PASS\` como variables de entorno privadas (Repository Secrets en Actions):

\`\`\`bash
export IMACEC_INE_PREPARED=1
export IMACEC_INE_MANIFEST=data/raw/ine_source_manifest.json
Rscript scripts/01_update_imacec.R
\`\`\`

No incorporar las credenciales al código ni al manifiesto. La opción \`--strict\` falla si cualquier descarga INE falla: se usa en pruebas de integración, mientras que el proceso normal conserva la posibilidad de funcionar con BDE.

## Calendario y operación

El workflow \`.github/workflows/update-imacec.yml\` revisa los datos de lunes a viernes **cada 30 minutos entre las 08:00 y las 18:30 (America/Santiago)**, con ajuste horario automático a verano/invierno. El scheduler de GitHub no asegura puntualidad al minuto; pueden producirse demoras o ejecuciones omitidas.

Una PR ejecuta pruebas unitarias y una comprobación real de descarga/lectura sin credenciales del Banco Central. El workflow de validación end-to-end realiza una ejecución aislada, sin publicar ni hacer push a \`main\`, cuando se dispara manualmente.

## Limitaciones

El INE puede modificar la estructura del HTML, sus enlaces o la disposición de los Excel; en tal caso las pruebas de integración o el propio parser R señalarán el fallo. No existe garantía de disponibilidad continua ni de inmediatez exacta de GitHub Actions. La validación de la publicación vigente no prueba automáticamente la siguiente publicación; vigilar los resultados de Actions en cada cambio de mes.
