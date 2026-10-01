<div class="kpi-grid imacec-cycle-kpis">
  <div class="kpi"><div class="kpi-label">Último dato efectivo</div><div class="kpi-value">{{ imacec.last_actual }}</div><div class="kpi-note">Total y no minero publicados por el BCCh</div></div>
  <div class="kpi"><div class="kpi-label">Período en seguimiento</div><div class="kpi-value">{{ imacec.target }}</div><div class="kpi-note">La EEE de M corresponde al IMACEC de M−1</div></div>
  <div class="kpi"><div class="kpi-label">Señal principal</div><div class="kpi-value">{{ imacec.default_label }}</div><div class="kpi-note">Cambia solo cuando el corte informativo está completo</div></div>
  <div class="kpi"><div class="kpi-label">Última actualización</div><div class="kpi-value">{{ imacec.updated }}</div><div class="kpi-note">Última ejecución publicada del ciclo</div></div>
</div>

<div class="callout imacec-status" data-cycle-stage="{{ imacec.stage }}">
  <strong>Estado del ciclo:</strong> {{ imacec.stage_label }}.
</div>

{% if not imacec.ready %}
<div class="callout callout-warning">
  <strong>Transición segura pendiente.</strong> La interfaz conserva únicamente el IMACEC efectivo hasta que GitHub Actions genere por primera vez el nuevo archivo de estados; ninguna cifra antigua se presenta como corte experimental o corte INE.
</div>
{% endif %}

{% if imacec.ready %}
<div class="download-grid imacec-downloads imacec-downloads-top">
  <a class="download" href="../assets/files/{{ imacec.download_graphs_file }}" data-multi-download="../assets/img/charts/imacec_total_history.png::imacec-total-{{ imacec.target_slug }}.png|../assets/img/charts/imacec_nonmining_history.png::imacec-no-minero-{{ imacec.target_slug }}.png|../assets/img/charts/imacec_projection_table.png::imacec-tabla-proyecciones-{{ imacec.target_slug }}.png"><div><strong>Descargar gráficos</strong><span>3 PNG directos · IMACEC total, no minero y tabla · {{ imacec.target }}</span></div><span>↓</span></a>
  <a class="download" href="../assets/files/{{ imacec.download_table_file }}" download="{{ imacec.download_table_file }}"><div><strong>Descargar tabla</strong><span>CSV · historia observada y proyección vigente · {{ imacec.target }}</span></div><span>↓</span></a>
</div>
{% endif %}

## Seguimiento mensual

Los dos paneles responden al mismo calendario de publicación. Antes de la nueva EEE, el resumen conserva el último dato efectivo y compara los puntos del corte experimental, corte INE, AR(1), media móvil y EEE disponibles. Después, el selector deja por defecto el corte más informativo que ya esté completo.

### IMACEC total

<div class="interactive-card imacec-panel" data-project-chart="imacec-total" data-url="../assets/data/project_charts.json">
  <div class="chart-controls">
    <label for="imacec-total-dataset">Corte o resumen</label><select id="imacec-total-dataset" data-dataset-select></select>
    <label for="imacec-total-range">Ventana</label><select id="imacec-total-range" data-range-select><option value="0">Historia completa</option><option value="36">Últimos 36 meses</option><option value="24">Últimos 24 meses</option><option value="12">Últimos 12 meses</option></select>
  </div>
  <div class="interactive-chart" data-generic-chart aria-live="polite"></div>
  <div class="chart-legend" data-chart-legend></div>
  <p class="chart-note" data-chart-note></p>
  <div data-chart-table></div>
  <p class="chart-help">Pasa el cursor sobre el gráfico para consultar el IMACEC efectivo, el ajuste histórico, la proyección vigente y la EEE comparable.</p>
</div>

### IMACEC no minero

<div class="interactive-card imacec-panel" data-project-chart="imacec-nonmining" data-url="../assets/data/project_charts.json">
  <div class="chart-controls">
    <label for="imacec-nonmining-dataset">Corte o resumen</label><select id="imacec-nonmining-dataset" data-dataset-select></select>
    <label for="imacec-nonmining-range">Ventana</label><select id="imacec-nonmining-range" data-range-select><option value="0">Historia completa</option><option value="36">Últimos 36 meses</option><option value="24">Últimos 24 meses</option><option value="12">Últimos 12 meses</option></select>
  </div>
  <div class="interactive-chart" data-generic-chart aria-live="polite"></div>
  <div class="chart-legend" data-chart-legend></div>
  <p class="chart-note" data-chart-note></p>
  <div data-chart-table></div>
  <p class="chart-help">La serie no minera se estima de forma independiente, manteniendo el conjunto fijo de predictores del corte experimental o del corte INE.</p>
</div>

## Cómo se actualiza

| Momento del mes | Publicación visible | Opción predeterminada |
|---|---|---|
| Dato efectivo recién publicado, antes de la nueva EEE | Historia, efectivo del último mes, corte experimental, corte INE, AR(1), media móvil y EEE comparables | Resumen del período |
| Nueva EEE, antes de estadísticas experimentales | EEE alineada al mes anterior y dos referencias simples | AR(1) de referencia |
| Estadísticas experimentales completas | Efectivo, ajuste del corte experimental, proyección, proxies y punto EEE | Corte experimental |
| Indicadores INE e IVS completos | El corte experimental y los proxies siguen visibles y se añade el corte INE | Corte INE |
| Publicación del IMACEC objetivo | Efectivo y puntos archivados de cada corte; las reconstrucciones quedan rotuladas aparte | Resumen del período |

El archivo de vintages conserva las proyecciones disponibles con su fecha. Para julio de 2026, el punto del corte INE corresponde a la salida operativa aportada por el autor el 31 de agosto. La curva de ajuste histórico se reestima con la base disponible en cada ejecución: es un ajuste dentro de muestra y no reconstruye el historial de pronósticos emitidos en tiempo real.

La estimación histórica utiliza exclusivamente períodos anteriores al mes objetivo. Si faltan indicadores contemporáneos, puede mostrarse ese ajuste aunque no exista una nueva proyección automática. La nota del gráfico identifica la disponibilidad de ambos resultados.

## Especificaciones publicadas

<div class="equation"><span class="equation-label">Corte experimental</span>IMACEC<sub>t</sub> = f(ventas<sub>t</sub>, crédito monto<sub>t</sub>, crédito cantidad<sub>t</sub>, IMACEC<sub>t−1</sub>, calendario)</div>

<div class="equation"><span class="equation-label">Corte INE + IVS real</span>IMACEC<sub>t</sub> = f(crédito cantidad<sub>t</sub>, IMACEC<sub>t−1</sub>, avisos<sub>t−1</sub>, sectores INE<sub>t</sub>, IVS real<sub>t</sub>, crédito real<sub>t</sub>, calendario)</div>

Las dos fórmulas se reestiman por separado para **IMACEC total** e **IMACEC no minero**. El factor IVS real promedia seis índices de ventas de servicios deflactados por el IPC de Servicios y el corte INE solo se habilita con cobertura contemporánea completa.

Los predictores sectoriales del corte INE se descargan desde las páginas oficiales del INE en cada ejecución. El pipeline descubre dinámicamente el Excel vigente de **Índice de Producción Industrial** —sin fijar el mes en el nombre del archivo— y extrae las series originales de **IPMin, IPMan e IPEGA**. Para comercio utiliza el **Índice de Actividad del Comercio al por Menor**, manteniendo la misma definición histórica del predictor. Los IVS también se refrescan desde su Excel oficial y se deflactan con IPC Servicios. La BDE queda como respaldo técnico, no como fuente primaria. [IPI · INE](https://www.ine.gob.cl/estadisticas-por-tema/industria-energia-y-construccion/indice-de-produccion-industrial) · [Comercio · INE](https://www.ine.gob.cl/estadisticas-por-tema/comercio-y-servicios/actividad-mensual-del-comercio).

## Evaluación pseudo out-of-sample

{{ imacec.oos_table|safe }}

La evaluación es recursiva y mantiene fijas las dos especificaciones principales. También incluye AR(1) y media móvil de tres meses como benchmarks simples, todos evaluados sobre la misma ventana común. Usa la base final disponible; por ello mide capacidad predictiva comparable, no reproduce todas las revisiones de cada vintage histórico.

## Reproducibilidad y publicación

- La EEE publicada en **M** se asigna al IMACEC de **M−1**.
- AR(1) y media móvil de tres meses son referencias transparentes; no sustituyen a los cortes experimental o INE.
- El modelo principal cambia por disponibilidad verificable, no por cuál arroje la cifra más conveniente.
- La tarea programada lanza sondeos redundantes cada 30 minutos durante una ventana amplia de días hábiles para amortiguar retrasos de GitHub Actions; cada ejecución vuelve a consultar las fuentes oficiales y publica el nuevo corte cuando la cobertura requerida está completa.

<div class="callout">
  <strong>Fuentes de alta frecuencia:</strong> IPMin, IPMan, IPEGA, comercio al por menor e IVS se leen directamente desde los Excel oficiales del INE. La BDE se conserva únicamente como respaldo de continuidad si una descarga oficial falla.
</div>

Última ejecución publicada: **{{ imacec.updated }}**.

## Archivos principales

{% if imacec.ready %}
<div class="download-grid">
  <a class="download" href="../assets/files/imacec-nowcast-summary.csv"><div><strong>Estado y proyecciones vigentes</strong><span>CSV · total, no minero, corte experimental, corte INE y EEE</span></div><span>↓</span></a>
  <a class="download" href="../assets/files/imacec-oos-metrics.csv"><div><strong>Métricas pseudo OOS</strong><span>CSV · dos series y dos especificaciones</span></div><span>↓</span></a>
</div>
{% endif %}
