# Subproyecto IPoM / IRIS Matlab

Esta carpeta deja ordenado el trabajo IPoM dentro del mismo repositorio Quarto. La lógica es separar tres capas:

1. **Motor IRIS/Matlab**: resuelve el modelo y genera `fcast_*.csv`.
2. **Outputs limpios para Quarto**: R transforma los CSV crudos de IRIS a formato tidy.
3. **Página Quarto**: `proyectos/ipom-iris.qmd` solo lee resultados procesados y grafica.

## Estructura

```text
matlab/ipom/
├─ model/
│  └─ minimep0.model              # ecuaciones del modelo, preservadas
├─ src/
│  ├─ config_ipom.m               # configuración central del subproyecto
│  ├─ setup_ipom_project.m         # prepara paths y carpetas
│  ├─ run_all_ipom.m               # wrapper principal Matlab/IRIS
│  ├─ readmodel_alternativo.m      # calibración/modelo usado actualmente
│  ├─ identificar_shocks_ipom.m    # identifica shocks para baseline tipo IPoM
│  ├─ fcast_alt_ipom.m             # escenario alternativo editable
│  └─ exportar_outputs_quarto.m    # helper para guardar outputs IRIS
├─ inputs/
│  ├─ history.csv                  # historia base IRIS
│  └─ Data.csv                     # input histórico para makedata, si se requiere
├─ outputs/
│  ├─ raw_iris/                    # CSV crudos exportados por IRIS/Matlab
│  └─ quarto/                      # espejo de data/processed/ipom/*.csv
├─ legacy_original/
│  ├─ m_files/                     # scripts originales completos como respaldo
│  └─ reports_pdf/                 # PDFs antiguos útiles solo como referencia
└─ runtime/                        # legacy/respaldo; el flujo nuevo no depende de esto
```

## Regla de oro

- **No tocar `model/minimep0.model`** salvo cambios deliberados de ecuaciones.
- Editar escenarios en `src/fcast_alt_ipom.m`.
- Editar rutas/opciones generales en `src/config_ipom.m`.
- Quarto debe leer solo `data/processed/ipom/`.

## Flujo recomendado

Desde la raíz del repositorio:

```matlab
% Desde MATLAB
cd('D:\Users\mullo\Documents\GitHub\Economics\matlab\ipom\src')
run_all_ipom
```

Luego, desde la raiz del repositorio:

```powershell
Rscript scripts/03_build_ipom_outputs.R
quarto render
```

Para una primera prueba sin recalcular IRIS, basta con:

```powershell
Rscript scripts/03_build_ipom_outputs.R
quarto render
```

porque ya se dejaron CSV crudos en `outputs/raw_iris/` y CSV limpios en `data/processed/ipom/`.
