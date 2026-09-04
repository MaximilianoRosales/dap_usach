# dap_usach

Análisis del proyecto disposición a pagar (DAP) — USACH

## Contenido del repositorio

- `data-raw/` — Base de datos brutos (sin modificar)
- `data/` — Base de datos procesadas/limpias
- `scripts/` — Scripts de limpieza, análisis y figuras
- `output/` — Tablas, gráficos y resultados
- `R/` — Funciones reutilizables
- `docs/` — Documentación adicional
- `renv.lock` — Versiones exactas de paquetes (reproducibilidad)

## Requisitos

- **R versión:** 4.x+
- **RStudio:** Recomendado para mayor facilidad

## Instalación de dependencias

Para reproducir el análisis exactamente, ejecuta en la consola de R:

```r
renv::restore()
```

Esto instala automáticamente todos los paquetes en las versiones correctas.

## Flujo de trabajo

Ejecuta los scripts en orden:

1. `scripts/01-import.R` — Importar datos desde Google Form
2. `scripts/02-clean.R` — Limpieza y validación
3. `scripts/03-analysis.R` — Análisis PLS-SEM y descriptivas
4. `scripts/04-figures.R` — Generación de gráficos

O ejecuta todo de una vez:
```r
source("scripts/00-run-all.R")
```

## Datos

Los datos brutos se encuentran en `data-raw/`. Para regenerarlos:
1. Exportar Google Form como CSV
2. Guardar en `data-raw/` con nombre `responses.csv`


## Metodología

- **Método:** Contingent Valuation (payment card format)
- **Análisis:** PLS-SEM via `seminr` o `plspm`
- **Marco teórico:** Theory of Planned Behavior (TPB)