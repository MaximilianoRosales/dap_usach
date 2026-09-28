# dap_usach

Análisis del proyecto disposición a pagar (DAP) — USACH


## Contenido del repositorio

- `data-raw/` — Base de datos brutos (sin modificar)
- `data/` — Base de datos procesadas/limpias
- `scripts/` — Scripts de limpieza, análisis y (futuras) figuras
- `output/` — Tablas, gráficos y resultados
- `R/` — Funciones reutilizables
- `docs/` — Documentación adicional
- `renv.lock` — Versiones exactas de paquetes (reproducibilidad)



## Flujo de trabajo

Ejecuta los scripts en orden:

1. `scripts/01-import.R` — Importar datos desde Google Form
2. `scripts/02-clean.R` — Limpieza y validación
3. `scripts/03-analysis.R` — Análisis PLS-SEM y descriptivas


## Metodología

- **Método:** Contingent Valuation (payment card format)
- **Análisis:** PLS-SEM via `seminr` o `plspm`
