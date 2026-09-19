# ============================================================================
# SCRIPT 01: IMPORTAR DATOS
# ============================================================================
# Propósito: Importar respuestas del Google Form y hacer validaciones básicas
# Fecha: 2024
# ============================================================================

#install.packages("tidyverse")
#install.packages("here")
# Cargar paquetes necesarios
library(tidyverse)  # ggplot2, dplyr, readr, etc.
library(here)       # Manejo de rutas relativas

# ============================================================================
# 1. IMPORTAR DATOS BRUTOS
# ============================================================================

# Ruta hacia datos brutos
ruta_bruto <- here("data-raw", "bd_dap_raw.csv")

# Importar CSV
df_raw <- read_csv(ruta_bruto, show_col_types = FALSE)

# Ver dimensiones
cat("Datos importados:\n")
cat("- Filas:", nrow(df_raw), "\n")
cat("- Columnas:", ncol(df_raw), "\n\n")

# Ver primeras filas
head(df_raw)

# ============================================================================
# 2. EXPLORACIÓN INICIAL
# ============================================================================

# Ver estructura
glimpse(df_raw)

# Nombres de columnas originales
names(df_raw)

# ============================================================================
# 3. VALIDACIONES BÁSICAS
# ============================================================================

# Verificar datos duplicados
duplicados <- sum(duplicated(df_raw))
cat("Registros duplicados:", duplicados, "\n")

# Verificar valores faltantes
cat("\nValores faltantes por columna:\n")
print(colSums(is.na(df_raw)))

# ============================================================================
# 4. GUARDAR DATOS IMPORTADOS
# ============================================================================

# Guardar una copia de los datos crudos procesados mínimamente
saveRDS(df_raw, here("data", "01_raw_imported.rds"))

cat("\n✓ Datos importados y guardados en data/01_raw_imported.rds\n")