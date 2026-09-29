# ============================================================================
# SCRIPT 08: ANÁLISIS NANIAR - SUBSET DE 38 VARIABLES DESDE CERO
# ============================================================================
# Solo las 38 variables seleccionadas. Nada más.
#
# ============================================================================

library(tidyverse)
library(naniar)
library(here)

# ============================================================================
# CARGAR Y SELECCIONAR SUBSET
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║        ANÁLISIS NANIAR - 38 VARIABLES SELECCIONADAS       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

df <- readRDS(here("data", "02_clean.rds")) %>%
  select(
    # Sección 1: Vínculo y campus
    vinculo, facultad, anio_car, freq_campus,
    # Sección 2: Compra
    lugar_compra, decisor, gasto_sem, freq_org,
    # Sección 3A: Health Consciousness
    hc1, hc2, hc3, hc4,
    # Sección 3B: Environmental Consciousness
    ec1, ec2, ec3, ec4,
    # Sección 3C: Perceived Control
    pc1, pc2, pc3, pc4,
    # Sección 3D: Attribute Importance
    atr_precio, atr_sabor, atr_salud, atr_pest, atr_cert, atr_acceso,
    # Sección 4: DAP
    dap_porcentaje, cert_dap, no_dap, dap,
    # Sección 5: Demográficas
    sexo, edad, hhsize, under_15, income, junaeb, educ_sos
  )

cat("✓ Dataset cargado:\n")
cat("  Observaciones:", nrow(df), "\n")
cat("  Variables:", ncol(df), "\n")
cat("  Total celdas:", nrow(df) * ncol(df), "\n")
cat("  Total NA's:", sum(is.na(df)), "\n")
cat("  Missingness:", 
    round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 2), 
    "%\n\n")

# ============================================================================
# 1. TABLA RESUMEN DE NA's
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              VARIABLES CON NA's - RESUMEN                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

print(miss_var_summary(df))

cat("\n")

# ============================================================================
# 2. GRÁFICO 1: NA's POR VARIABLE
# ============================================================================

p1 <- gg_miss_var(df, show_pct = TRUE) +
  labs(title = "NA's por Variable",
       x = "Variable",
       y = "Cantidad de NA's") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

print(p1)
ggsave(here("output", "08-miss_var.png"), p1, width = 10, height = 7, dpi = 300)
cat("✓ Guardado: 08-miss_var.png\n\n")

# ============================================================================
# 3. GRÁFICO 2: MAPA VISUAL DE NA's
# ============================================================================

p2 <- vis_miss(df, cluster = FALSE, sort_miss = TRUE) +
  labs(title = "Mapa Visual de NA's")

print(p2)
ggsave(here("output", "08-vis_miss.png"), p2, width = 14, height = 8, dpi = 300)
cat("✓ Guardado: 08-vis_miss.png\n\n")

# ============================================================================
# 4. GRÁFICO 3: NA's POR OBSERVACIÓN
# ============================================================================

p3 <- gg_miss_case(df) +
  labs(title = "NA's por Observación",
       x = "Observación",
       y = "Cantidad de NA's") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))

print(p3)
ggsave(here("output", "08-miss_case.png"), p3, width = 11, height = 6, dpi = 300)
cat("✓ Guardado: 08-miss_case.png\n\n")

# ============================================================================
# 5. GRÁFICO 4: PATRONES DE NA's (UPSET PLOT)
# ============================================================================

vars_with_na <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  select_if(~. > 0) %>%
  names()

if (length(vars_with_na) > 0) {
  
  p4 <- gg_miss_upset(df, nsets = 6, order.by = "freq") +
    labs(title = "Patrones: ¿Cuáles variables faltan JUNTAS?")
  
  print(p4)
  ggsave(here("output", "08-miss_upset.png"), p4, width = 12, height = 7, dpi = 300)
  cat("✓ Guardado: 08-miss_upset.png\n\n")
  
}

# ============================================================================
# 6. EXPLORACIÓN DETALLADA
# ============================================================================

if (length(vars_with_na) > 0) {
  
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║        ANÁLISIS DETALLADO DE VARIABLES CON NA's           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  for (var in vars_with_na) {
    
    na_count <- sum(is.na(df[[var]]))
    na_pct <- round(100 * na_count / nrow(df), 2)
    
    cat("────────────────────────────────────\n")
    cat("Variable: ", var, "\n")
    cat("────────────────────────────────────\n")
    cat("  NA's:", na_count, "(", na_pct, "% de", nrow(df), "obs.)\n")
    
    # Estadísticas si es numérica
    if (is.numeric(df[[var]])) {
      valid <- sum(!is.na(df[[var]]))
      cat("  Valores válidos:", valid, "\n")
      cat("  Media:", round(mean(df[[var]], na.rm = TRUE), 2), "\n")
      cat("  Mediana:", round(median(df[[var]], na.rm = TRUE), 2), "\n")
      cat("  Desv.Est.:", round(sd(df[[var]], na.rm = TRUE), 2), "\n")
      cat("  Rango:", round(min(df[[var]], na.rm = TRUE), 2), 
          "-", round(max(df[[var]], na.rm = TRUE), 2), "\n")
    }
    
    # Filas con NA
    obs_na <- which(is.na(df[[var]]))
    if (length(obs_na) > 0) {
      if (length(obs_na) <= 10) {
        cat("  Filas con NA:", paste(obs_na, collapse = ", "), "\n")
      } else {
        cat("  Filas con NA (primeras 10 de", length(obs_na), "):\n")
        cat("    ", paste(obs_na[1:10], collapse = ", "), "...\n")
      }
    }
    
    cat("\n")
  }
  
}

# ============================================================================
# 7. MECANISMO DE MISSINGNESS
# ============================================================================

if (length(vars_with_na) >= 2) {
  
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   CORRELACIÓN DE MISSINGNESS: ¿Faltan variables juntas?   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  missing_matrix <- df %>%
    select(all_of(vars_with_na)) %>%
    is.na() %>%
    as.data.frame() %>%
    mutate(across(everything(), as.numeric))
  
  miss_corr <- cor(missing_matrix, use = "complete.obs")
  
  print(round(miss_corr, 3))
  
  cat("\nInterpretación:\n")
  cat("  1.0 = variables faltan EXACTAMENTE juntas\n")
  cat("  0.0 = variables faltan INDEPENDIENTEMENTE\n")
  cat("  Valores intermedios = correlación parcial\n\n")
}

# ============================================================================
# 8. COMPARACIÓN: CASOS COMPLETOS vs CON NA's
# ============================================================================

complete <- df %>% drop_na()

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║      COMPARACIÓN: CASOS COMPLETOS vs CON NA's             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Casos COMPLETOS (sin NA's en ninguna variable):\n")
cat("  n =", nrow(complete), "(", 
    round(100 * nrow(complete) / nrow(df), 1), "% del total)\n\n")

cat("Casos CON AL MENOS UN NA:\n")
cat("  n =", nrow(df) - nrow(complete), "(", 
    round(100 * (nrow(df) - nrow(complete)) / nrow(df), 1), "% del total)\n\n")

# Comparar características
if (nrow(complete) > 0) {
  
  df_indicator <- df %>%
    mutate(has_na = !complete.cases(.))
  
  cat("Diferencias demográficas por missingness:\n\n")
  
  # Edad
  edad_comparison <- df_indicator %>%
    group_by(has_na) %>%
    summarise(
      edad_media = round(mean(edad, na.rm = TRUE), 1),
      edad_de = round(sd(edad, na.rm = TRUE), 1),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(has_na = ifelse(has_na, "Con NA's", "Completos"))
  
  cat("EDAD:\n")
  print(edad_comparison)
  cat("\n")
  
  # Sexo
  if ("sexo" %in% names(df_indicator)) {
    sex_comparison <- df_indicator %>%
      group_by(has_na, sexo) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
      mutate(has_na = ifelse(has_na, "Con NA's", "Completos"))
    
    cat("SEXO:\n")
    print(sex_comparison)
    cat("\n")
  }
}

# ============================================================================
# 9. RESUMEN FINAL Y RECOMENDACIONES
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                    RESUMEN Y RECOMENDACIONES              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("DATOS DEL SUBSET (38 variables):\n")
cat("  Observaciones:", nrow(df), "\n")
cat("  Variables:", ncol(df), "\n")
cat("  Total celdas:", nrow(df) * ncol(df), "\n")
cat("  Total NA's:", sum(is.na(df)), "\n")
cat("  Missingness general:", 
    round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 2), "%\n\n")

if (length(vars_with_na) > 0) {
  cat("VARIABLES CON NA's:", length(vars_with_na), "\n")
  for (var in vars_with_na) {
    na_pct <- round(100 * sum(is.na(df[[var]])) / nrow(df), 1)
    cat("  •", var, "-", na_pct, "% de NA's\n")
  }
} else {
  cat("✓ NO HAY NA's en estas 38 variables\n")
}

cat("\n")

# Recomendación
n_complete <- nrow(complete)
pct_complete <- round(100 * n_complete / nrow(df), 1)

cat("RECOMENDACIÓN PARA ANÁLISIS:\n\n")

if (pct_complete >= 95) {
  cat("✓ LISTWISE DELETION (eliminar filas con NA)\n")
  cat("  → Retención: ", pct_complete, "%\n")
  cat("  → Poder estadístico: EXCELENTE\n")
  cat("  → Implementación: df %>% drop_na()\n")
} else if (pct_complete >= 90) {
  cat("✓ LISTWISE DELETION\n")
  cat("  → Retención: ", pct_complete, "%\n")
  cat("  → Poder estadístico: BUENO\n")
  cat("  → Implementación: df %>% drop_na()\n")
} else if (pct_complete >= 80) {
  cat("⚠ PAIRWISE DELETION\n")
  cat("  → Retención: ", pct_complete, "% (listwise)\n")
  cat("  → Pero pairwise retiene más N por análisis\n")
  cat("  → Recomendación: usar completes.cases() por variable\n")
} else {
  cat("⚠ PAIRWISE DELETION\n")
  cat("  → Retención: ", pct_complete, "%\n")
  cat("  → Mucho missingness\n")
  cat("  → Considerar excluir variables con >10% NA\n")
}

cat("\n════════════════════════════════════════════════════════════\n")

# ============================================================================
# BONUS: Guardar dataset limpio
# ============================================================================

df_clean_subset <- df %>% drop_na()

cat("\nBONUS: Dataset limpio guardado\n")
cat("Para usar después:\n")
cat("  df_clean_subset <- readRDS(here('data', '08_clean_subset.rds'))\n")

saveRDS(df_clean_subset, here("data", "08_clean_subset.rds"))
cat("\n✓ Guardado: data/08_clean_subset.rds\n")
cat("  (", nrow(df_clean_subset), "observaciones,", 
    ncol(df_clean_subset), "variables)\n")