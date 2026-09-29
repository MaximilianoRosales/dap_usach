

library(tidyverse)
library(here)

# ============================================================================
# 1. CARGAR Y PREPARAR DATOS
# ============================================================================

df_indices <- readRDS(here("data", "03_with_indices.rds"))

# Preparar datos para análisis por facultad
df_plot <- df_indices %>%
  filter(!is.na(dap_porcentaje), !is.na(facultad)) %>%
  mutate(facultad = as.character(facultad))


# ============================================================================
# 2. EXPLORACIÓN INICIAL
# ============================================================================

tabla_facultades <- df_plot %>%
  group_by(facultad) %>%
  summarise(
    n = n(),
    pct = round(100 * n / nrow(df_plot), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(tabla_facultades)
cat("\n")

# ============================================================================
# 3. ESTADÍSTICAS DESCRIPTIVAS POR FACULTAD
# ============================================================================

cat("========== ESTADÍSTICAS DE DAP POR FACULTAD ==========\n\n")
stats_facultad <- df_plot %>%
  group_by(facultad) %>%
  summarise(
    n = n(),
    Media = round(mean(dap_porcentaje, na.rm = TRUE), 2),
    DE = round(sd(dap_porcentaje, na.rm = TRUE), 2),
    Mediana = round(median(dap_porcentaje, na.rm = TRUE), 2),
    Min = min(dap_porcentaje, na.rm = TRUE),
    Max = max(dap_porcentaje, na.rm = TRUE),
    Q1 = round(quantile(dap_porcentaje, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(dap_porcentaje, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Media))

print(stats_facultad)
cat("\n")

# ============================================================================
# 4. GRÁFICO 1: BOXPLOT (ordenado por mediana)
# ============================================================================

cat("Generando gráficos...\n\n")

p_boxplot <- ggplot(df_plot, 
                    aes(x = reorder(facultad, dap_porcentaje, FUN = median), 
                        y = dap_porcentaje, 
                        fill = facultad)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  labs(
    title = "Disposición a Pagar por Alimentos Orgánicos\nSegún Facultad",
    subtitle = "Ordenado por mediana de DAP",
    x = "Facultad",
    y = "DAP (%)",
    caption = "n = observaciones con DAP válida"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 11),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank()
  )

print(p_boxplot)
ggsave(here("output", "05-dap_por_facultad_boxplot.png"), 
       p_boxplot, width = 12, height = 7, dpi = 300)
cat("✓ Guardado: 05-dap_por_facultad_boxplot.png\n\n")

# ============================================================================
# 5. GRÁFICO 2: VIOLIN PLOT (alternativa elegante)
# ============================================================================

p_violin <- ggplot(df_plot, 
                   aes(x = reorder(facultad, dap_porcentaje, FUN = median), 
                       y = dap_porcentaje, 
                       fill = facultad)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.15, alpha = 0.8, fill = "white", 
               outlier.size = 1) +
  labs(
    title = "Distribución de DAP por Facultad",
    x = "Facultad",
    y = "DAP (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3)
  )

print(p_violin)
ggsave(here("output", "05-dap_por_facultad_violin.png"), 
       p_violin, width = 12, height = 7, dpi = 300)
cat("✓ Guardado: 05-dap_por_facultad_violin.png\n\n")

# ============================================================================
# 6. GRÁFICO 3: MEDIAS CON INTERVALOS DE CONFIANZA
# ============================================================================

stats_means <- df_plot %>%
  group_by(facultad) %>%
  summarise(
    Media = mean(dap_porcentaje, na.rm = TRUE),
    SE = sd(dap_porcentaje, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    LI = Media - 1.96 * SE,
    LS = Media + 1.96 * SE
  ) %>%
  arrange(desc(Media))

p_means <- ggplot(stats_means, aes(x = reorder(facultad, Media), y = Media)) +
  geom_col(aes(fill = facultad), alpha = 0.7) +
  geom_errorbar(aes(ymin = LI, ymax = LS), width = 0.3, size = 1) +
  geom_text(aes(label = paste0(round(Media, 1), "%")), 
            vjust = -1.5, size = 3.5, fontface = "bold") +
  labs(
    title = "DAP Promedio por Facultad (Intervalo Confianza 95%)",
    x = "Facultad",
    y = "DAP Promedio (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank()
  )

print(p_means)
ggsave(here("output", "05-dap_facultad_medias_ci.png"), 
       p_means, width = 11, height = 6, dpi = 300)
cat("✓ Guardado: 05-dap_facultad_medias_ci.png\n\n")

# ============================================================================
# 7. TEST ESTADÍSTICO: ANOVA
# ============================================================================

cat("========== TEST ESTADÍSTICO: ANOVA ==========\n\n")

anova_result <- aov(dap_porcentaje ~ facultad, data = df_plot)
anova_summary <- summary(anova_result)
print(anova_summary)

p_value <- anova_summary[[1]]$`Pr(>F)`[1]
cat("\n")
if (p_value < 0.001) {
  cat("✓ RESULTADO: Hay diferencias MUY SIGNIFICATIVAS en DAP entre facultades\n")
  cat("  (p < 0.001 ***)\n")
} else if (p_value < 0.01) {
  cat("✓ RESULTADO: Hay diferencias SIGNIFICATIVAS en DAP entre facultades\n")
  cat("  (p < 0.01 **)\n")
} else if (p_value < 0.05) {
  cat("✓ RESULTADO: Hay diferencias SIGNIFICATIVAS en DAP entre facultades\n")
  cat("  (p < 0.05 *)\n")
} else {
  cat("✗ RESULTADO: NO hay diferencias significativas en DAP entre facultades\n")
  cat("  (p =", round(p_value, 3), ")\n")
}

cat("\nTamaño del efecto (η²):\n")
ss_between <- anova_summary[[1]]$`Sum Sq`[1]
ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
eta_sq <- ss_between / ss_total
cat("η² =", round(eta_sq, 4), 
    ifelse(eta_sq > 0.14, "(GRANDE)", 
           ifelse(eta_sq > 0.06, "(MEDIO)", "(pequeño)")), "\n\n")

# ============================================================================
# 8. TEST POST-HOC: TUKEY HSD (si ANOVA es significativo)
# ============================================================================

if (p_value < 0.05) {
  cat("========== TEST POST-HOC: Tukey HSD ==========\n")
  cat("(Comparaciones pareadas de todas las facultades)\n\n")
  
  tukey_result <- TukeyHSD(anova_result)
  
  # Extraer y mostrar solo pares significativos
  tukey_df <- as.data.frame(tukey_result$facultad)
  tukey_df$comparacion <- rownames(tukey_df)
  tukey_df <- tukey_df %>%
    filter(`p adj` < 0.05) %>%
    arrange(`p adj`)
  
  if (nrow(tukey_df) > 0) {
    cat("Pares de FACULTADES CON DIFERENCIAS SIGNIFICATIVAS:\n\n")
    print(tukey_df %>% select(comparacion, diff, `p adj`) %>%
            arrange(`p adj`))
  } else {
    cat("No hay diferencias significativas entre pares de facultades.\n")
  }
  
  cat("\nNota: El test Tukey HSD completo está disponible ejecutando:\n")
  cat("  tukey_result <- TukeyHSD(anova_result)\n")
  cat("  plot(tukey_result)\n\n")
}

# ============================================================================
# 9. TABLA RESUMEN PARA EXPORTAR
# ============================================================================

cat("========== TABLA RESUMEN (ordenada por DAP promedio) ==========\n\n")

tabla_final <- stats_facultad %>%
  select(facultad, n, Media, DE, Mediana, Q1, Q3) %>%
  rename(
    Facultad = facultad,
    N = n,
    "DAP Media" = Media,
    "DE" = DE,
    "DAP Mediana" = Mediana,
    "Q1 (25%)" = Q1,
    "Q3 (75%)" = Q3
  )

print(tabla_final)

# Guardar tabla en CSV
write_csv(tabla_final, here("output", "05-dap_estadisticas_facultad.csv"))
cat("\n✓ Tabla guardada: 05-dap_estadisticas_facultad.csv\n\n")

# ============================================================================
# 10. RESUMEN FINAL
# ============================================================================

cat("========== RESUMEN DE ANÁLISIS ==========\n")
cat("Observaciones analizadas:", nrow(df_plot), "\n")
cat("Facultades:", n_distinct(df_plot$facultad), "\n")
cat("DAP promedio global:", round(mean(df_plot$dap_porcentaje, na.rm = TRUE), 2), "%\n")
cat("DAP rango:", 
    round(min(df_plot$dap_porcentaje, na.rm = TRUE), 1), "-",
    round(max(df_plot$dap_porcentaje, na.rm = TRUE), 1), "%\n\n")

cat("Facultad CON MAYOR DAP:", 
    stats_facultad$facultad[1], 
    "(",
    stats_facultad$Media[1], 
    "%)\n")
cat("Facultad CON MENOR DAP:", 
    stats_facultad$facultad[nrow(stats_facultad)], 
    "(",
    stats_facultad$Media[nrow(stats_facultad)], 
    "%)\n\n")

cat("GRÁFICOS GENERADOS:\n")
cat("  ✓ 05-dap_por_facultad_boxplot.png\n")
cat("  ✓ 05-dap_por_facultad_violin.png\n")
cat("  ✓ 05-dap_facultad_medias_ci.png\n")
cat("  ✓ 05-dap_estadisticas_facultad.csv\n")
cat("\n========================================\n")