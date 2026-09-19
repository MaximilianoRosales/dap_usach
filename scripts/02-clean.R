# ============================================================================
# SCRIPT 02: LIMPIAR Y PREPARAR DATOS
# ============================================================================
# Propósito: Renombrar variables, codificar escalas, crear índices
# Versión: 2.0 (corregida y mejorada)
# Fecha: 2024
# ============================================================================

library(tidyverse)
library(here)

# ============================================================================
# 1. CARGAR DATOS IMPORTADOS
# ============================================================================

df_raw <- readRDS(here("data", "01_raw_imported.rds"))

cat("Datos cargados:\n")
cat("- Observaciones:", nrow(df_raw), "\n")
cat("- Variables:", ncol(df_raw), "\n\n")

# ============================================================================
# 2. RENOMBRAR VARIABLES
# ============================================================================
# NOTA: Los nombres en Google Form son muy largos. 
# Este mapeo es sensible a cambios mínimos en los textos.

df_clean <- df_raw %>%
  rename(
    #seccion 1: vinculo con la universidad
    vinculo = '¿Cuál es su relación principal con la Universidad De Santiago de Chile?',
    facultad = '¿En qué facultad o unidad académica estudia o trabaja?',
    anio_car = '(en el caso de ser Estudiante) ¿Cuántos años de universidad lleva?',
    freq_campus = '¿Con qué frecuencia compra almuerzo o alimentos dentro del campus?',
    
    #seccion 2: habitos de compra alimentaria
    freq_org = '¿Con qué frecuencia compra alimentos orgánicos actualmente?',
    lugar_compra = '¿Dónde compra sus alimentos habitualmente? (puede marcar más de una opción)',
    decisor = '¿Quién decide principalmente qué alimentos se compran en su hogar? (en general, no solo los orgánicos)',
    gasto_sem = 'Aproximadamente ¿cuánto gasta usted semanalmente en su propia alimentación? (incluye casino, cafeterías, compras y delivery)',
    
    #seccion 3A: Conciencia de Salud
    hc1 = 'Cuando compro alimentos, evalúo con atención sus posibles efectos en la salud',
    hc2_inv = 'La información sobre salud NO es importante en mis decisiones de compra',
    hc3 = 'Considero que los alimentos orgánicos son más saludables que los productos convencionales',
    hc4_inv = 'Los alimentos convencionales son igual de seguros que los orgánicos',
    
    #seccion 3B: conciencia ambiental
    ec1 = 'El consumo de alimentos orgánicos contribuye a la protección del medioambiente',
    ec2_inv = 'La producción convencional de alimentos (con químicos y pesticidas) ofrece beneficios ambientales similares a la producción orgánica',
    ec3 = 'El uso de pesticidas sintéticos en la agricultura dañan el medioambiente',
    ec4_inv= 'El impacto ambiental no influye en mis decisiones de compra',
    
    #seccion 3C: sensibilidad al precio, barreras y control conductual
    pc1 = 'El precio de los alimentos orgánicos es una barrera importante para comprarlos',
    pc2 = 'Tengo fácil acceso a alimentos orgánicos cerca de donde estudio',
    pc3 = 'Confío en los sellos y certificaciones de alimentos orgánicos disponibles en Chile',
    pc4 = 'Puedo comprar alimentos orgánicos cuando quiero hacerlo',
    
    #sección 3D: importancia de atributos
    atr_precio = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Precio]',
    atr_sabor = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Sabor]',
    atr_salud = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Beneficios para la salud]',
    atr_pest = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Ausencia de pesticidas]',
    atr_origen = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Producción nacional]',
    atr_cert = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Certificación o sello de calidad]',
    atr_acceso = '¿Cuál es la importancia para usted los siguientes atributos al momento de comprar alimentos? (no solo orgánicos)\n1. Ninguna Importancia\n2. Muy poca importancia\n3. Poca Importancia\n4. Importante\n5. Mucha Importancia\n6. Máxima Importancia [Facilidad de encontrar]',
    
    #seccion 4: dispocision a pagar
    dap = 'En esta situación, ¿Cuánto más pagarías (porcentaje extra) por la versión orgánica por sobre la convencional? (ejemplo:  x% extra)',
    cert_dap = '¿Qué tan seguro/a estás de tu respuesta anterior?',
    no_dap = '¿Cuáles son las razones principales por las que decidirías no pagar más por alimentos orgánicos?',
    
    #seccion 5: perfil sociodemografico
    sexo = '¿Con qué identidad de género se identifica?',
    edad = '¿Cuántos años tiene?',
    hhsize = '¿Cuántas personas viven en su hogar, incluyendo a usted?',
    under_15 = '¿Hay niños menores de 15 años en su hogar?',
    income = '¿Cuánto es el ingreso aproximado mensual de su hogar? (sumando todos los que trabajan en casa, incluyendo sueldos, pensiones y/u otros ingresos)',
    junaeb = '¿Cuenta con algún tipo de beneficio estudiantil?',
    educ_sos = '¿Cuál es el nivel educativo más alto completado por el principal sostenedor económico de su hogar?',
  )  

cat("✓ Variables renombradas\n")
glimpse(df_clean)

# ============================================================================
# 3. CONVERTIR ESCALAS LIKERT A NUMÉRICO (1-6)
# ============================================================================
# Google Form devuelve valores como "4 = De acuerdo"
# Extraemos SOLO el número del inicio

df_clean <- df_clean %>%
  mutate(
    # Items Conciencia de Salud (1-6)
    hc1 = as.numeric(str_extract(hc1, "^\\d")),
    hc2_inv = as.numeric(str_extract(hc2_inv, "^\\d")),
    hc3 = as.numeric(str_extract(hc3, "^\\d")),
    hc4_inv = as.numeric(str_extract(hc4_inv, "^\\d")),
    
    # Items Conciencia Ambiental (1-6)
    ec1 = as.numeric(str_extract(ec1, "^\\d")),
    ec2_inv = as.numeric(str_extract(ec2_inv, "^\\d")),
    ec3 = as.numeric(str_extract(ec3, "^\\d")),
    ec4_inv = as.numeric(str_extract(ec4_inv, "^\\d")),
    
    # Items Sensibilidad precio (1-6)
    pc1 = as.numeric(str_extract(pc1, "^\\d")),
    pc2 = as.numeric(str_extract(pc2, "^\\d")),
    pc3 = as.numeric(str_extract(pc3, "^\\d")),
    pc4 = as.numeric(str_extract(pc4, "^\\d")),
    
    # Items Atributos (1-6)
    across(starts_with("atr_"),
           ~as.numeric(str_extract(., "^\\d")))
  )

cat("\n✓ Escalas Likert convertidas a numérico (1-6)\n")

# Validar conversión
cat("\nValidación - Rango de valores Likert:\n")
cat("hc1 (Conciencia Salud):", min(df_clean$hc1, na.rm = T), "-", max(df_clean$hc1, na.rm = T), "\n")
cat("ec1 (Conciencia Ambiental):", min(df_clean$ec1, na.rm = T), "-", max(df_clean$ec1, na.rm = T), "\n")
cat("pc1 (Control Percibido):", min(df_clean$pc1, na.rm = T), "-", max(df_clean$pc1, na.rm = T), "\n")

# ============================================================================
# 4. INVERTIR ITEMS INVERSOS (escala 1-6, inversión: 7 - valor)
# ============================================================================

df_clean <- df_clean %>%
  mutate(
    # Invertir items de salud consciencia
    hc2 = 7 - hc2_inv,
    hc4 = 7 - hc4_inv,
    
    # Invertir items de consciencia ambiental
    ec2 = 7 - ec2_inv,
    ec4 = 7 - ec4_inv,
    # NOTA: pc1 ("El precio es factor más importante") va INVERTIDO
    # en el contexto de control percibido
    pc1_inv = 7 - pc1
  )

cat("\n✓ Items inversos invertidos correctamente (7 - valor)\n")

# ============================================================================
# 5. CREAR VARIABLE BINARIA Y NUMÉRICA DE DAP (TARJETA DE PAGO)
# ============================================================================

df_clean <- df_clean %>%
  mutate(
    # Mapear opciones de tarjeta de pago a números
    dap_porcentaje = case_when(
      dap == "0%" ~ 0,                                    
      dap == "5%" ~ 5,
      dap == "10%" ~ 10,
      dap == "20%" ~ 20,
      dap == "30%" ~ 30,
      dap == "40%" ~ 40,
      dap == "50%" ~ 50,
      dap == "60%" ~ 60,
      dap == "70%" ~ 70,
      dap == "80%" ~ 80,
      dap == "90%" ~ 90,
      dap == "100%" ~ 100,
      dap == "Otra, por favor especifique" ~ as.numeric(no_dap),
      dap == "No lo sé" ~ NA_real_,
      is.na(dap) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Variable binaria (¿está dispuesto a pagar algo más?)
    tiene_dap = case_when(
      dap_porcentaje > 0 ~ 1,
      dap_porcentaje == 0 ~ 0,
      is.na(dap_porcentaje) ~ NA_real_
    ),
    tiene_dap = factor(tiene_dap, levels = c(0, 1), labels = c("No", "Sí"))
  )

cat("\n✓ Variable DAP procesada\n")
cat("Distribución de DAP:\n")
print(table(df_clean$tiene_dap, useNA = "ifany"))
cat("\n")

# ============================================================================
# 6. CODIFICAR VARIABLES CATEGÓRICAS COMO FACTORES
# ============================================================================

df_clean <- df_clean %>%
  mutate(
    sexo = factor(sexo),
    vinculo = factor(vinculo),
    facultad = factor(facultad),
    freq_campus = factor(freq_campus),
    freq_org = factor(freq_org),
    decisor = factor(decisor),
    under_15 = factor(under_15),
    junaeb = factor(junaeb),
    educ_sos = factor(educ_sos)
  )

cat("✓ Variables categóricas convertidas a factores\n")

# ============================================================================
# 7. CONVERTIR VARIABLES NUMÉRICAS
# ============================================================================

df_clean <- df_clean %>%
  mutate(
    edad = as.numeric(edad),
    anio_car = as.numeric(str_extract(anio_car, "\\d")),
    gasto_sem = as.numeric(str_extract(gasto_sem, "\\d+")),
    hhsize = as.numeric(hhsize),
    cert_dap = as.numeric(str_extract(cert_dap, "^\\d"))
  )

cat("✓ Variables numéricas convertidas\n")

# ============================================================================
# 8. MANEJAR VALORES FALTANTES
# ============================================================================

cat("\nValores faltantes ANTES de filtrado:\n")
print(colSums(is.na(df_clean)))

# Crear variable para tracking
df_clean <- df_clean %>%
  mutate(
    completitud = rowSums(!is.na(select(., starts_with(c("hc", "ec", "pc", "atr")))))
  )

# Eliminar filas con MUCHOS missings en items Likert
# (mantener si tienen al menos el 50% de los ítems)
df_clean_filtered <- df_clean %>%
  filter(completitud >= 8)  # 16 ítems total, al menos 8

cat("\nObservaciones filtradas por completitud:\n")
cat("Antes:", nrow(df_clean), "\n")
cat("Después:", nrow(df_clean_filtered), "\n")
cat("Eliminadas:", nrow(df_clean) - nrow(df_clean_filtered), "\n")

# Usar datos filtrados
df_clean <- df_clean_filtered %>%
  select(-completitud)  # Eliminar variable auxiliar

# ============================================================================
# 9. GUARDAR DATOS LIMPIOS
# ============================================================================

saveRDS(df_clean, here("data", "02_clean.rds"))
write_csv(df_clean, here("data", "02_clean.csv"))

cat("\n✓ Datos limpios guardados:\n")
cat("  - data/02_clean.rds (para R)\n")
cat("  - data/02_clean.csv (para Excel/inspección)\n")

# ============================================================================
# 10. RESUMEN FINAL DE LIMPIEZA
# ============================================================================

cat("\n")
cat("================== RESUMEN DE LIMPIEZA ==================\n")
cat("Observaciones finales:", nrow(df_clean), "\n")
cat("Variables finales:", ncol(df_clean), "\n")
cat("=========================================================\n\n")

cat("DISPOSICIÓN A PAGAR (DAP):\n")
print(table(df_clean$tiene_dap, useNA = "ifany"))

cat("\nESTADÍSTICAS DE DAP (porcentaje):\n")
print(summary(df_clean$dap_porcentaje))

cat("\nDEMOGRÁFICAS:\n")
cat("Edad - Media:", round(mean(df_clean$edad, na.rm = T), 1), 
    "| DE:", round(sd(df_clean$edad, na.rm = T), 1), "\n")
cat("Tamaño hogar - Media:", round(mean(df_clean$hhsize, na.rm = T), 1), "\n")

cat("\nÍTEMS LIKERT - Rango esperado (1-6):\n")
items_likert <- df_clean %>%
  select(hc1, hc2, hc3, hc4, ec1, ec2, ec3, ec4, pc2, pc3, pc4, starts_with("atr_")) %>%
  pivot_longer(everything(), names_to = "item", values_to = "valor")

cat("Mínimo:", min(items_likert$valor, na.rm = T), "\n")
cat("Máximo:", max(items_likert$valor, na.rm = T), "\n")
cat("Media:", round(mean(items_likert$valor, na.rm = T), 2), "\n")

cat("\nPRÓXIMO PASO:\n")
cat("Ejecutar: 03-analysis.R\n")
cat("- Validar confiabilidad (Cronbach's alpha) por constructo\n")
cat("- Análisis Factorial Exploratorio (EFA)\n")
cat("- LUEGO crear índices de constructos\n")