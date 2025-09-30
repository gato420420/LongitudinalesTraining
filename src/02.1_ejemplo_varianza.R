# ==========================================================
# Análisis de Encuestas de Hogares con R
# Autores: Andrés Gutiérrez y Stalyn Guerrero
# Contacto: andres.gutierrez@cepal.org
# Descripción: Procesamiento de datos longitudinales de 
# encuestas de hogares con diseño rotativo. Incluye ajuste
# anual de ponderadores, calibración de pesos y estimación
# de totales poblacionales y de subpoblaciones (desocupados).
# ==========================================================

# ==========================================================
# 1. CONFIGURACIÓN INICIAL Y CARGA DE PAQUETES
# ==========================================================

# Instalar paquetes necesarios si no están instalados
# install.packages(c("haven", "tidyverse", "survey",
# "srvyr", "printr", "openxlsx", "fastDummies"))

# Cargar librerías
library(haven)       # Importar datos desde formatos SPSS, Stata, SAS
library(tidyverse)   # Manipulación de datos y visualización
library(survey)      # Análisis de encuestas con diseño complejo
library(srvyr)       # Interfaz tipo tidy para survey
library(printr)      # Presentación de tablas en consola
library(openxlsx)    # Lectura de archivos Excel
library(fastDummies) # Crear variables dummy rápidamente

# Opciones globales
options(scipen = 999) # Evitar notación científica en salidas

# ==========================================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# ==========================================================

# Leer base de datos anual (ya en formato RDS)
encuesta_anual <- readRDS("Data/base_anual.rds")

# ==========================================================
# 3. ANÁLISIS DESCRIPTIVO INICIAL
# ==========================================================

## 3.1 Conteo de UPMs (Unidades Primarias de Muestreo) por trimestre y mes
upm_mes <- encuesta_anual %>% 
  distinct(trimestre, mes_trimestre, upm)

conteo_upm <- upm_mes %>% 
  group_by(trimestre, mes_trimestre) %>%
  count(name = "n_upm") %>% 
  pivot_wider(
    names_from = mes_trimestre,
    values_from = n_upm,
    values_fill = 0
  ) %>% 
  mutate(Total = M1 + M2 + M3)  # Total de UPMs por trimestre

print(conteo_upm)

## 3.2 Identificación de traslapes entre trimestres
# Se detectan UPMs que aparecen en más de un trimestre
traslape_trimestres <- upm_mes %>%
  semi_join(upm_mes %>% count(upm) %>% filter(n > 1), by = "upm") %>%
  group_by(upm) %>%
  summarise(
    trimestres = list(sort(unique(trimestre))),
    .groups = "drop"
  ) %>%
  filter(lengths(trimestres) > 1) %>%  
  mutate(
    pares_traslape = map(trimestres, ~ combn(.x, 2, simplify = FALSE))
  ) %>%
  select(upm, pares_traslape) %>%
  unnest(pares_traslape) %>%
  mutate(
    trimestre_1 = map_chr(pares_traslape, 1),
    trimestre_2 = map_chr(pares_traslape, 2)
  ) %>%
  count(trimestre_1, trimestre_2, name = "traslape") %>%
  arrange(trimestre_1)

print(traslape_trimestres)

## 3.3 Ajuste anual de ponderadores
# Calcula factor de ajuste para que los ponderadores trimestrales sumen correctamente a nivel anual
ponderador_anos <- encuesta_anual %>% 
  group_by(trimestre, mes_trimestre) %>%
  summarise(num = sum(fep), .groups = "drop") %>% 
  mutate(
    den = sum(num),
    bi = num / den  # Factor de ajuste anual
  )

encuesta_anual_ajus2 <- encuesta_anual %>%
  inner_join(ponderador_anos %>% select(-num, -den), 
             by = c("trimestre", "mes_trimestre")) %>% 
    mutate(dk_anual = fep * bi)  # Ponderador ajustado anual

# ==========================================================
# 5. CALIBRACIÓN DE PESOS MUESTRALES
# ==========================================================

## 5.1 Preparación de datos para calibración

# Leer población de referencia por estrato
poblacion <- read.xlsx("Imagenes/15_cap/muestra_anual.xlsx") %>%
  transmute(
    estrato = paste0("hh_", dam, "_", area), 
    N_personas
  )

# Crear vector de totales poblacionales
total_pob <- setNames(round(poblacion$N_personas), poblacion$estrato)

# Crear fórmula de calibración para survey::calibrate
formula_calb <- as.formula(
  paste0("~ 0 + ", paste0(names(total_pob), collapse = " + "))
)

# Conteo de personas por hogar
conte_persona_hh <- encuesta_anual_ajus2 %>% 
  group_by(trimestre, mes_trimestre, estrato, dam, upm, id_hogar) %>% 
  count(name = "n_personas")

# Preparar base de hogares para calibración
base_hog <- encuesta_anual_ajus2 %>%
  filter(id_pers == 1) %>%  # Tomar solo un representante por hogar
  distinct(trimestre, mes_trimestre, estrato, dam, upm, id_hogar, dk_anual) %>%
  mutate(estrato_hh = estrato) %>%
  fastDummies::dummy_cols(
    select_columns = "estrato_hh",
    remove_selected_columns = TRUE
  ) %>%
  rename_with(~ gsub("^estrato_", "", .x), .cols = starts_with("estrato_hh"))

# Unir conteo de personas
base_hog <- inner_join(
  conte_persona_hh,
  base_hog,
  by = join_by(trimestre, mes_trimestre, estrato, dam, upm, id_hogar)
)

# Ajustar variables dummy multiplicando por número de personas
base_hog <- base_hog %>% mutate(across(matches("hh_"), ~.*n_personas))

# ==========================================================
# 5.2 Diseño muestral y calibración
# ==========================================================

design <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~dk_anual,
  data = base_hog,
  nest = TRUE
)

summary(design)

# Calibración de pesos
design_calib <- calibrate(
  design = design,
  formula = formula_calb,
  population = total_pob
) %>% 
  as_survey()

summary(weights(design_calib))
sum(weights(design_calib) <1)
hist(weights(design_calib), breaks = 100)

# Validación: comparar totales por estrato vs población de referencia
design_calib %>% group_by(estrato) %>%
  summarise(total = survey_total(n_personas)) %>%
  mutate(estrato = paste0("hh_", estrato)) %>%
  inner_join(data.frame(total_pob) %>% tibble::rownames_to_column(var = "estrato")) %>%
  mutate(dif = abs(total - total_pob))

# ==========================================================
# 5.3 Unir pesos calibrados a la base completa
# ==========================================================

base_hog_calib <- design_calib$variables %>% 
  transmute(
    trimestre, mes_trimestre, upm, id_hogar, 
    dk_anual_calib = weights(design_calib)
  ) 

encuesta_anual_ajus_calib <- inner_join(
  encuesta_anual_ajus2, 
  base_hog_calib,
  by = c("trimestre", "mes_trimestre", "upm", "id_hogar")
) %>% 
filter(!is.na(trabajo)) %>%   
  mutate(
    y = case_when(trabajo == "Desocupado" ~ 1,
                  TRUE ~ 0), 
    trim_mes = paste0(trimestre, "_", mes_trimestre)
  )

# ==========================================================
# 5.4 Diseño muestral con pesos calibrados
# ==========================================================

design_pers_calib <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~dk_anual_calib,
  data = encuesta_anual_ajus_calib,
  nest = TRUE
) 

# Estimación de totales por mes

design_pers_calib  %>% as_survey() %>% 
  group_by(trim_mes) %>%
  summarise(total = survey_total(y))


totales_mensuales <-
  svytotal( ~ interaction(trim_mes, y), design_pers_calib)
names(totales_mensuales) <-
  gsub("interaction\\(trim_mes, y\\)", "", names(totales_mensuales))
totales_mensuales

# Matriz de varianzas y covarianzas
vc <- vcov(totales_mensuales)
colnames(vc) <- gsub("interaction\\(trim_mes, y\\)", "", colnames(vc))
rownames(vc) <- colnames(vc)

# Seleccionar solo desocupados (y=1)
indices_y1 <- grep("\\.1$", colnames(vc))
desocupados_vc <- vc[indices_y1, indices_y1]
sum(diag(desocupados_vc))

# Varianza total agregada y desviación estándar
var_total_agregado <- sum(desocupados_vc)
sqrt(var_total_agregado)

sum(desocupados_vc) - sum(diag(desocupados_vc))
############################################################
# Estimación de totales
totales <-
  svytotal( ~ y , design_pers_calib)


############################################################
## Estimación de la proporción de desocupados 
############################################################
medias_mensuales <-
  svymean( ~ interaction(trim_mes, y), design_pers_calib)
names(medias_mensuales) <-
  gsub("interaction\\(trim_mes, y\\)", "", names(totales_mensuales))
medias_mensuales

delta <- 0.024559 - 0.024850

vc_mean <- vcov(medias_mensuales)
colnames(vc_mean) <- gsub("interaction\\(trim_mes, y\\)", "", colnames(vc_mean))
rownames(vc_mean) <- colnames(vc_mean)

vc_mean[c("T1_M1.1", "T1_M2.1"), c("T1_M1.1", "T1_M2.1")]
SE_delta <- sqrt(0.000012660118  + 0.000010734555 - 
                   2*(-0.000001931855))
delta

