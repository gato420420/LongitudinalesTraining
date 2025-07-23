# =============================================
# ANÁLISIS LONGITUDINAL DE ENCUESTAS ROTATIVAS
# =============================================
# Script desarrollado para el procesamiento de datos longitudinales
# de encuestas de hogares con diseño rotativo
# Basado en metodologías de Feinberg & Stasny (1983), Verma et al. (2006)
# y Gutiérrez (2014)

# Autor: Andrés Gutiérrez y Stalyn Guerrero 
# Contacto: andres.gutierrez@cepal.org

# =============================================
# 1. CONFIGURACIÓN INICIAL
# =============================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación y visualización de datos
library(survey)       # Análisis de encuestas complejas
library(srvyr)        # Versión tidyverse de survey
library(tidyr)        # Manejo de datos en formato largo/ancho
# remotes::install_github("guilhermejacob/surf") # Instalar si es necesario
library(surf)         # Estimación de flujos brutos

# =============================================
# 2. CARGA DE DATOS
# =============================================

# Definir ruta de entrada (ajustar según estructura de directorios)
input <- "data/"

# Cargar base de datos a nivel de personas
base_personas_trim <- readRDS(file.path(input, "base_personas_trim.rds")) 

# =============================================
# 3. DISEÑO MUESTRAL COMPLEJO
# =============================================

# 3.1 Crear diseño muestral con pesos calibrado
design_cali <- svydesign(
  ids = ~upm,         # Unidades primarias de muestreo
  strata = ~estrato,  # Estratificación
  data = base_personas_trim,
  weights = ~fex_cali)  %>% 
  as_survey()

# =============================================
# 4. ANÁLISIS DE FLUJOS BRUTOS
# =============================================

# 4.1 Conteo de transiciones de pobreza
base_personas_trim %>% 
  count(pobreza_t1, pobreza_t2, name = "n") %>%
  complete(pobreza_t1 = as.character(1:3),  
           pobreza_t2 = as.character(1:3),  
           fill = list(n = 0)) %>%
  mutate(pobreza_t1 = case_when(  
    pobreza_t1 == 1 ~ "Pobreza extrema",  
    pobreza_t1 == 2 ~ "Pobreza no extrema",  
    pobreza_t1 == 3 ~ "Fuera de la pobreza"),  
    pobreza_t2 = case_when(  
      pobreza_t2 == 1 ~ "Pobreza extrema",  
      pobreza_t2 == 2 ~ "Pobreza no extrema",  
      pobreza_t2 == 3 ~ "Fuera de la pobreza")) %>%
  pivot_wider(names_from = pobreza_t2, values_from = n, values_fill = 0)

# 4.2 Estimación poblacional de transiciones
design_cali %>% 
  group_by(pobreza_t1, pobreza_t2) %>%
  summarise(tol_pob = survey_total()) %>%
  mutate(total = paste0(round(tol_pob), " (", round(tol_pob_se), ")"),  
         tol_pob = NULL, tol_pob_se = NULL) %>%
  mutate(pobreza_t1 = case_when(  
    pobreza_t1 == 1 ~ "Pobreza extrema",  
    pobreza_t1 == 2 ~ "Pobreza no extrema",  
    pobreza_t1 == 3 ~ "Fuera de la pobreza"),  
    pobreza_t2 = case_when(  
      pobreza_t2 == 1 ~ "Pobreza extrema",  
      pobreza_t2 == 2 ~ "Pobreza no extrema",  
      pobreza_t2 == 3 ~ "Fuera de la pobreza")) %>%
  ungroup() %>%
  pivot_wider(names_from = pobreza_t2,  
              values_from = total, values_fill = "0 (0)")

# =============================================
# 5. MODELOS DE MARKOV PARA TRANSICIONES
# =============================================

# 5.1 Preparar factores para modelo
design_cali <- design_cali %>% 
  mutate(
    pobreza_t1 = factor(
      pobreza_t1,
      levels = c(1, 2, 3),
      labels = c("Pobreza extrema", "Pobreza no extrema", "Fuera de la pobreza")),
    pobreza_t2 = factor(
      pobreza_t2,
      levels = c(1, 2, 3),
      labels = c("Pobreza extrema", "Pobreza no extrema", "Fuera de la pobreza"))
  )

# 5.2 Ajustar Modelo C (transiciones dependen del estado inicial)
model_C <- svyflow(~ pobreza_t1 + pobreza_t2, 
                   model = "C",
                   design = design_cali,
                   as.zero.flows = TRUE)

# 5.3 Resultados del modelo
# Probabilidades iniciales (eta)
coef(model_C$eta)

# Probabilidades finales (gamma)
coef(model_C$gamma)

# Matriz de transición (pij)
coef(model_C$pij)

# =============================================
# 6. ANÁLISIS POR SEXO
# =============================================

# 6.1 Modelo para hombres
model_C_H <- svyflow(~ pobreza_t1 + pobreza_t2, 
                     model = "C",
                     design = design_cali %>% filter(sexo == "Hombre"),
                     as.zero.flows = TRUE)

# Resultados para hombres
coef(model_C_H$eta)
coef(model_C_H$gamma)
coef(model_C_H$pij)

# 6.2 Modelo para mujeres
model_C_M <- svyflow(~ pobreza_t1 + pobreza_t2, 
                     model = "C",
                     design = design_cali %>% filter(sexo == "Mujer"),
                     as.zero.flows = TRUE)

# Resultados para mujeres
coef(model_C_M$eta)
coef(model_C_M$gamma)
coef(model_C_M$pij)

# =============================================
# FIN DEL SCRIPT
# =============================================
