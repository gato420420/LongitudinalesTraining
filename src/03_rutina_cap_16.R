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
library(pROC)         # Curvas ROC para modelos de respuesta
library(printr)       # Mejor presentación de tablas
library(tidyr)        # Manejo de datos en formato largo/ancho
# remotes::install_github("guilhermejacob/surf") # Instalar si es necesario
library(surf)         # Estimación de flujos brutos

# =============================================
# 2. CARGA DE DATOS
# =============================================

# Definir ruta de entrada (ajustar según estructura de directorios)
input <- "data/"

# Cargar base de datos a nivel de personas
base_personas <- readRDS(file.path(input, "base_anual.rds")) %>%  
  filter(trimestre %in% c("T1", "T2")) %>% 
  ungroup() # Eliminar agrupamientos previos

# Crear base a nivel de hogares (una observación por hogar-trimestre)
base_hogares <- base_personas %>%  
  distinct(upm, trimestre, id_hogar, fep) # fep = factor de expansión

# Visualización preliminar
head(base_hogares, 10)

# =============================================
# 3. ESTADÍSTICAS DESCRIPTIVAS
# =============================================

# 3.1 Número de hogares por trimestre
base_hogares %>% 
  group_by(trimestre) %>%  
  tally(name = "hogares")

# 3.2 Número de UPMs únicas por trimestre
base_hogares %>% 
  distinct(trimestre, upm) %>%  
  group_by(trimestre) %>% 
  tally(name = "upm")

# =============================================
# 4. IDENTIFICACIÓN DE TRASLAPES
# =============================================

# 4.1 Hogares presentes en ambos trimestres (T1 y T2)
hogares_ambos <- base_hogares %>%  
  group_by(id_hogar) %>%  
  count() %>% 
  filter(n == 2) %>% # Aparecen en dos trimestres  
  pull(id_hogar)

# 4.2 Extraer pesos del trimestre 1 para comparación
base_t1 <- base_hogares %>%  
  filter(trimestre == "T1") %>% 
  select(id_hogar, fep_t1 = fep)

# Número total de hogares con traslape
length(hogares_ambos)

# =============================================
# 5. CREACIÓN DE BASE LONGITUDINAL
# =============================================

# 5.1 Crear variable de respuesta en ambos trimestres
base_t1_t2 <- base_hogares %>%
  mutate(respboth = if_else(id_hogar %in% hogares_ambos, 1, 0)) %>%
  inner_join(base_t1, by = "id_hogar")

# 5.2 Asignar peso longitudinal básico (probabilidad de panel = 2/4)
prob_panel <- 2/4  
base_t1_t2 <- base_t1_t2 %>%  
  mutate(fep_long = ifelse(respboth == 1, fep_t1/prob_panel, 0))

# Verificar suma de pesos
sum(base_t1_t2$fep_long) ## Total hogares

base_t1_t2 %>% 
  inner_join(base_personas %>%
               select(trimestre, id_hogar, id_pers)) %>%
  summarise(pob_estimada = sum(fep_long))


# Comparar con población estimada por trimestre
base_personas %>% 
  distinct(trimestre, id_hogar, fep) %>% 
  group_by(trimestre) %>%  
  summarise(Hog_estimada = sum(fep))

base_personas %>% 
  group_by(trimestre) %>%  
  summarise(pob_estimada = sum(fep))


# =============================================
# 6. IDENTIFICACIÓN DE PERSONAS EN AMBOS TRIMESTRES
# =============================================

# 6.1 Filtrar personas en hogares con traslape
base_personas_t1_t2 <- base_personas %>%
  filter(id_hogar %in% hogares_ambos) %>%
  mutate(id_llave = paste0(id_hogar, id_pers))

# 6.2 Identificar personas con respuesta en ambos trimestres
hogares_personas <- base_personas_t1_t2 %>%
  group_by(id_llave) %>% 
  count() %>%
  filter(n == 2) %>% 
  pull(id_llave)

# 6.3 Marcar personas con respuesta completa
base_personas_t1_t2 <- base_personas_t1_t2 %>%
  mutate(respboth_per = if_else(id_llave %in% hogares_personas, 1, 0))

# 6.4 Unir con información de hogares
base_personas_t1_t2 <- base_personas_t1_t2 %>%
  inner_join(base_t1_t2 %>%
               select(id_hogar, fep_long, trimestre),
             by = c("id_hogar","trimestre"))

# =============================================
# 7. AJUSTE POR NO RESPUESTA
# =============================================

# 7.1 Modelo logístico de probabilidad de respuesta
modelo_logit <- glm(respboth_per ~ sexo + ingreso,
                    data = base_personas_t1_t2,
                    family = binomial(link = "logit"))
summary(modelo_logit)
# 7.2 Predecir probabilidades de respuesta
base_personas_t1_t2$prob_resp <- predict(modelo_logit, type = "response")

# 7.3 Evaluar modelo con curva ROC
roc_obj <- roc(base_personas_t1_t2$respboth_per,  
               base_personas_t1_t2$prob_resp)
plot(roc_obj, main = paste("Curva ROC - AUC:", round(auc(roc_obj), 3)))

# 7.4 Ajustar pesos longitudinales
base_personas_t1_t2 <- base_personas_t1_t2 %>%  
  mutate(fep_aj = fep_long / prob_resp)

head(base_personas_t1_t2 %>% 
       select(id_hogar, id_pers ,
              trimestre, fep_long,
              fep_aj))
# =============================================
# 8. PREPARACIÓN PARA ANÁLISIS LONGITUDINAL
# =============================================

poblacion <- openxlsx::read.xlsx("Imagenes/15_cap/muestra_anual.xlsx") %>%
  transmute(estrato = paste0("pp_", dam, "_", area), N_personas)

# 8.1 Definir totales poblacionales conocidos para calibración
total_pob <- setNames(round(poblacion$N_personas),
                      paste0(poblacion$estrato))

fomula_calb <-
  as.formula(paste0("~ 0 + ", paste0(names(total_pob), collapse = " + ")))
fomula_calb

# 8.2 Organizar base longitudinal
base_personas_ambos <-  base_personas_t1_t2 %>% filter( respboth_per == 1)
temp <- base_personas_ambos %>%
  distinct(upm, estrato, id_hogar, id_pers, id_llave, 
           edad, sexo, area, fep_aj)

temp_pobreza <- base_personas_ambos %>%
  select(id_llave, trimestre, pobreza) %>%
  pivot_wider(names_from = trimestre, values_from = pobreza) %>%
  rename(pobreza_t1 = 'T1', pobreza_t2 = 'T2')

table(temp_pobreza$pobreza_t1, temp_pobreza$pobreza_t2)

temp  <- temp %>% mutate(estrato_pp = estrato) %>%
  fastDummies::dummy_columns(select_columns = "estrato_pp",
                             remove_selected_columns = TRUE) %>%
  rename_with(.fn = ~ gsub("^estrato_", "", .x),
              .cols = starts_with("estrato_pp"))

base_personas_trim <- inner_join(temp, temp_pobreza, by = "id_llave") 

# =============================================
# 9. DISEÑO MUESTRAL COMPLEJO
# =============================================

# 9.1 Crear diseño muestral con pesos ajustados
design_long <- svydesign(
  ids = ~upm,         # Unidades primarias de muestreo
  strata = ~estrato,  # Estratificación
  data = base_personas_trim,
  weights = ~fep_aj)  # Pesos ajustados

# 9.2 Calibración por área y sexo
design_cali <- calibrate(  
  design = design_long,  
  formula = fomula_calb, # Efectos fijos sin intercepto
  population = total_pob) %>% 
  as_survey()

# 9.3 Comparar pesos antes/después de calibración
summary(weights(design_long))
summary(weights(design_cali))

# 9.4 Validación de calibración
# Por estrato
options(survey.lonely.psu="certainty")
design_cali %>% group_by(estrato) %>%
  summarise(n_upm = n_distinct(upm),
            total = survey_total(vartype = "se"))


# 9.5 Asignar pesos calibrados a la base
base_personas_trim$fex_cali <- weights(design_cali)

saveRDS(base_personas_trim, file.path(input, "base_personas_trim.rds"))
# =============================================
# FIN DEL SCRIPT
# =============================================