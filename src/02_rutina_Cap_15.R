# Análisis de Encuestas de Hogares con R 
# Autor: Andrés Gutiérrez y Stalyn Guerrero
# Contacto: andres.gutierrez@cepal.org
# Descripción: Procesamiento de datos longitudinales
# de encuestas de hogares con diseño rotativo

# =============================================
# 1. CONFIGURACIÓN INICIAL Y CARGA DE PAQUETES
# =============================================

# Instalar paquetes si no están instalados (descomentar si es necesario)
# install.packages(c("haven", "tidyverse", "survey",
# "srvyr", "printr", "openxlsx"))

# Cargar paquetes necesarios
library(haven)       # Para importación de datos
library(tidyverse)   # Manipulación y visualización de datos
library(survey)      # Análisis de encuestas complejas
library(srvyr)       # Versión tidy de survey
library(printr)      # Mejor presentación de tablas
library(openxlsx)    # Para leer archivos Excel

# Configurar opciones globales
options(scipen = 999) # Desactivar notación científica

# =============================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# =============================================

# Leer base de datos anual
encuesta_anual <- readRDS("Data/base_anual.rds")

# =============================================
# 3. ANÁLISIS DESCRIPTIVO INICIAL
# =============================================

## 3.1 Conteo de UPMs (Unidades Primarias de Muestreo)
upm_mes <- encuesta_anual %>% 
  distinct(trimestre, mes_trimestre, upm)

# Tabla de conteo de UPMs por mes y trimestre
conteo_upm <- upm_mes %>% 
  group_by(trimestre, mes_trimestre) %>%
  count(name = "n_upm") %>% 
  pivot_wider(
    names_from = mes_trimestre,
    values_from = n_upm,
    values_fill = 0
  ) %>% 
  mutate(Total = M1 + M2 + M3)

print(conteo_upm)

## 3.2 Identificación de traslapes entre trimestres
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


# 3.3 Conteo de hogares por mes y trimestre

hogar_mes <- encuesta_anual %>% 
  distinct(trimestre, mes_trimestre, upm, id_hogar)

hogar_mes %>% group_by(trimestre, mes_trimestre) %>%
  count(name = "n_hogar") %>% 
   pivot_wider(
    names_from = mes_trimestre,
    values_from = n_hogar,
    values_fill = 0  
  )  %>% mutate(Total = M1 + M2 + M3) %>% data.frame()



## 3.4 Conteo de personas por mes y trimestre

encuesta_anual %>% group_by(trimestre, mes_trimestre) %>%
  count(name = "n_pers") %>% 
   pivot_wider(
    names_from = mes_trimestre,
    values_from = n_pers,
    values_fill = 0  
  ) %>% mutate(Total = M1 + M2 + M3) %>% data.frame()

# =============================================
# 4. MANEJO DE FACTORES DE EXPANSIÓN
# =============================================

## 4.1 Ajuste mensual de ponderadores
ponderador_mes <- encuesta_anual %>% 
  group_by(trimestre, mes_trimestre) %>%
  summarise(num = sum(fep), .groups = "drop") %>%
  group_by(trimestre) %>% 
  mutate(
    den = sum(num),
    ai = num / den  # Factor de ajuste mensual
  )

# Unir con la base original
encuesta_anual_ajus <- encuesta_anual %>%
  inner_join(ponderador_mes %>% select(-num, -den), 
             by = c("trimestre", "mes_trimestre")) %>% 
  mutate(dk_mes = fep * ai) # Ponderador ajustado mensual


# Total de hogares estimado por trimestre 

encuesta_anual_ajus %>% 
  filter(id_pers == "1") %>% group_by(trimestre) %>%
  summarise(  tot_hog_dk = sum(dk_mes)) %>% data.frame()

# Total de hogares estimado por mes 

encuesta_anual_ajus %>% 
  filter(id_pers == "1") %>% group_by(trimestre, mes_trimestre) %>%
  summarise(tot_hog_fep = sum(fep)) %>% data.frame()


# Total de personas estimado por trimestre 

encuesta_anual_ajus %>% 
 group_by(trimestre) %>%
  summarise(tot_pers_dk = sum(dk_mes)) %>% data.frame()

# Total de personas estimado por mes 

encuesta_anual_ajus %>% 
 group_by(trimestre, mes_trimestre) %>%
  summarise(tot_pers_fep = sum(fep)) %>% data.frame()


# Total de personas estado de ocupación estimado por trimestre 


encuesta_anual_ajus %>%
  filter(!is.na(trabajo)) %>%
  group_by(trimestre, trabajo) %>%
  summarise(tot_empleo_dk = sum(dk_mes)) %>% 
  pivot_wider(
    names_from = trabajo,
    values_from = tot_empleo_dk,
    values_fill = 0  
  ) %>% data.frame()

# Total de personas estado de ocupación estimado por trimestre 

encuesta_anual_ajus %>%
  filter(!is.na(trabajo)) %>%
  mutate(
    yk =  ifelse(trabajo == "Desocupado", 1, 0),
    zk =  ifelse(trabajo != "Inactivo", 1, 0)
  ) %>% group_by(trimestre) %>%
  summarise(tot_yk = sum(dk_mes*yk),
            tot_zk = sum(dk_mes*zk)) %>% 
  mutate(theta = tot_yk/tot_zk)%>% data.frame()



## 4.2 Ajuste anual de ponderadores
ponderador_anos <- encuesta_anual %>% 
  group_by(trimestre, mes_trimestre) %>%
  summarise(num = sum(fep), .groups = "drop") %>% 
  mutate(
    den = sum(num),
    bi = num / den  # Factor de ajuste anual
  )

# Unir con la base original
encuesta_anual_ajus2 <- encuesta_anual %>%
  inner_join(ponderador_anos %>% select(-num, -den), 
             by = c("trimestre", "mes_trimestre")) %>% 
  mutate(dk_anual = fep * bi) # Ponderador ajustado anual


# Total de hogares estimado anual

encuesta_anual_ajus2 %>% ungroup() %>% 
  filter(id_pers == "1") %>%
  summarise(tot_hog_dk = sum(dk_anual)) %>% data.frame()

encuesta_anual_ajus2 %>% ungroup() %>% 
  summarise(  tot_pers_dk = sum(dk_anual)) %>% data.frame()

# Total de personas estado de ocupación estimado de forma anual 

encuesta_anual_ajus2 %>%
  filter(!is.na(trabajo)) %>%
  group_by(trabajo) %>%
  summarise(tot_empleo_dk = sum(dk_anual)) %>% 
  pivot_wider(
    names_from = trabajo,
    values_from = tot_empleo_dk,
    values_fill = 0  
  ) %>% data.frame()

# Total de personas estado de ocupación estimado de forma anual 

encuesta_anual_ajus2 %>% ungroup() %>% 
  filter(!is.na(trabajo)) %>%
  mutate(
    yk =  ifelse(trabajo == "Desocupado", 1, 0),
    zk =  ifelse(trabajo != "Inactivo", 1, 0)
  ) %>%
  summarise(tot_yk = sum(dk_anual*yk),
            tot_zk = sum(dk_anual*zk)) %>% 
  mutate(theta = tot_yk/tot_zk)%>% data.frame()



# =============================================
# 5. CALIBRACIÓN DE PESOS MUESTRALES
# =============================================

## 5.1 Preparación de datos para calibración

# Leer población de referencia
poblacion <- read.xlsx("Imagenes/15_cap/muestra_anual.xlsx") %>%
  transmute(
    estrato = paste0("hh_", dam, "_", area), 
    N_hogares
  )

# Crear vector de totales poblacionales
total_pob <- setNames(round(poblacion$N_hogares), poblacion$estrato)

# Preparar fórmula para calibración
formula_calb <- as.formula(
  paste0("~ 0 + ", paste0(names(total_pob), collapse = " + "))
)

# Preparar base de hogares
base_hog <- encuesta_anual_ajus %>%
  filter(id_pers == 1) %>% # Un representante por hogar
  distinct(trimestre, mes_trimestre, estrato, dam, upm, id_hogar, dk_mes) %>%
  mutate(estrato_hh = estrato) %>%
  fastDummies::dummy_cols(
    select_columns = "estrato_hh",
    remove_selected_columns = TRUE
  ) %>%
  rename_with(
    ~ gsub("^estrato_", "", .x),
    .cols = starts_with("estrato_hh")
  )

## 5.2 Calibración por trimestre

# Función para calibrar un trimestre específico
calibrar_trimestre <- function(trim, base) {
  df <- base %>% filter(trimestre == trim)
  
  design <- svydesign(
    id = ~upm,
    strata = ~estrato,
    weights = ~dk_mes,
    data = df,
    nest = TRUE
  )
  
  calibrate(
    design = design,
    formula = formula_calb,
    population = total_pob
  ) %>% 
    as_survey()
}

# Calibrar para cada trimestre (T1 a T4)
calibrado_T1 <- calibrar_trimestre("T1", base_hog)
calibrado_T2 <- calibrar_trimestre("T2", base_hog)
calibrado_T3 <- calibrar_trimestre("T3", base_hog)
calibrado_T4 <- calibrar_trimestre("T4", base_hog)

summary(weights(calibrado_T1))
summary(weights(calibrado_T2))
summary(weights(calibrado_T3))
summary(weights(calibrado_T4))

# =============================================
# 6. ANÁLISIS DE RESULTADOS
# =============================================

## 6.1 Crear base con pesos calibrados
base_hog_calib <- bind_rows(
  calibrado_T1$variables %>%
    transmute(trimestre, mes_trimestre, upm, id_hogar,
              dk_mes_cali = weights(calibrado_T1)),
  calibrado_T2$variables %>%
    transmute(trimestre, mes_trimestre, upm, id_hogar,
              dk_mes_cali = weights(calibrado_T2)),
  calibrado_T3$variables %>%
    transmute(trimestre, mes_trimestre, upm, id_hogar,
              dk_mes_cali = weights(calibrado_T3)),
  calibrado_T4$variables %>%
    transmute(trimestre, mes_trimestre, upm, id_hogar,
              dk_mes_cali = weights(calibrado_T4))
)

# Unir con la base completa
encuesta_anual_ajus_calib <- inner_join(
  encuesta_anual_ajus, 
  base_hog_calib,
  by = c("trimestre", "mes_trimestre", "upm", "id_hogar")
)

## 6.2 Estimación de totales poblacionales

# Diseño muestral para T1 calibrado
design_T1_calib <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~dk_mes_cali,
  data = encuesta_anual_ajus_calib %>% filter(trimestre == "T1"),
  nest = TRUE
) %>% 
  as_survey()

# Estimación de totales por mes
total_mes <- design_T1_calib %>% 
  group_by(mes_trimestre) %>%
  summarise(
    total_estimado = survey_total(vartype = "var"),
    .groups = "drop"
  )

print(total_mes)

# Estimación de total trimestral
total_trimestre <- total_mes %>% 
  summarise(
    total_estimado_trim = sum(total_estimado),
    total_estimado_var_trim = sqrt(sum(total_estimado_var))
  )

print(total_trimestre)

## 6.3 Análisis de estado ocupacional

# Por mes
total_mes_empleo <- design_T1_calib %>%
  filter(!is.na(trabajo)) %>% 
  group_by(mes_trimestre, trabajo) %>%
  summarise(
    total_estimado = survey_total(vartype = "var"),
    .groups = "drop"
  )

print(total_mes_empleo)

# Por trimestre
total_trimestre_empleo <- total_mes_empleo %>% 
  group_by(trabajo) %>% 
  summarise(
    total_estimado_trim = sum(total_estimado),
    total_estimado_var_trim = sqrt(sum(total_estimado_var))
  )

print(total_trimestre_empleo)


# 6.4 Comprobación de la estabilidad de los factores trimestrales

# Nacianal
encuesta_anual_ajus_calib %>% 
  group_by(trimestre, mes_trimestre) %>% 
  summarise(n_obs = n(), N_hat = sum(dk_mes_cali )) %>% 
 transmute(trimestre, mes_trimestre, prop = N_hat /n_obs ) %>% 
  pivot_wider(
    names_from = mes_trimestre,
    values_from = prop,
    values_fill = 0  
  ) %>% data.frame()



## Sexo 

encuesta_anual_ajus_calib %>% 
  group_by(trimestre, mes_trimestre, sexo) %>% 
  summarise(n_obs = n(), N_hat = sum(dk_mes_cali )) %>% 
 transmute(trimestre, mes_trimestre, sexo,
           prop = N_hat /n_obs ) %>% 
  pivot_wider(
    names_from = mes_trimestre,
    values_from = prop,
    values_fill = 0  
  ) %>% data.frame()


## Empleo

encuesta_anual_ajus_calib %>% 
  group_by(trimestre, mes_trimestre, trabajo) %>% 
  summarise(n_obs = n(), N_hat = sum(dk_mes_cali )) %>% 
 transmute(trimestre, mes_trimestre, trabajo,
           prop = N_hat /n_obs ) %>% 
  pivot_wider(
    names_from = trabajo,
    values_from = prop,
    values_fill = 0  
  ) %>% data.frame()


## Área - Sexo

encuesta_anual_ajus_calib %>% 
  group_by(trimestre, mes_trimestre, area, sexo) %>% 
  summarise(n_obs = n(), N_hat = sum(dk_mes_cali )) %>% 
 transmute(trimestre, area, mes_trimestre, sexo,
           prop = N_hat /n_obs ) %>% 
  pivot_wider(
    names_from = mes_trimestre,
    values_from = prop,
    values_fill = 0  
  ) %>% data.frame()


## 6.4 Prueba de hipótesis entre trimestres

# Diseño para comparar T1 y T2
design_T1_T2_calib <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~dk_mes_cali,
  data = encuesta_anual_ajus_calib %>% filter(trimestre %in% c("T1", "T2")),
  nest = TRUE
) %>% 
  as_survey()

## Estimación de la media por trimestre 

(media_ingresos <- svyby(~ingreso, ~trimestre,
                         design = design_T1_T2_calib,
                         svymean, vartype = "var",
                         covmat = TRUE))

## Covarianza del ingreso 

(vcov_mat <- attr(media_ingresos, "var"))

## Estimación de la diferencia de dos trimestres 

(delta <- svycontrast(media_ingresos, 
                      quote(`T2` - `T1`)))



## 6.5 Estimación del promedio para 3 trimestre

design_T1_T2_T3_calib <- svydesign(
  id = ~ upm,
  strata = ~ estrato,
  weights = ~ dk_mes_cali,
  data = encuesta_anual_ajus_calib %>%
    filter(trimestre %in% c("T2", "T1", "T3")),
  nest = TRUE
) %>% as_survey()

(
  media_ingresos <- svyby(
    ~ ingreso,
    ~ trimestre,
    design = design_T1_T2_T3_calib,
    svymean,
    vartype = "var",
    covmat = TRUE
  )
)

## Estimación del promedio para 3 trimestre

(media_trimestral <- mean(media_ingresos$ingreso))

vcov(media_ingresos)

(delta <- svycontrast(media_ingresos, 
                      quote((`T1` + `T2`+ `T3`)/3)))


# Comparación de medias de ingreso
resultado_ttest <- svyttest(ingreso ~ trimestre, design = design_T1_T2_calib)
print(resultado_ttest)



# =============================================
# FIN DEL SCRIPT
# =============================================
