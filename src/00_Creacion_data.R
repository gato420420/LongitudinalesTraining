#################################################
#             Proyecto : PPT                    #
#        Construcción base ejemplo              #
#################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################

library(tidyverse)
library(dplyr)
library(labelled)
library(survey)
library(haven)
library(srvyr)

################################################################################
###----------------------------- Loading dataset  ---------------------------###
################################################################################

### Temporary directories ###
input <- "Data/"

base <- read_dta(file.path(input, "data_2017N.dta"))

id_hogar <- base %>% distinct(id_hogar) 

set.seed(123)

id_hogar_t1 <- sample_n(id_hogar, 2000) %>% mutate(trimestre = "t1")
id_hogar <- anti_join(id_hogar, id_hogar_t1)
id_hogar_t2 <- sample_n(id_hogar, 500)


id_hogar_t2 <- sample_n(id_hogar_t1, 1400) %>% bind_rows(id_hogar_t2) %>% mutate(trimestre = "t2")

id_hogar_T <- bind_rows(id_hogar_t1, id_hogar_t2)

base_hogares <- base %>% inner_join(id_hogar_T, by = "id_hogar")
# base_hogares %>% distinct(trimestre, id_hogar) %>% group_by(trimestre) %>% tally()


########################################
#----- Construccion base rotativa -----#
########################################

base_hogar <- base_hogares %>%
  transmute(
    id_hogar,
    id_pers,
    trimestre,
    parentesco    = as.character(as_factor(paren_ee,levels = "values")),
    edad          = edad,
    sexo          = sexo,
    etnia         = as.character(as_factor(etnia_ee,levels = "values")),
    area          = as.character(as_factor(area_ee,levels = "values")),
    ingreso       = ingcorte,
    pobreza       = as.character( as_factor(pobreza, levels="values")),
    upm           =`_upm`,
    estrato       = `_estrato`,
    fep           = `_fep`,
    anoest        = anoest,
    niveduc_ee    = niveduc_ee
  ) %>%
  ungroup()

base_hogar <- base_hogar %>% 
  mutate(
    ingreso = ifelse(trimestre == "t1", ingreso, ingreso + rnorm(1,0, 10000)),
    unif = runif(n()),
    pobreza = case_when(
      trimestre == "t2" & pobreza == "1" & unif < 0.03 ~ "2",
      trimestre == "t2" & pobreza == "2" & unif < 0.05 ~ "1",
      trimestre == "t2" & pobreza == "3" & unif < 0.03 ~ "2",
      trimestre == "t2" & pobreza == "2" & unif < 0.07 ~ "3",
      TRUE ~ pobreza
    )
  )

etnia <- base_hogar %>% filter(parentesco == "1") %>% select(id_hogar, etnia2 = etnia) %>% distinct()

base_hogar <- base_hogar %>% inner_join(etnia, by = "id_hogar")
base_hogar <- base_hogar %>% select(-etnia)

base_hogar <- base_hogar %>% group_by(id_hogar) %>% mutate(
  pobreza_h = ifelse(any(pobreza != "3"), 1,0)
) %>% ungroup()

base_hogar <- base_hogar %>% group_by(id_hogar) %>% mutate(
  unif = runif(n()),
  temu = case_when(
    trimestre == "t2" & unif >0.90 ~ "1",
    TRUE ~ "0"
  )
)

base_hogar <- base_hogar %>% filter(temu == "0")
names(base_hogar)

base_hogar <- base_hogar %>% select(-c("unif", "temu")) %>% rename(etnia = etnia2)
# base_hogar %>% group_by(trimestre) %>% summarise(mean(ingreso), sd(ingreso))
# base_hogar %>% group_by(trimestre, pobreza2) %>% tally()
# base_hogar %>% group_by(trimestre, pobreza) %>% tally()

##########################################
# Calibración de totales persona --------#
##########################################


base_t1 <- base_hogar %>% filter(trimestre == "t1")
base_t2 <- base_hogar %>% filter(trimestre == "t2")

vec_tol <- colSums(model.matrix(~ 0 + area + etnia, data = base_t1)*base_t1$fep) #vector de totales


design<- svydesign(
  ids = ~upm,
  strata = ~estrato,
  nest = TRUE, #anidado
  data = base_t1,
  weights = ~fep
)


design_cali <- calibrate(
  design = design,
  formula = ~ 0 + area + etnia,           
  population = vec_tol
)

summary(weights(design_cali))

##########################################
# Calibración de totales hogares --------#
##########################################
base_h_t1 <- base_t1 %>% group_by(id_hogar, area, fep) %>% count() 
base_h_t2 <- base_t2 %>% group_by(id_hogar, area, fep, etnia, estrato, upm) %>% count() 
vec_tol_h <- colSums(model.matrix(~ 0 + area, data = base_h_t1)*base_h_t1$fep) #vector de totales


vec_tol_et <- colSums(model.matrix(~ 0 + etnia, data = base_t1)*base_t1$fep) #vector de totales

vec_tol_ho <- c(vec_tol_h, vec_tol_et) 


##########################################
# Base diseño ------------------ --------#
##########################################

x_area <- as.data.frame(model.matrix(~0 + area , data = base_h_t2))
x_etnia <- as.data.frame(model.matrix(~0 + etnia , data = base_h_t2))
x <- cbind(x_area, x_etnia)
x$etnia1 <- x$etnia1*base_h_t2$n
x$etnia99 <- x$etnia99*base_h_t2$n
x$etnia0 <- x$etnia0*base_h_t2$n
base_h_t2 <- base_h_t2 %>% bind_cols(x)

design_h<- svydesign(
  ids = ~upm,
  strata = ~estrato,
  nest = TRUE, #anidado
  data = base_h_t2,
  weights = ~fep
)

options(survey.lonely.psu="adjust")
design_cali_h <- calibrate(
  design = design_h,
  formula = ~ 0 + area1 + area2 + etnia0+ etnia1 + etnia99,           
  population = vec_tol_ho
) %>% as_survey()

design_cali_h %>% group_by(etnia) %>% cascade(total = survey_total(n))
design_cali_h %>% group_by(area) %>% cascade(total = survey_total())

sum(weights(design_cali_h))
summary(weights(design_cali_h))
hist(weights(design_cali_h))

base_t2 <- base_h_t2 %>% ungroup() %>% select(id_hogar) %>%
  mutate(fep = weights(design_cali_h)) %>%  
  inner_join(base_t2 %>% select(-fep))

base_hogares <- bind_rows(base_t1, base_t2)
base_hogares %>% group_by(trimestre) %>% summarise(sum(fep))

base_hogares %>% group_by(trimestre, area) %>% summarise(sum(fep))

base_hogares %>% group_by(trimestre, etnia) %>% summarise(sum(fep))

saveRDS(base_hogar,file.path("Data/base_personas.rds") )
