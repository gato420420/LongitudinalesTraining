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

base <- read_dta(file.path(input, "data_2017N.dta")) %>% 
  mutate( upm     = as.character(`_upm`))

id_upm <- base %>% distinct(upm) 

set.seed(123)

id_upm_t1 <- sample_n(id_upm, 500) %>% mutate(trimestre = "t1")
id_upm <- anti_join(id_upm, id_upm_t1)
id_upm_t2 <- sample_n(id_upm, 125)

id_upm_t2 <- sample_n(id_upm_t1, 370) %>%
  bind_rows(id_upm_t2) %>% mutate(trimestre = "t2")

id_upm_T <- bind_rows(id_upm_t1, id_upm_t2)

temp_upm <- id_upm_T %>% distinct(upm)

id_hogares <-  base %>% inner_join(temp_upm) %>% 
  distinct(id_hogar, upm) %>% 
  group_by(upm) %>% 
  sample_n(10) %>% inner_join(id_upm_T)

base_hogares <- base %>% inner_join(id_hogares, by = c("upm", "id_hogar"))

base_hogares %>%  distinct( upm, id_hogar ,trimestre) %>% 
  group_by(upm,trimestre) %>% 
tally() %>% arrange(n)

 # base_hogares %>% distinct(trimestre, upm) %>% 
 #   group_by(trimestre) %>% tally()


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
    sexo          = as.character(as_factor(sexo,levels = "labels")),
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

base_hogar <- base_hogar %>% group_by(id_hogar) %>% mutate(
  unif = runif(n()),
  temu = case_when(
    trimestre == "t2" & unif >0.90 ~ "1",
    TRUE ~ "0"
  )
)

base_hogar <- base_hogar %>% filter(temu == "0")
names(base_hogar)

base_hogar <- base_hogar %>% select(-c("unif", "temu"))
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
design_trimestre<- svydesign(
  ids = ~upm+trimestre,
  strata = ~estrato,
  nest = TRUE, #anidado
  data = base_hogar,
  weights = ~fep
) %>% as_survey()

design_trimestre %>% group_by(trimestre, sexo) %>% cascade(total = survey_total())
design_trimestre %>% filter(parentesco  == 1) %>%
  group_by(trimestre, area) %>% cascade(total = survey_total())



base_h_t1 <- base_t1 %>% group_by(id_hogar, area, fep) %>% count() 

vec_tol_h <- colSums(model.matrix(~ 0 + area, data = base_h_t1)*base_h_t1$fep) #vector de totales


vec_tol_sex <- colSums(model.matrix(~ 0 + sexo, data = base_t1)*base_t1$fep) #vector de totales

vec_tol_ho <- c(vec_tol_h, vec_tol_sex) 


##########################################
# Base diseño ------------------ --------#
##########################################
base_h_t2_area <- base_t2 %>% 
  distinct(id_hogar, area, fep, estrato, upm) 

base_h_t2_sexo <- base_t2 %>% 
  group_by(id_hogar) %>% 
  summarise(sexoHombre = sum(sexo    == "Hombre"), 
            sexoMujer  = sum(sexo    == "Mujer")) 

base_h_t2 <- inner_join(base_h_t2_area, base_h_t2_sexo)
x_area <- as.data.frame(model.matrix(~0 + area , data = base_h_t2))

base_h_t2 <- base_h_t2 %>% bind_cols(x_area)

design_h<- svydesign(
  ids = ~upm,
  strata = ~estrato,
  nest = TRUE, #anidado
  data = base_h_t2,
  weights = ~fep
) %>% as_survey()

design_h %>% cascade(total = survey_total(sexoHombre))
design_h  %>% cascade(total = survey_total(sexoMujer))
design_h %>% group_by(area)  %>% cascade(total = survey_total())


options(survey.lonely.psu="adjust")
design_cali_h <- calibrate(
  design = design_h,
  formula = ~ 0 + area1 + area2 + sexoHombre +  sexoMujer ,           
  population = vec_tol_ho, verbose = FALSE,
  calfun = "raking"
) %>% as_survey()

design_cali_h %>% cascade(total = survey_total(sexoHombre))
design_cali_h  %>% cascade(total = survey_total(sexoMujer))
design_cali_h %>% group_by(area)  %>% cascade(total = survey_total())

sum(weights(design_cali_h))
summary(weights(design_cali_h))
hist(weights(design_cali_h))

base_t2 <- base_h_t2 %>% ungroup() %>% select(id_hogar) %>%
  mutate(fep = weights(design_cali_h)) %>%  
  inner_join(base_t2 %>% select(-fep))

base_hogares <- bind_rows(base_t1, base_t2)
base_hogares %>% group_by(trimestre) %>% summarise(sum(fep))

base_hogares %>% group_by(trimestre, area) %>% summarise(sum(fep))

base_hogares %>% group_by(trimestre, sexo) %>% summarise(sum(fep))

saveRDS(base_hogar,file.path("Data/base_personas.rds") )
