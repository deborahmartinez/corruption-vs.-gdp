#Crear base de datos

library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)


pob <- read_csv("Variables/Corrupción/Modificadas manualmente/pob_tot_15.csv")

pob <-pob %>%  
  mutate(entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad))  
  
pob15 <- pob %>% mutate(year = 2015, year_real= 2015)
pob17 <- pob %>% mutate(year = 2017, year_real= 2015)


pob19 <- read_csv("Variables/Corrupción/Modificadas manualmente/pob_tot_19.csv")

bind_rows(pob15, pob17, pob19) %>%  write_excel_csv("Variables/Corrupción/Finales/poblacion.csv")
