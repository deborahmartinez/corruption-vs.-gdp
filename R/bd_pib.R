library(dplyr)
library(tidyverse)
library(readxl)
# total -------------------------------------------------------------------

tot <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_2.xlsx", skip = 4)

tot <- tot %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(7:38) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_total, `2015`:`2019`)
# primarias ---------------------------------------------------------------
pri <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_3.xlsx", skip = 4)

pri <- pri %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(5:36) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_primarias, `2015`:`2019`)
# secundarias -------------------------------------------------------------
sec <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_6.xlsx", skip = 4)
sec <- sec %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(5:36) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_secundarias, `2015`:`2019`)

# terciarias --------------------------------------------------------------

ter <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_25.xlsx", skip = 4)
ter <- ter %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(5:36) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_terciarias, `2015`:`2019`)

# salud -------------------------------------------------------------------
sal <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_36.xlsx", skip = 4)
sal <- sal %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(5:36) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_salud, `2015`:`2019`)

# educación ---------------------------------------------------------------

edu <- read_excel("Variables/Corrupción/Originales/tabulados_pibent/PIBE_35.xlsx", skip = 4)
edu <- edu %>%  select(Concepto,`2015` ,`2017`, `2019R`) %>%  slice(5:36) %>% 
  rename(`2019` = `2019R`, entidad = Concepto) %>% 
  gather(year, pib_educacion, `2015`:`2019`)

pib <- tot %>%  
  left_join(pri, by = c("entidad", "year"))%>%  
  left_join(sec, by = c("entidad", "year")) %>% 
  left_join(ter, by = c("entidad", "year")) %>% 
  left_join(sal, by = c("entidad", "year")) %>% 
  left_join(edu, by = c("entidad", "year"))

# per cápita --------------------------------------------------------------

pob <- read_csv("Variables/Corrupción/Finales/poblacion.csv")

pib <-pib %>%  left_join(pob %>%  mutate(year = as.character(year)), 
                   by = c("entidad", "year") ) %>% 
  mutate(across(.cols = starts_with("pib_"),
                .fns = ~./pob_tot,
                .names = "{.col}")) 

pib %>%  write_excel_csv("Variables/Corrupción/Finales/pib.csv")
