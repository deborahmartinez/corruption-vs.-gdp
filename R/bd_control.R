#Crear base de datos

library(dplyr)
library(tidyverse)
library(readxl)
# irs ---------------------------------------------------------------------
irs15 <- read_excel("Variables/Corrupción/Originales/IRS_entidades_mpios_2015.xlsx", skip =6)
irs15 <-irs15 %>%  mutate(entidad = case_when(entidad == "Distrito Federal"~ "Ciudad de México",
                                              entidad == "Coahuila" ~ "Coahuila de Zaragoza",
                                              entidad == "Michoacán"~ "Michoacán de Ocampo",
                                              entidad == "Veracruz"~ "Veracruz de Ignacio de la Llave",
                                              entidad == "México"~ "Estado de México",
                                              T~entidad)) %>% 
  filter(!is.na(entidad))
irs15 <- irs15 %>%  select(entidad, irs) %>%  mutate(year = 2015, year_real = 2015)
irs17 <- irs15 %>%  select(entidad, irs) %>%  mutate(year = 2017, year_real = 2015)

irs20 <- read_excel("Variables/Corrupción/Originales/IRS_entidades_mpios_2020.xlsx", skip =6)
irs20 <- irs20 %>%  select(entidad, irs) %>%  mutate(year = 2019, year_real = 2020)%>% 
  mutate(entidad = case_when(entidad == "México"~ "Estado de México",
                             T~entidad)) %>%
  filter(!is.na(entidad))

irs <- bind_rows(irs15, irs17, irs20)
rm(irs15, irs17, irs20)


# lengua indígena ---------------------------------------------------------
li15 <- read_csv("Variables/Corrupción/Modificadas manualmente/lengua_indígena_15.csv")

li15 <- li15 %>%  
  mutate(entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad),
         year = 2015, year_li = 2015) 

li17 <- li15 %>%  
  mutate(entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad),
         year = 2017, year_li = 2015) 

li19 <- read_csv("Variables/Corrupción/Modificadas manualmente/lengua_indígena_20.csv")
li19 <-li19 %>%  mutate(habla_indigena = habla_indigena/pob,
                        entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad),
                        year = 2019, year_li = 2020) %>% 
  select(-pob)

li <- bind_rows(li15, li17, li19)
li <- li %>%  
  mutate(entidad=case_when(entidad == "México"~ "Estado de México",
                           T~entidad))
rm(li15, li17, li19)
# escolaridad promedio ----------------------------------------------------
esc15 <- read_csv("Variables/Corrupción/Modificadas manualmente/escolaridad_15.csv")
esc15 <- esc15 %>% mutate(entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad),
                          year = 2015, year_esc = 2015)
esc17 <-esc15 %>% mutate(year = 2017, year_esc = 2015)

esc19 <- read_csv("Variables/Corrupción/Modificadas manualmente/escolaridad_20.csv")
esc19 <- esc19 %>% mutate(entidad = gsub("[[:digit:]]", "", entidad), entidad = str_squish(entidad),
                          year = 2019, year_esc = 2020)

esc <- bind_rows(esc15, esc17, esc19) 
esc <-esc %>%  mutate(entidad=case_when(entidad == "México"~ "Estado de México",
                                        T~entidad))
rm(esc15, esc17, esc19)

# Inversión pública -------------------------------------------------------

ip <- read_csv("Variables/Corrupción/Originales/INEGI_inv.csv", skip = 4)
ip <- ip %>%  select(entidad, `2015`, `2017`, `2019` ) %>% 
  filter(entidad != "Total") %>% 
  gather(year, inversion, `2015`:`2019`) %>% filter(!is.na(inversion)) 

ip_cdmx <- read_csv("Variables/Corrupción/Originales/INEGI_inversion_cdmx.csv", skip = 4)
ip_cdmx <-ip_cdmx %>%  select( `2015`, `2017`, `2019`) %>%  na.omit() %>% 
  gather(year, inversion) %>%  mutate(entidad = "Ciudad de México")


inversion <- bind_rows(ip, ip_cdmx ) 
inversion <- inversion %>%  mutate(entidad=case_when(entidad == "México"~ "Estado de México",
                                        T~entidad))
rm(ip, ip_cdmx)
# Exportaciones -----------------------------------------------------------
exp <- read_excel("Variables/Corrupción/Originales/EAEF_Entidad.xlsx", skip = 4)
exp <-exp %>%  select(entidad = `Entidad Federativa`, `2015`, `2017`, `2019`) %>% 
  filter(!is.na(entidad), entidad != "Exportaciones totales*") %>% 
  gather(year, exportaciones, `2015`:`2019` ) %>% 
  filter(!is.na(exportaciones))  %>% 
  mutate(entidad=case_when(entidad == "México"~ "Estado de México",
                           T~entidad))

# Índice de edo de derecho -------------------------------------------------------
ed <- read_excel("Variables/Corrupción/Originales/wjp/Mapa_IndiceEstadoDeDerechoEnMexico2018.xlsx", sheet = 3)

ed <- ed %>%  filter(Puntaje %in% c("Puntaje Total", "Factor 2: Ausencia de corrupción")) %>% 
  gather(var, puntos, Aguascalientes:Zacatecas) %>% 
  spread(Puntaje, puntos) %>% 
  set_names(c("entidad", "wjp_corrupcion", "wjp_edoderecho"))  
ed15 <- ed %>%   mutate(year = 2015, year_wjp = 2018)
ed17 <- ed %>%   mutate(year = 2017, year_wjp = 2018)


ed <- read_excel("Variables/Corrupción/Originales/wjp/6_Mapa_IndiceEstadoDeDerechoEnMexico2019-2020-V3.xlsx", sheet = 3)

ed19 <- ed %>%  filter(`...1` %in% c("Puntaje Total", "Factor 2: Ausencia de corrupción")) %>% 
  gather(var, puntos, Aguascalientes:Zacatecas) %>% 
  spread(`...1`, puntos) %>% 
  set_names(c("entidad", "wjp_corrupcion", "wjp_edoderecho")) %>% 
  mutate(year = 2019, year_wjp = 2019)

ed <- bind_rows(ed15,ed17,ed19 )
ed <-ed %>%  mutate(entidad = case_when(entidad == "Coahuila" ~ "Coahuila de Zaragoza",
                                        entidad == "Michoacán"~ "Michoacán de Ocampo",
                                        entidad == "Veracruz"~ "Veracruz de Ignacio de la Llave",
                                        T~entidad))
rm(ed15,ed17,ed19)


# bd ----------------------------------------------------------------------

bd_control <-ed %>% 
  inner_join(esc, by = c("entidad", "year")) %>%  
  inner_join(exp %>%  mutate(year = as.numeric(year)), by = c("entidad", "year")) %>% 
  inner_join(inversion %>%  mutate(year = as.numeric(year)), by = c("entidad", "year")) %>% 
  inner_join(irs, by = c("entidad", "year")) %>% 
  inner_join(li, by = c("entidad", "year"))

bd_control %>%  write_excel_csv("Variables/Corrupción/Finales/control.csv")  

