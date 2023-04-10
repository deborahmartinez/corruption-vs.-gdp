library(dplyr)
library(tidyverse)
library(Hmisc)
library(ggpubr)
library(purrr)  
library(ggplot2)
library(broom)
library(stargazer)


# Bases -------------------------------------------------------------------
corrupcion <- read_csv("Variables/Corrupción/Finales/corrupcion.csv")
pib <- read_csv("Variables/Corrupción/Finales/pib.csv")
control <- read_csv("Variables/Corrupción/Finales/control.csv")

bd <- corrupcion %>%  left_join(pib, by = c("entidad", "year")) %>% 
  mutate(corrupcion = Muy_frecuente+Frecuente,
         camara = Diputados_y_Senadores_Muy_frecuente+Diputados_y_Senadores_Frecuente,
         ejercito =Ejército_y_Marina_Muy_frecuente+Ejército_y_Marina_Frecuente,
         federal = Gobierno_Federal_Muy_frecuente+Gobierno_Federal_Frecuente,
         estatal = Gobiernos_Estatales_Muy_frecuente+Gobiernos_Estatales_Frecuente,
         municipal = Gobiernos_Municipales_Muy_frecuente+ Gobiernos_Municipales_Frecuente,
         ministerio = Ministerio_Publico_Muy_frecuente+Ministerio_Publico_Frecuente,
         policia = Policias_Muy_frecuente+Policias_Frecuente, 
         contacto = contacto/pob_usuario, tramites =  tramites/pob_usuario) %>%  
  select(entidad,year, corrupcion, camara,federal, estatal, municipal,
         ejercito, ministerio, policia, contacto, tramites, 
         starts_with(match = "pib_"))

bd_completa <- bd %>%  inner_join(control, by = c("entidad", "year")) 
# bd_completa %>%  write_excel_csv("Variables/Corrupción/Finales/completa_respaldo.csv")
# r2 ----------------------------------------------------------------------

bd %>% select(- starts_with(c("pib_")), -entidad) %>% 
  map(~lm(bd$pib_total~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x) 

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_terciarias~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x) 

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_secundarias~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x) 

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_primarias~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x) 

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_educacion~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x)

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_salud~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x)

bd %>% select(- starts_with(c("pib_")), -entidad, -year) %>% 
  map(~lm(bd$pib_salud~ .x, data = bd)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x)

bd %>%  select(pib_secundarias, contacto) %>%  na.omit() %>% 
  cor


# Modelo ------------------------------------------------------------------

#Corrupción general

modelo1 <- lm(pib_total~corrupcion+escolaridad+irs+wjp_edoderecho+habla_indigena+
                inversion + exportaciones,
              data = bd_completa) 
names(modelo1$coefficients) <- c("Intercept",'Corruption perception',
                                 'average school years',
                                 'Social backwardness index', "Rule of law",
                                 "Indigenous speaking", "Public spending", 
                                 "Exports")
summary(modelo1)
modelo1%>%  stargazer(out = "~/Desktop/models.html", type = "html", dep.var.labels = "Total GDP per capita by entity at constant prices",
                      title = "GDP by state and percentage of the population that perceives corruption as frequent and very frequent")
plot(modelo1)

confint(modelo1)

anova(modelo1)

step(modelo1, direction = "backward", )

modelo1_2 <- lm(pib_terciarias~corrupcion+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion,
                data = bd_completa) 
summary(modelo1_2)

ministerio <- lm(pib_terciarias~ministerio +escolaridad+irs+wjp_edoderecho+habla_indigena+
                   inversion,
                 data = bd_completa) 
summary(ministerio)

ministerio<- lm(pib_secundarias~corrupcion+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion,
                data = bd_completa) 
summary(ministerio)

modelo1_3 <- lm(pib_primarias~corrupcion+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion,
                data = bd_completa) 
summary(modelo1_3)



modelo2 <- lm(pib_total~tramites+escolaridad+irs+wjp_edoderecho+habla_indigena+
                inversion+exportaciones,
              data = bd_completa) 
names(modelo2$coefficients) <- c("Intercept",'Procedures in which there was corruption',
                                 'average school years',
                                 'Social backwardness index', "Rule of law",
                                 "Indigenous speaking", "Public spending", 
                                 "Exports")
summary(modelo1)
modelo2%>%  stargazer(out = "~/Desktop/models2.html", type = "html", dep.var.labels = "Total GDP per capita by entity at constant prices",
                      title = "GDP by state and proportion of procedures in which there was corruption")
summary(modelo2)

modelo2_2 <- lm(pib_terciarias~tramites+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion+exportaciones,
                data = bd_completa) 
summary(modelo2_2)

names(modelo2_2$coefficients) <- c( "Intercept",
                                    'Procedures in which there was corruption',
                                    'average school years',
                                    'Social backwardness index', "Rule of law",
                                    "Indigenous speaking", "Public spending", 
                                    "Exports")
summary(modelo2_2)
modelo2_2%>%  stargazer(out = "~/Desktop/models2_2.html", type = "html", dep.var.labels = "GDP of tertiary activities per capita by entity at constant prices",
                        title = "GDP of tertiary activities by state and proportion of procedures in which there was corruption")

modelo2_2 <- lm(pib_secundarias~tramites+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion,
                data = bd_completa) 
summary(modelo2_2)

modelo2_3 <- lm(pib_primarias~tramites+escolaridad+irs+wjp_edoderecho+habla_indigena+
                  inversion,
                data = bd_completa) 
summary(modelo2_3)

lm(pib_salud ~tramites+escolaridad+irs+wjp_edoderecho+habla_indigena+
     inversion+exportaciones,
   data = bd_completa) %>%  summary()

lm(pib_terciarias~contacto+escolaridad+irs+wjp_edoderecho+habla_indigena+
     inversion+exportaciones,
   data = bd_completa) %>%  summary()
# Mapa --------------------------------------------------------------------
library(sf)
library(leaflet)

mexico <- st_read("Variables/Corrupción/Finales/dest_2015gw/dest_2015gw.shp")

mexico <- mexico %>%  mutate(NOM_ENT = case_when(NOM_ENT == "Distrito Federal" ~ "Ciudad de México",
                                                 NOM_ENT == "México" ~ "Estado de México",
                                                 T ~NOM_ENT)) 
# as_tibble() %>% 
# separate(col = geometry, into =  c("lon", "lat"), sep = ", ") %>%
# mutate(lon = gsub(pattern = "list(list(c(", replacement = "", x = lon, fixed = T))

aux <- bd %>% filter(year == 2019) %>% 
  select(entidad, corrupcion) %>% 
  rename(NOM_ENT = entidad)

mexico <- mexico %>%  left_join(aux, by = "NOM_ENT")

bins <- c( 0, 20, 40, 60, 80, 100)
pal <- colorBin("YlOrRd", domain = aux$corrupcion, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  mexico$NOM_ENT, mexico$corrupcion
) %>% lapply(htmltools::HTML)


percepcion_mapa <- leaflet(mexico) %>%
  # setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons(
    fillColor = ~pal(corrupcion),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~corrupcion, opacity = 0.7, title = NULL,
            position = "bottomright")


mexico <- st_read("Variables/Corrupción/Finales/dest_2015gw/dest_2015gw.shp")

mexico <- mexico %>%  mutate(NOM_ENT = case_when(NOM_ENT == "Distrito Federal" ~ "Ciudad de México",
                                                 NOM_ENT == "México" ~ "Estado de México",
                                                 T ~NOM_ENT)) 
aux <- bd %>% filter(year == 2019) %>% 
  # select(entidad, year, corrupcion )
  mutate(tramites = tramites*100) %>% 
  select(entidad, tramites) %>% 
  rename(NOM_ENT = entidad) %>%  arrange(desc(tramites))

mexico <- mexico %>%  left_join(aux, by = "NOM_ENT")

bins <- c(0,2,4,6,8,10,12)
pal <- colorBin("YlOrRd", domain = aux$tramites, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  mexico$NOM_ENT,mexico$tramites
) %>% lapply(htmltools::HTML)


tramites_mapa <- leaflet(mexico) %>%
  # setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons(
    fillColor = ~pal(tramites),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~tramites, opacity = 0.7, title = NULL,
            position = "bottomright")

tramites_mapa
corrupcion_mapa

bd %>%  filter(year ==2019) %>%  arrange(desc(corrupcion)) %>%  
  select(entidad, tramites, corrupcion) %>% 
  ggplot(aes(x = fct_reorder(entidad, tramites), y = tramites))+
  ggchicklet::geom_chicklet(width = .9, alpha =.9, fill = "#0096c7")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
  ggfittext::geom_bar_text(aes(label=scales::percent(tramites, accuracy = 1)),
                           contrast = T)+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Times New Roman", size = 20))+
  labs(x = "Entidad", y = NULL)

bd %>%  filter(year ==2019) %>%  arrange(desc(corrupcion)) %>%  
  select(entidad, tramites, corrupcion) %>% 
  mutate(corrupcion = corrupcion/100) %>% 
  ggplot(aes(x = fct_reorder(entidad, corrupcion), y = corrupcion))+
  ggchicklet::geom_chicklet(width = .9, alpha =.9, fill = "#023e8a")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
  ggfittext::geom_bar_text(aes(label=scales::percent(corrupcion, accuracy = 1)),
                           contrast = T)+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Times New Roman", size = 20))+
  labs(x = "Entidad", y = NULL)

bd %>%  ggplot(aes(x = tramites, y = corrupcion))+
  geom_point()+
  geom_text(aes(label = entidad), check_overlap = T)+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman"))

bd %>%  select(pib_terciarias, contacto) %>%na.omit() %>%   cor()


bd %>% filter(year == 2019) %>% 
  # select(entidad, year, corrupcion )
  # mutate(tramites = tramites*100) %>% 
  select(entidad, tramites) %>% 
  rename(NOM_ENT = entidad) %>%  arrange(desc(tramites)) %>% 
  ggplot(aes(x = fct_reorder(NOM_ENT, tramites), y= tramites))+
  ggchicklet::geom_chicklet(alpha=.8, fill = "#FF5F4A")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1) ) +
  labs(x =  NULL, y= "Trámites corruptos")+
  theme(text = element_text(family = "Times New Roman", size =16),
        panel.grid.major = element_blank(),
        panel.grid.minor.y  = element_blank())


bd %>% filter(year == 2019) %>% 
  select(entidad, corrupcion) %>% 
  rename(NOM_ENT = entidad) %>% 
  mutate(corrupcion = corrupcion/100) %>% 
  ggplot(aes(x = fct_reorder(NOM_ENT, corrupcion), y= corrupcion))+
  ggchicklet::geom_chicklet(alpha=.8, fill = "#DB1446")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1) ) +
  labs(x =  NULL, y= "Percepción de corrupción frecuente y muy frecuente")+
  theme(text = element_text(family = "Times New Roman", size =16),
        panel.grid.major = element_blank(),
        panel.grid.minor.y  = element_blank())

