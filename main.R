library(tidyverse)


datos <- read_csv("data/datos.csv")

datos %>% 
  filter(is.na(uc_aprobadas)) %>% 
  View()


datos_endogamia1 <- datos %>% 
  group_by(idpersona) %>% 
  summarise(prop_aprobadas = sum(egresado, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(prop_aprobadas) %>% 
  summarise(personas = n_distinct(idpersona))



