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



datos <- read_csv("data/datos_v3.csv")

datos_categorias <- datos %>% 
  mutate(
    periodo = case_when(
      cohorte_anio %in% c(2016,2017) ~ "P1",
      cohorte_anio %in% c(2018,2019) ~ "P2",
      cohorte_anio %in% c(2020,2021) ~ "P3",
      cohorte_anio %in% c(2022,2023) ~ "P4"),
    periodo = factor(periodo, levels = c("P1","P2","P3","P4"))
  ) %>% 
  mutate(
    categoria = case_when(
      periodo=="P1" ~ "nuevos",
      periodo=="P2" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P2" & p1_act=="No" ~ "nuevos",
      periodo=="P3" & p1_act=="Si" & p2_act=="No" ~ "recurrentes_p1",
      periodo=="P3" & p1_act=="Si" & p2_act=="Si" ~ "recurrentes_p1_p2",
      periodo=="P3" & p1_act=="No" & p2_act=="Si" ~ "recurrentes_p2",
      periodo=="P3" & p1_act=="No" & p2_act=="No" ~ "nuevos",
      periodo=="P4" & p1_act=="Si" & p2_act=="No" & p3_act=="No" ~ "recurrentes_p1",
      periodo=="P4" & p1_act=="Si" & p2_act=="Si" & p3_act=="No" ~ "recurrentes_p1_p2",
      periodo=="P4" & p1_act=="Si" & p2_act=="Si" & p3_act=="Si" ~ "recurrentes_p1_p2_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="Si" & p3_act=="Si" ~ "recurrentes_p2_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="Si" & p3_act=="No" ~ "recurrentes_p2",
      periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="Si" ~ "recurrentes_p3",
      periodo=="P4" & p1_act=="Si" & p2_act=="No" & p3_act=="Si" ~ "recurrentes_p1_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="No" ~ "nuevos"),
    categoria = as.factor(categoria)
  )

datos_categorias %>% 
  group_by(periodo, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo, y = n, fill = categoria)) +
  geom_col()
