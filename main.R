library(tidyverse)
library(ggtext)
library(glue)

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
    periodo = factor(periodo, levels = c("P1","P2","P3","P4")),
    periodo_anio = case_when(
      periodo=="P1" ~ "2016-2017",
      periodo=="P2" ~ "2018-2019",
      periodo=="P3" ~ "2020-2021",
      periodo=="P4" ~ "2022-2023")
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


(recurrencia_periodos_plot <- datos_categorias %>% 
  mutate(periodo_mod = glue("<b>{periodo}</b><br><i>{periodo_anio}</i>")) %>% 
  group_by(periodo_mod, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo_mod, y = n, fill = categoria)) +
  geom_col(width = .5) +
  ggthemes::scale_fill_tableau() +
  labs(y = NULL, fill = "Categoría", x = "Periodo") +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_markdown()
  ))

ggsave("plot1.png", width = 10, height = 10, dpi = 320)


(recurrencia_)
