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
      periodo=="P1" ~ "nuevas",
      periodo=="P2" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P2" & p1_act=="No" ~ "nuevas",
      periodo=="P3" & p1_act=="Si" & p2_act=="No" ~ "recurrentes_p1",
      periodo=="P3" & p1_act=="Si" & p2_act=="Si" ~ "recurrentes_p1_p2",
      periodo=="P3" & p1_act=="No" & p2_act=="Si" ~ "recurrentes_p2",
      periodo=="P3" & p1_act=="No" & p2_act=="No" ~ "nuevas",
      periodo=="P4" & p1_act=="Si" & p2_act=="No" & p3_act=="No" ~ "recurrentes_p1",
      periodo=="P4" & p1_act=="Si" & p2_act=="Si" & p3_act=="No" ~ "recurrentes_p1_p2",
      periodo=="P4" & p1_act=="Si" & p2_act=="Si" & p3_act=="Si" ~ "recurrentes_p1_p2_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="Si" & p3_act=="Si" ~ "recurrentes_p2_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="Si" & p3_act=="No" ~ "recurrentes_p2",
      periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="Si" ~ "recurrentes_p3",
      periodo=="P4" & p1_act=="Si" & p2_act=="No" & p3_act=="Si" ~ "recurrentes_p1_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="No" ~ "nuevas"),
    categoria = as.factor(categoria)
  )




lineas_verticales_alturas <- datos_categorias %>% 
  filter(periodo != "P1", categoria!="nuevas") %>% 
  group_by(periodo) %>% 
  summarise(altura = n_distinct(idpersona)) %>% 
  pull(altura)

lineas_verticales_pos <- c(2.35,3.35,4.35)

lineas_v <- data.frame(
  x = lineas_verticales_pos,
  xend = lineas_verticales_pos,
  y = 0,
  yend = lineas_verticales_alturas
)

lineas_h <- data.frame(
  x = c(2.3,3.3,4.3),
  xend = c(2.4,3.4,4.4),
  y = lineas_verticales_alturas,
  yend = lineas_verticales_alturas
)

porcentajes <- datos_categorias %>% 
  filter(periodo != "P1") %>% 
  group_by(periodo) %>% 
  summarise(total = n_distinct(idpersona),
            recurrentes = n_distinct(idpersona[categoria!="nuevas"])) %>% 
  mutate(pct = recurrentes / total) %>% 
  pull(pct)

porcentajes_df <- data.frame(
  label = porcentajes,
  x = c(2.4,3.4,4.4),
  y = lineas_verticales_alturas / 2
)

(recurrencia_periodos_plot <- datos_categorias %>% 
  mutate(periodo_mod = glue("<b>{periodo}</b><br><i>({periodo_anio})</i>")) %>% 
  group_by(periodo_mod, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo_mod, y = n)) +
  geom_col(aes(fill = categoria), width = .5, color = "black") +
  geom_segment(data = lineas_v, mapping = aes(x = x, xend = xend, y = y, yend = yend),
               size = 1) +
  geom_segment(data = lineas_h, mapping = aes(x = x, xend = xend, y = y, yend = yend),
               size = 1) +
  geom_text(data = porcentajes_df, 
            mapping = aes(label = scales::percent(label, accuracy = 1), x = x, y = y), 
            hjust = 0, size = 5) +
  scale_y_continuous(breaks = seq(0,10000,2000)) +
  ggthemes::scale_fill_tableau() +
  coord_cartesian(clip = "off") +
  labs(y = NULL, fill = "Categoría", x = "Periodo") +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_markdown()
  ))

ggsave("plot1.png", width = 10, height = 10, dpi = 320)



