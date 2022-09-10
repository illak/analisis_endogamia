library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)


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



datos <- read_csv("data/datos_v4.csv")

datos_categorias <- datos %>% 
  mutate(
    periodo = case_when(
      cohorte_anio %in% c(2016,2017) ~ "P1",
      cohorte_anio %in% c(2018,2019) ~ "P2",
      cohorte_anio %in% c(2020) ~ "P3",
      cohorte_anio %in% c(2021) ~ "P4",
      cohorte_anio %in% c(2022) ~ "P5"),
    periodo = factor(periodo, levels = c("P1","P2","P3","P4","P5")),
    periodo_anio = case_when(
      periodo=="P1" ~ "2016-2017",
      periodo=="P2" ~ "2018-2019",
      periodo=="P3" ~ "2020",
      periodo=="P4" ~ "2021",
      periodo=="P5" ~ "2022")
  ) %>% 
  mutate(
    categoria = case_when(
      periodo=="P1" ~ "nuevas",
      periodo=="P2" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P2" & p1_act=="No" ~ "nuevas",
      periodo=="P3" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P3" & p2_act=="Si" ~ "recurrentes_p2",
      periodo=="P3" & p1_act=="No" & p2_act=="No" ~ "nuevas",
      periodo=="P4" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P4" & p2_act=="Si" ~ "recurrentes_p2",
      periodo=="P4" & p3_act=="Si" ~ "recurrentes_p3",
      periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="No" ~ "nuevas",
      periodo=="P5" & p1_act=="Si" ~ "recurrentes_p1",
      periodo=="P5" & p2_act=="Si" ~ "recurrentes_p2",
      periodo=="P5" & p3_act=="Si" ~ "recurrentes_p3",
      periodo=="P5" & p4_act=="Si" ~ "recurrentes_p4",
      periodo=="P5" & p1_act=="No" & p2_act=="No" & p3_act=="No" & p4_act=="No" ~ "nuevas"),
    categoria = as.factor(categoria)
  )




lineas_verticales_alturas <- datos_categorias %>% 
  filter(periodo != "P1", categoria!="nuevas") %>% 
  group_by(periodo) %>% 
  summarise(altura = n_distinct(idpersona)) %>% 
  pull(altura)

lineas_verticales_pos <- c(2.35,3.35,4.35,5.35)

lineas_v <- data.frame(
  x = lineas_verticales_pos,
  xend = lineas_verticales_pos,
  y = 0,
  yend = lineas_verticales_alturas
)

lineas_h <- data.frame(
  x = c(2.3,3.3,4.3,5.3),
  xend = c(2.4,3.4,4.4,5.4),
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
  x = c(2.4,3.4,4.4,5.4),
  y = lineas_verticales_alturas / 2
)

(recurrencia_periodos_plot <- datos_categorias %>% 
  mutate(periodo_mod = glue("<b>{periodo}</b><br><i>({periodo_anio})</i>")) %>% 
  group_by(periodo_mod, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo_mod, y = n)) +
  geom_col(aes(fill = categoria), width = .5, color = "black") +
  geom_text(aes(label = n, group = categoria), position=position_stack(vjust=0.5)) +
  geom_segment(data = lineas_v, mapping = aes(x = x, xend = xend, y = y, yend = yend),
               size = 1) +
  geom_segment(data = lineas_h, mapping = aes(x = x, xend = xend, y = y, yend = yend),
               size = 1) +
  geom_text(data = porcentajes_df, 
            mapping = aes(label = scales::percent(label, accuracy = 1), x = x, y = y), 
            hjust = 0, size = 5) +
  scale_y_continuous(breaks = seq(0,10000,2000)) +
  ggthemes::scale_fill_tableau() +
  ggtitle("Recurrencia general") +
  coord_cartesian(clip = "off") +
  labs(y = NULL, fill = "Categoría", x = "Periodo") +
  theme_minimal(base_size = 17) +
  theme(
    axis.text.x = element_markdown(),
    plot.title.position = "plot"
  ))

ggsave("plot1.png", width = 10, height = 10, dpi = 320)



# Por agrupaciones ad-hoc ----

datos_agrupaciones <- read_csv("data/agrupaciones.csv")


datos_rec_grupo <- datos_categorias %>% 
  left_join(datos_agrupaciones) %>% 
  mutate(categoria = if_else(categoria=="nuevas","nuevas","recurrentes"))

recurrencia_periodos_grupo <- datos_rec_grupo %>% 
  group_by(grupo, periodo, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo, y = n, fill=categoria)) +
  geom_col() +
  ggthemes::scale_fill_tableau() +
  labs(y = NULL) +
  facet_wrap(vars(grupo)) +
  theme_minimal() +
  theme(
    strip.background = element_rect(color = "black")
  )


recurrencia_periodos_grupo


datos_2 <- datos %>% 
  left_join(datos_agrupaciones)

filtros <- datos_agrupaciones %>% 
  pull(grupo) %>% 
  unique()

mi_func <- function(filtro) {
    
  datos_2 %>% 
    filter(grupo == {{filtro}}) %>%
    mutate(grupo = str_wrap(grupo, width = 30)) %>% 
    mutate(
      periodo = case_when(
        cohorte_anio %in% c(2016,2017) ~ "P1",
        cohorte_anio %in% c(2018,2019) ~ "P2",
        cohorte_anio %in% c(2020) ~ "P3",
        cohorte_anio %in% c(2021) ~ "P4",
        cohorte_anio %in% c(2022) ~ "P5"),
      periodo = factor(periodo, levels = c("P1","P2","P3","P4","P5")),
      periodo_anio = case_when(
        periodo=="P1" ~ "2016-2017",
        periodo=="P2" ~ "2018-2019",
        periodo=="P3" ~ "2020",
        periodo=="P4" ~ "2021",
        periodo=="P5" ~ "2022")
    ) %>% 
    mutate(
      categoria = case_when(
        periodo=="P1" ~ "nuevas",
        periodo=="P2" & p1_act=="Si" ~ "recurrentes_p1",
        periodo=="P2" & p1_act=="No" ~ "nuevas",
        periodo=="P3" & p1_act=="Si" ~ "recurrentes_p1",
        periodo=="P3" & p2_act=="Si" ~ "recurrentes_p2",
        periodo=="P3" & p1_act=="No" & p2_act=="No" ~ "nuevas",
        periodo=="P4" & p1_act=="Si" ~ "recurrentes_p1",
        periodo=="P4" & p2_act=="Si" ~ "recurrentes_p2",
        periodo=="P4" & p3_act=="Si" ~ "recurrentes_p3",
        periodo=="P4" & p1_act=="No" & p2_act=="No" & p3_act=="No" ~ "nuevas",
        periodo=="P5" & p1_act=="Si" ~ "recurrentes_p1",
        periodo=="P5" & p2_act=="Si" ~ "recurrentes_p2",
        periodo=="P5" & p3_act=="Si" ~ "recurrentes_p3",
        periodo=="P5" & p4_act=="Si" ~ "recurrentes_p4",
        periodo=="P5" & p1_act=="No" & p2_act=="No" & p3_act=="No" & p4_act=="No" ~ "nuevas"),
      categoria = as.factor(categoria)
    )
  
}

mi_df <- map_df(filtros, mi_func)

recurrencia_periodos_grupo2 <- mi_df %>% 
  group_by(grupo, periodo, categoria) %>% 
  summarise(n = n_distinct(idpersona)) %>% 
  ggplot(aes(x = periodo, y = n, fill=categoria)) +
  geom_col() +
  ggthemes::scale_fill_tableau() +
  labs(y = NULL) +
  guides(fill = "none") +
  ggtitle("Recurrencia por grupo de propuestas") +
  facet_wrap(vars(grupo)) +
  theme_minimal(base_size = 17) +
  theme(
    strip.background = element_rect(color = "black"),
    strip.text = element_text(size = 12),
    plot.title.position = "plot"
  )

recurrencia_periodos_grupo2


plot_total <- recurrencia_periodos_plot + recurrencia_periodos_grupo2 +
  plot_layout(widths = c(1.6,3))


ggsave("plot_total.png", plot_total, width = 20, height = 12, dpi = 320)



# segundo approach (personas) ----

datos_personas <- datos %>% 
  mutate(
    periodo = case_when(
      cohorte_anio %in% c(2016) ~ "2016",
      cohorte_anio %in% c(2017) ~ "2017",
      cohorte_anio %in% c(2018) ~ "2018",
      cohorte_anio %in% c(2019) ~ "2019",
      cohorte_anio %in% c(2020) ~ "2020",
      cohorte_anio %in% c(2021) ~ "2021",
      cohorte_anio %in% c(2022) ~ "2022"),
    periodo = factor(periodo, levels = c("2016","2017","2018","2019","2020","2021","2022"))
  ) %>% 
  left_join(datos_agrupaciones)


analisis_grupo <- function(filtro){
  
  datos_personas %>% 
    filter(grupo == {{filtro}}) %>% 
    group_by(idpersona) %>% 
    arrange(periodo) %>% 
    mutate(prev = lag(periodo)) %>% 
    ungroup()
  
}

filtros <- datos_agrupaciones %>% 
  pull(grupo) %>% 
  unique()

dfs <- map_df(filtros, analisis_grupo)

recurrencia_x_grupo <- dfs %>% 
  mutate(grupo = str_wrap(grupo, width = 30)) %>% 
  group_by(grupo, periodo) %>% 
  summarise(nuevas = n_distinct(idpersona[is.na(prev)]),
            recurrentes = n_distinct(idpersona[!is.na(prev)])) %>% 
  pivot_longer(cols = c(nuevas, recurrentes), names_to = "categoria", values_to = "n") %>% 
  ggplot(aes(x = periodo, y = n, fill=categoria)) +
  geom_col() +
  ggthemes::scale_fill_tableau() +
  labs(y = NULL) +
  guides(fill = "none") +
  ggtitle("Recurrencia al interior de cada grupo de propuestas") +
  labs(subtitle = "Personas que registran matrícula por <i style='color:#4E79A7'>primera vez</i> y personas <i style='color:#F28E2B'>recurrentes</i>") +
  facet_wrap(vars(grupo)) +
  theme_minimal(base_size = 17) +
  theme(
    strip.background = element_rect(color = "black"),
    strip.text = element_text(size = 12),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.subtitle = element_markdown()
  )

recurrencia_x_grupo


# test ----