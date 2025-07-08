library(ggplot2)
library(sf)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggspatial)
library(showtext)

install.packages("ggspatial")
install.packages("extrafont")
library(extrafont)

font_import(prompt = FALSE)

loadfonts(device = "win")  

mapa_pb <- read_sf("Dados/PB_Municipios_2024/PB_Municipios_2024.shp") %>%
  mutate(NM_MUN = str_to_upper(NM_MUN))

todos_munic <- mapa_pb %>%
  st_drop_geometry() %>%
  select(NM_MUN) %>%
  distinct()

casos_provaveis <- base_pb %>%
  filter(!str_to_upper(CLASSI_FIN) %in% c("DESCARTADO", "2")) %>%
  mutate(NM_MUN = str_to_upper(Munic_Resid)) %>%
  count(NM_MUN, name = "casos_provaveis")

write_xlsx(casos_provaveis, "casosprovaveis.xlsx")


pop_munic <- read_xlsx("Dados/populacao_municipios_pb_2024.xlsx") %>%
  mutate(NM_MUN = str_to_upper(NM_MUN))


dados_inc <- todos_munic %>%
  left_join(casos_provaveis, by = "NM_MUN") %>%
  left_join(pop_munic, by = "NM_MUN") %>%
  mutate(
    casos_provaveis = replace_na(casos_provaveis, 0),
    Incidencia_100k = round((casos_provaveis / populacao) * 100000, 2)
  )


mapa_categ <- mapa_pb %>%
  left_join(dados_inc, by = "NM_MUN") %>%
  mutate(
    faixa_incidencia = case_when(
      is.na(Incidencia_100k) ~ "Sem dados registrados",
      Incidencia_100k > 0 & Incidencia_100k <= 100 ~ "> 0 e <= 100",
      Incidencia_100k > 100 & Incidencia_100k <= 300 ~ "> 100 e <= 300",
      Incidencia_100k > 300 & Incidencia_100k <= 600 ~ "> 300 e <= 600",
      Incidencia_100k > 600 ~ "> 600",
      TRUE ~ "Sem dados registrados"
    )
  )


cores_mapa_legenda <- c(
  "> 0 e <= 100" = "#d4e79f",     # verde claro
  "> 100 e <= 300" = "#f7c442",   # amarelo
  "> 300 e <= 600" = "#f78c3d",   # laranja médio
  "> 600" = "#d7301f",            # vermelho forte
  "Sem dados registrados" = "white"
)


ggplot(mapa_categ) +
  geom_sf(aes(fill = faixa_incidencia), color = "gray8", size = 0.2) +
  

  scale_fill_manual(
    values = cores_mapa_legenda,
    name = "Incidência por 100 mil"
  ) +
  

  annotation_scale(location = "bl", width_hint = 0.3, 
                   text_cex = 0.7, line_width = 0.5, 
                   text_family = "Times New Roman") +
  

  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering(
                           text_family = "Times New Roman"
                         ),
                         height = unit(1.2, "cm"),
                         width = unit(1.2, "cm")) +
  

  labs(
    title = "Distribuição da incidência dos casos notificados do Vírus Zika, no período de 2016 a 2024, na Paraíba"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14,
      family = "Times New Roman"
    ),
    legend.title = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman")
  )