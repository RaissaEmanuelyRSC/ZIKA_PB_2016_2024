# Pacotes ------------------------------------------------------------------

# Instala e carrega pacotes
pacotes <- c(
  "tidyverse", "foreign", "DescTools", "summarytools", "gtsummary",
  "dygraphs", "readr", "readxl", "janitor", "skimr", "stringr",
  "stringi", "lubridate", "writexl", "devtools", "modeest", "sf"
)

novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(novos_pacotes)) install.packages(novos_pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# Dados --------------------------------------------------------------------

# Carregando os dados dbf e csv corretamente
anos_dbf <- 16:22
anos_csv <- 23:24

bases_dbf <- map(anos_dbf, ~ read.dbf(glue::glue("Dados/ZIKABR{.x}.dbf")))
bases_csv <- map(anos_csv, ~ read_csv(glue::glue("Dados/ZIKABR{.x}.csv")))

bases <- c(bases_dbf, bases_csv)
nomes_bases <- paste0("zk_", 2016:2024)

# Atribuir as bases às variáveis
walk2(nomes_bases, bases, ~ assign(.x, .y, envir = .GlobalEnv))

Tabela_Munic <- read_excel("Dados/tabela_municipios_PB.xlsx")
Unid_not <- read_excel("Dados/tabela_unidades_PB.xlsx")

# Unindo -------------------------------------------------------------------

lista_bases <- mget(nomes_bases)
todas_colunas <- reduce(map(lista_bases, names), union)

base_unica <- lista_bases %>%
  map(~ .x %>%
        add_column(!!!set_names(rep(list(NA), length(setdiff(todas_colunas, names(.x)))), 
                                setdiff(todas_colunas, names(.x)))) %>%
        select(all_of(todas_colunas)) %>%
        mutate(across(everything(), as.character))) %>%
  bind_rows()

# Alterando códigos municipais para nome municpiais | Unidades notificadoras-----------------------
#Filtrar por UF de notificação 25 (Paraíba)

base_pb <- base_unica |>
  filter(SG_UF_NOT == 25)

base_pb$ID_MN_RESI <- as.factor(base_pb$ID_MN_RESI)
Tabela_Munic$ID_MN_RESI <- as.factor(Tabela_Munic$ID_MN_RESI)

# 1º Join
base_pb <- left_join(
  x = base_pb,
  y = Tabela_Munic,
  by = c("ID_MN_RESI" = "ID_MN_RESI")
)

base_pb <- base_pb |>
  rename( Munic_Resid = Município)
names(base_pb)

# 2º Join (atenção aqui!)
base_pb$ID_MUNICIP <- as.factor(base_pb$ID_MUNICIP)
Tabela_Munic$ID_MN_RESI <- as.factor(Tabela_Munic$ID_MN_RESI)

# Aqui pode haver conflito de nomes
base_pb <- left_join(
  x = base_pb,
  y = Tabela_Munic,
  by = c("ID_MUNICIP" = "ID_MN_RESI")
)

base_pb <- base_pb |>
  rename( Munic_Not = Município)
names(base_pb)
# Verificar estrutura

# 3º Join com unidade
base_pb$ID_UNIDADE <- as.factor(base_pb$ID_UNIDADE)
Unid_not$ID_UNIDADE <- as.factor(Unid_not$ID_UNIDADE)

base_pb <- left_join(
  x = base_pb, 
  y = Unid_not, 
  by = c("ID_UNIDADE" = "ID_UNIDADE")
)

glimpse(base_pb)

# Criando um ID único para cada notificação -------------------------------

base_pb$ID <- 1:nrow(base_pb)
base_pb <- base_pb[, c("ID", setdiff(names(base_pb), "ID"))]


# Organização dos dados  --------------------------------------------------

base_pb <- base_pb |>
  mutate(
    # Extrair unidade e valor
    unidade_idade = str_sub(NU_IDADE_N, 1, 1),
    valor_idade = as.numeric(str_sub(NU_IDADE_N, 2, 4)),
    
    # Calcular idade em anos decimais
    Idade_Anos = case_when(
      unidade_idade == "1" ~ valor_idade / (24 * 365),  # horas
      unidade_idade == "2" ~ valor_idade / 365,         # dias
      unidade_idade == "3" ~ valor_idade / 12,             # meses
      unidade_idade == "4" ~ valor_idade,                  # anos
      TRUE ~ NA_real_
    ),
    
    # Faixas etárias padronizadas
    Fx_Etaria = cut(
      Idade_Anos,
      breaks = c(-Inf, 1, 4, 9, 14, 19, 29, 39, 49, 59, 69, 79, Inf),
      labels = c(
        "<1 ano", "1 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
        "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
        "60 a 69 anos", "70 a 79 anos", "80 anos e mais"
      ),
      right = TRUE
    ),
    
    # Datas e campos epidemiológicos
    DT_SIN_PRI = ymd(DT_SIN_PRI),
    DT_NOTIFIC = ymd(DT_NOTIFIC),
    Mês_Epi = month(DT_SIN_PRI, label = TRUE, abbr = FALSE),
    SE_SINTOMAS = epiweek(DT_SIN_PRI),
    SE_NOTIFIC = epiweek(DT_NOTIFIC),
    Ano_Sintomas = year(DT_SIN_PRI),
    Ano_Notificacao = year(DT_NOTIFIC)
  )
#############################

base_pb$CS_GESTANT <- factor(
  x = base_pb$CS_GESTANT, 
  levels = c("1", "2", "3", "4", "5", "6", "9"),
  labels = c(
    "1ºTrimestre",
    "2ºTrimestre",
    "3ºTrimestre", 
    "Idade gestacional Ignorada",
    "Não", 
    "Não se aplica",
    "Ignorado"
  )
)
#Classificação final 
base_pb$CLASSI_FIN <- factor(
  x = base_pb$CLASSI_FIN, 
  levels = c("1", "2", "8"),
  labels = c(
    "Confirmado",
    "Descartado",
    "8"
  )
)

# Critério de confirmação 
base_pb$CRITERIO <- factor(
  x = base_pb$CRITERIO, 
  levels = c("1", "2"),
  labels = c(
    "Laboratorial",
    "Clínico-Epidemiológico"
  )
)

base_pb$CS_RACA<- factor(
  x = base_pb$CS_RACA, 
  levels = c("1", "2", "3", "4", "5", "9", NA),
  labels = c(
    "Branca",
    "Preta",
    "Amarela", 
    "Parda",
    "Indígena", 
    "Ignorado"
  )
)

base_pb$TPAUTOCTO <- factor(
  x = base_pb$TPAUTOCTO, 
  levels = c("1", "2", "3", NA),
  labels = c(
    "Sim",
    "Não",
    "Indeterminado"
  )
)

base_pb$EVOLUCAO <- factor(
  x = base_pb$EVOLUCAO, 
  levels = c("1", "2", "3", "9", NA),
  labels = c(
    "Cura",
    "Óbito pelo agravo notificado",
    "Óbito por outras causas",
    "Ignorado"
  )
)

# Análise descritiva ------------------------------------------------------

#Sexo por gestante
sexo_gestante <- base_pb |>
  group_by(CS_SEXO, CS_GESTANT ) |>
  summarise(notificacao = n(), .groups = 'drop') |>
  pivot_wider(names_from = CS_SEXO, values_from = notificacao) |>
  adorn_totals(c("row", "col"))

glimpse(sexo_gestante)


sexo_fet <- base_pb |> 
  group_by(CS_SEXO, Fx_Etaria) |>
  summarise(notificacao = n(), .groups = 'drop') |> 
  pivot_wider(names_from = CS_SEXO, values_from = notificacao) |>
  adorn_totals(c("row", "col"))
glimpse(sexo_fet)



raca_cor <- base_pb |> 
  group_by(CS_RACA, TP_NOT) |>
  summarise(notificacao = n(), .groups = 'drop') |> 
  pivot_wider(names_from = CS_RACA, values_from = notificacao) |>
  adorn_totals(c("row", "col"))
glimpse(raca_cor)



autoctone <- base_pb |> 
  group_by(TPAUTOCTO, TP_NOT) |>
  summarise(notificacao = n(), .groups = 'drop') |> 
  pivot_wider(names_from = TPAUTOCTO, values_from = notificacao) |>
  adorn_totals(c("row", "col"))
glimpse(raca_cor)



classificacao <- base_pb |> 
  group_by(CLASSI_FIN, CRITERIO) |> 
  summarise(notificacao = n(),  .groups = 'drop') |>
  pivot_wider(names_from = CLASSI_FIN, values_from = notificacao)|>
  adorn_totals(c("row", "col"))
glimpse(classificacao)


evolucao <- base_pb |> 
  group_by(EVOLUCAO, TP_NOT) |>
  summarise(notificacao = n(), .groups = 'drop') |> 
  pivot_wider(names_from = EVOLUCAO, values_from = notificacao) |>
  adorn_totals(c("row", "col"))
glimpse(evolucao)


#Exportação das tabelas
write_xlsx(raca_cor, "raca_cor.xlsx")
write_xlsx(sexo_fet, "sexo_faixa_etaria.xlsx")
write_xlsx(classificacao, "classificacao.xlsx")
write_xlsx(evolucao, "evolucao.xlsx")
write_xlsx(autoctone, "autoctone.xlsx")

# Gráfico -----------------------------------------------------------------


base_pb |>
  filter( Ano_Sintomas >= 2016)|>
  count(ano = floor_date(DT_SIN_PRI, "year"))|>
  ggplot(aes(x = ano, y = n)) +
  geom_col(fill = "tomato") +
  labs(title = "Casos de Zika por ano", x = "Ano", y = "Nº de casos")

base_pb |> 
  count(Munic_Resid) |>
  arrange(desc(n))


mapa_pb <- read_sf("Dados/PB_Municipios_2024/PB_Municipios_2024.shp")

casos_munic <- base_pb %>%
  mutate(NM_MUN = str_to_upper(Munic_Resid)) %>%
  count(NM_MUN)

mapa_pb <- mapa_pb %>%
  mutate(NM_MUN = str_to_upper(NM_MUN))

library(stringr)

# Criar variável categórica para a faixa de casos
mapa_categ <- mapa_pb %>%
  left_join(casos_munic, by = "NM_MUN") %>%
  mutate(
    faixa_casos = case_when(
      is.na(n) ~ "Sem dados registrados",
      n > 0 & n <= 100 ~ "> 0 e <= 100",
      n > 100 & n < 300 ~ "> 100 e < 300",
      n >= 300 ~ ">= 300",
      TRUE ~ "Sem dados registrados"
    )
  )

# Definir cores conforme legenda que você mostrou na imagem
cores_faixas <- c(
  "> 0 e <= 100" = "#D3EC8C",  # tom azul claro (similar à legenda)
  "> 100 e < 300" = "#FFC000", # tom amarelo claro
  ">= 300" = "#F55801",        # tom vermelho
  "Sem dados registrados" = "white"
)

# Plotar com legenda categórica
mapa_categ %>%
  ggplot() +
  geom_sf(aes(fill = faixa_casos), color = "black") +
  scale_fill_manual(
    values = cores_faixas,
    na.value = "white",
    name = "Legenda:"
  ) +
  labs(title = "") +
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

# Completitude e consistência (qualidade do dado) -------------------------


base_pb %>%
  summarise(
    comp_idade = mean(!is.na(Idade_Anos)) * 100,
    comp_sexo = mean(!is.na(CS_SEXO)) * 100,
    comp_gestante = mean(!is.na(CS_GESTANT)) * 100
  )

# Inconsistência: idade negativa ou gestante homem
base_pb %>%
  filter(Idade_Anos < 0 | (CS_SEXO == "M" & CS_GESTANT != "NA")) %>%
  count()

notificados <- base_pb |> 
  summarise(notificação = n())

notificados_anos <- base_pb %>%
  filter(Ano_Sintomas %in% 1930:2015) |> 
  group_by(Ano_Sintomas) %>%
  summarise(notificacao = n(), .groups = "drop") |> 
  pivot_wider(names_from = Ano_Sintomas, values_from = notificacao) %>%
  adorn_totals(c("row", "col"))

# 2. Tabela de notificações totais por ano (formato longo)
incidencia_ano <- base_pb |> 
  filter(!is.na(Ano_Sintomas)) |> 
  mutate(Ano_Sintomas = as.numeric(Ano_Sintomas)) |> 
  count(Ano_Sintomas, name = "Casos") |> 
  mutate(
    Populacao = case_when(
      Ano_Sintomas %in% 2016:2021 ~ 3766528,
      Ano_Sintomas %in% 2022:2024 ~ 3974687,
      TRUE ~ NA_real_
    ),
    Incidencia_100k = round((Casos / Populacao) * 100000, 1)
  ) |> 
  filter(Ano_Sintomas %in% 2016:2024)

# Transformar para formato longo
dados_longos <- incidencia_ano |> 
  pivot_longer(cols = c(Casos, Populacao, Incidencia_100k),
               names_to = "Indicador", values_to = "Valor") |> 
  mutate(Ano_Sintomas = as.character(Ano_Sintomas)) |> 
  pivot_wider(names_from = Ano_Sintomas, values_from = Valor)

# Calcular estatísticas
estatisticas <- dados_longos %>%
  rowwise() %>%
  mutate(
    Média = round(mean(c_across(`2016`:`2024`), na.rm = TRUE), 1),
    Mediana = median(c_across(`2016`:`2024`), na.rm = TRUE),
    Moda = {
      moda_calc <- mfv(c_across(`2016`:`2024`))
      if (length(moda_calc) > 1 | length(unique(c_across(`2016`:`2024`))) == length(c_across(`2016`:`2024`))) {
        "Amodal"
      } else {
        as.character(moda_calc)
      }
    },
    
    `Desvio Padrão` = round(sd(c_across(`2016`:`2024`), na.rm = TRUE), 1),
    `Coef. Variação (%)` = round((`Desvio Padrão` / Média) * 100, 1)
  ) %>%
  relocate(Indicador, .before = everything())
# Exibir tabela final
print(estatisticas, width = Inf)


# Cálculo de completude ---------------------------------------------------

variaveis_texto <- c("CS_SEXO", "CS_RACA", "CS_GESTANT", "EVOLUCAO", "CRITERIO", "Idade_Anos")
variaveis_data  <- c("DT_NOTIFIC")

# Calcula completude para variáveis de texto
comp_texto <- base_pb %>%
  summarise(across(all_of(variaveis_texto), 
                   ~ 100 * sum(!is.na(.) & . != "" & . != "Ignorado") / n(),
                   .names = "comp_{.col}"))

# Calcula completude para variáveis de data
comp_data <- base_pb %>%
  summarise(across(all_of(variaveis_data), 
                   ~ 100 * sum(!is.na(.)) / n(),
                   .names = "comp_{.col}"))

# Junta tudo e organiza
completude <- bind_cols(comp_texto, comp_data) %>%
  pivot_longer(everything(), 
               names_to = "Variável", 
               values_to = "Completude (%)") %>%
  mutate(`Completude (%)` = round(`Completude (%)`, 1),
         Classificação = case_when(
           `Completude (%)` >= 95 ~ "Excelente",
           `Completude (%)` >= 90 ~ "Boa",
           `Completude (%)` >= 70 ~ "Regular",
           `Completude (%)` >= 50 ~ "Ruim",
           TRUE ~ "Muito Ruim"
         ))

print(completude)


# Consistência ------------------------------------------------------------


base_pb <- base_pb %>%
  mutate(
    # Regra 1: Sexo masculino e gestante
    inconsistencia_gestante = ifelse(CS_SEXO == "Masculino" & CS_GESTANT == "1", 1, 0),
    
    # Regra 2: Óbito sem data de óbito
    inconsistencia_obito = ifelse(EVOLUCAO %in% c("2", "3") & (is.na(DT_OBITO) | DT_OBITO == ""), 1, 0),
    
    # NOVA Regra 3: Data dos sintomas muito distante da data de notificação (> 60 dias)
    inconsistencia_data_sintomas = ifelse(
      !is.na(DT_SIN_PRI) & !is.na(DT_NOTIFIC) &
        as.numeric(DT_NOTIFIC - DT_SIN_PRI) > 60,
      1, 0
    )
  )

# Calculando a consistência
consistencia_resultado <- base_pb %>%
  summarise(
    consistencia_gestante          = 100 * (1 - sum(inconsistencia_gestante, na.rm = TRUE) / n()),
    consistencia_obito             = 100 * (1 - sum(inconsistencia_obito, na.rm = TRUE) / n()),
    consistencia_data_sintomas     = 100 * (1 - sum(inconsistencia_data_sintomas, na.rm = TRUE) / n())
  ) %>%
  pivot_longer(everything(), names_to = "Regra", values_to = "Consistência (%)") %>%
  mutate(
    `Consistência (%)` = round(`Consistência (%)`, 1),
    Classificação = case_when(
      `Consistência (%)` >= 90 ~ "Excelente",
      `Consistência (%)` >= 70 ~ "Regular",
      `Consistência (%)` < 70  ~ "Baixa"
    )
  )

print(consistencia_resultado)


base_pb <- base_pb %>%
  mutate(
    DT_ENCERRA = as.Date(DT_ENCERRA),
    DT_NOTIFIC = as.Date(DT_NOTIFIC),
    Tempo_Encerramento = as.numeric(DT_ENCERRA - DT_NOTIFIC),
    Class_Encerramento = case_when(
      is.na(Tempo_Encerramento) ~ "Não informado",
      Tempo_Encerramento <= 60 ~ "Oportuno (≤60 dias)",
      Tempo_Encerramento > 60 ~ "Inoportuno (> 60 dias)",
      TRUE ~ "Não informado"
    )
  )

tab_encerramento <- base_pb %>%
  tabyl(Class_Encerramento) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

print(tab_encerramento)

write_xlsx(tab_encerramento, "tab_encerramento.xlsx")

tabela_se_ano <- base_pb %>%
  filter(Ano_Sintomas >= 2016, Ano_Sintomas <= 2024) %>%
  count(SE_SINTOMAS, Ano_Sintomas) %>%
  pivot_wider(
    names_from = Ano_Sintomas,
    values_from = n,
    values_fill = 0  # preenche os NAs com zero
  ) %>%
  arrange(SE_SINTOMAS)

write_xlsx(tabela_se_ano, "tabela_se_ano.xlsx")


dados_zika <- base_pb %>%
  mutate(
    DT_SIN_PRI = ymd(DT_SIN_PRI),
    Ano = year(DT_SIN_PRI),
    Semana = epiweek(DT_SIN_PRI)
  ) %>%
  filter(Ano >= 2016 & Ano <= 2024) %>%
  count(Ano, Semana, name = "Casos")

base_historica <- dados_zika %>%
  filter(Ano < 2024) %>%
  group_by(Semana) %>%
  summarise(
    media = mean(Casos, na.rm = TRUE),
    dp = sd(Casos, na.rm = TRUE),
    LSC = media + 2 * dp,
    LIC = pmax(media - 2 * dp, 0)  # não pode ser < 0
  )

dados_2024 <- dados_zika %>%
  filter(Ano == 2024) %>%
  select(Semana, Casos_2024 = Casos)

controle_zika <- base_historica %>%
  left_join(dados_2024, by = "Semana")

library(ggplot2)
install.packages("showtext")
library(showtext)

# se quiser garantir fonte no gráfico exportado

# Se quiser usar Times mesmo fora do R base
# font_add("Times", regular = "times.ttf") 
# showtext_auto()
install.packages("extrafont")
library(extrafont)

# Importa as fontes do sistema
font_import(prompt = FALSE)
loadfonts(device = "win")  # ou "pdf" para salvar em PDF

# Cria o gráfico com Times New Roman
ggplot(controle_zika, aes(x = Semana)) +
  geom_ribbon(aes(ymin = LIC, ymax = LSC, fill = "Zona de controle"), alpha = 0.2, show.legend = TRUE) +
  geom_line(aes(y = media, color = "Média histórica"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Casos_2024, color = "Casos 2024"), size = 1.2) +
  scale_x_continuous(breaks = 1:53) +
  scale_color_manual(
    name = "Linhas",
    values = c("Casos 2024" = "red", "Média histórica" = "black")
  ) +
  scale_fill_manual(
    name = "Faixa",
    values = c("Zona de controle" = "grey70")
  ) +
  labs(
    title = "Diagrama de Controle dos Casos de Zika - Paraíba, 2024",
    x = "Semana Epidemiológica",
    y = "Número de Casos",
    caption = "Fonte: Elaboração própria. LSC = Limite Superior de Controle; LIC = Limite Inferior de Controle"
  ) +
  theme_minimal(base_family = "Times New Roman") +  # <-- Aqui está a mágica
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, size = 8),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


# 1. Base com todos os municípios (nome padronizado)
todos_munic <- mapa_pb %>%
  st_drop_geometry() %>%
  mutate(NM_MUN = str_to_upper(NM_MUN)) %>%
  select(NM_MUN) %>%
  distinct()

# 2. Resumo de casos por município e classificação
classif_munic <- base_pb %>%
  mutate(
    NM_MUN = str_to_upper(Munic_Resid),
    CLASSI_FIN = as.character(CLASSI_FIN)
  ) %>%
  count(NM_MUN, CLASSI_FIN)

# 3. Cruzar com todos os municípios (mantém os 223)
tabela_classif_completa <- todos_munic %>%
  left_join(classif_munic, by = "NM_MUN") %>%
  pivot_wider(
    names_from = CLASSI_FIN,
    values_from = n,
    values_fill = 0
  )
write_xlsx(tabela_classif_completa, "Classificação.xlsx")
