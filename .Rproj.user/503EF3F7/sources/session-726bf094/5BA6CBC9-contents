# Pacotes -----------------------------------------------------------------

pacotes <- c(
  "tidyverse", "foreign", "DescTools", "summarytools", "gtsummary",
  "dygraphs", "readr", "readxl", "janitor", "skimr", "stringr", 
  "stringi", "lubridate", "writexl", "modeest", "devtools"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

walk(pacotes, install_if_missing)
install.packages("epitools")

# Importação das bases ---------------------------------------------------

anos_dbf <- 16:22
anos_csv <- 23:24

# Lendo os arquivos .dbf (2016 a 2022)
bases_dbf <- map(anos_dbf, ~ read.dbf(glue::glue("Dados/ZIKABR{.x}.dbf")))

# Lendo os arquivos .csv (2023 e 2024)
bases_csv <- map(anos_csv, ~ read_csv(glue::glue("Dados/ZIKABR{.x}.csv")))

# Juntando tudo
bases <- c(bases_dbf, bases_csv)


Tabela_Munic <- read_excel("Dados/tabela_municipios_PB.xlsx")
Unid_not <- read_excel("Dados/tabela_unidades_PB.xlsx")


# Unificação das bases ---------------------------------------------------

todas_colunas <- reduce(map(bases, names), union)

base_unica <- bases |>
  map(~ .x |>
        add_column(!!!set_names(rep(list(NA), length(setdiff(todas_colunas, names(.x)))), 
                                setdiff(todas_colunas, names(.x)))) |>
        select(all_of(todas_colunas)) |>
        mutate(across(everything(), as.character))
  ) |>
  bind_rows()

# Filtragem Paraíba e junções --------------------------------------------

base_pb <- base_unica |>
  filter(SG_UF_NOT == 25) |>
  mutate(ID_MN_RESI = as.factor(ID_MN_RESI)) |>
  left_join(Tabela_Munic |> mutate(ID_MN_RESI = as.factor(ID_MN_RESI)),
            by = "ID_MN_RESI") |>
  rename(Munic_Resid = Município) |>
  mutate(ID_MUNICIP = as.factor(ID_MUNICIP)) |>
  left_join(Tabela_Munic |> mutate(ID_MN_RESI = as.factor(ID_MN_RESI)),
            by = c("ID_MUNICIP" = "ID_MN_RESI")) |>
  rename(Munic_Not = Município) |>
  mutate(ID_UNIDADE = as.factor(ID_UNIDADE)) |>
  left_join(Unid_not |> mutate(ID_UNIDADE = as.factor(ID_UNIDADE)),
            by = "ID_UNIDADE")

# ID único ---------------------------------------------------------------

base_pb <- base_pb |>
  mutate(ID = row_number()) |>
  relocate(ID)

# Tratamento de variáveis ------------------------------------------------

base_pb <- base_pb |> 
  mutate(
    Idade_Anos = if_else(str_sub(NU_IDADE_N, 1, 1) == "4", 
                         as.numeric(str_sub(NU_IDADE_N, 2, 4)), NA_real_),
    Fx_Etaria = cut(
      Idade_Anos,
      breaks = c(-Inf, 0, 4, 9, 14, 19, 29, 39, 49, 59, 69, 79, Inf),
      labels = c("<1 ano", "1 a 4 anos", "5 a 9 anos", "10 a 14 anos",
                 "15 a 19 anos", "20 a 29 anos", "30 a 39 anos",
                 "40 a 49 anos", "50 a 59 anos", "60 a 69 anos",
                 "70 a 79 anos", "80 anos e mais"),
      right = TRUE
    ),
    DT_SIN_PRI = ymd(DT_SIN_PRI),
    DT_NOTIFIC = ymd(DT_NOTIFIC),
    Mês_Epi = month(DT_SIN_PRI, label = TRUE, abbr = FALSE),
    SE_SINTOMAS = epiweek(DT_SIN_PRI),
    SE_NOTIFIC = epiweek(DT_NOTIFIC),
    Ano_Sintomas = year(DT_SIN_PRI),
    Ano_Notificacao = year(DT_NOTIFIC)
  )

# Fatores ----------------------------------------------------------------

base_pb <- base_pb |> 
  mutate(
    CS_GESTANT = factor(CS_GESTANT, levels = c("1", "2", "3", "4", "5", "6", "9"),
                        labels = c("1ºTrimestre", "2ºTrimestre", "3ºTrimestre",
                                   "Idade gestacional Ignorada", "Não",
                                   "Não se aplica", "Ignorado")),
    CLASSI_FIN = factor(CLASSI_FIN, levels = c("1", "2", "8"),
                        labels = c("Confirmado", "Descartado", "8")),
    CRITERIO = factor(CRITERIO, levels = c("1", "2"),
                      labels = c("Laboratorial", "Clínico-Epidemiológico"))
  )

# Tabelas de análise -----------------------------------------------------

notificados <- base_pb |> summarise(Notificações = n())

notificados_anos <- base_pb |>
  filter(Ano_Sintomas %in% 1930:2015) |>
  count(Ano_Sintomas) |>
  pivot_wider(names_from = Ano_Sintomas, values_from = n) |>
  adorn_totals(c("row", "col"))

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

# Estatísticas -----------------------------------------------------------

dados_longos <- incidencia_ano |>
  pivot_longer(cols = c(Casos, Populacao, Incidencia_100k),
               names_to = "Indicador", values_to = "Valor") |>
  mutate(Ano_Sintomas = as.character(Ano_Sintomas)) |>
  pivot_wider(names_from = Ano_Sintomas, values_from = Valor)

estatisticas <- dados_longos |>
  rowwise() |>
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
    }
  ) |>
  relocate(Indicador)

print(estatisticas, width = Inf)

# Exportações ------------------------------------------------------------

write_xlsx(incidencia_ano, "incidencia_wide.xlsx")
write_xlsx(base_pb, "analise_base.xlsx")
write_xlsx(notificados_anos, "notificados_anos.xlsx")

# Consistência -----------------------------------------------------------

erro_ano <- base_pb |>
  count(Ano_Notificacao, Ano_Sintomas) |>
  pivot_wider(names_from = Ano_Sintomas, values_from = n)

ano_se <- base_pb |>
  count(Ano_Sintomas, SE_SINTOMAS, name = "notificacao") |>
  pivot_wider(names_from = Ano_Sintomas, values_from = notificacao)

inconsistencias <- base_pb |>
  filter(Ano_Sintomas < 2015 | DT_SIN_PRI > DT_NOTIFIC | is.na(DT_SIN_PRI) | is.na(DT_NOTIFIC))

n_inconsistentes <- nrow(inconsistencias)
porcentagem <- round(n_inconsistentes / nrow(base_pb) * 100, 2)

# Tempo de encerramento --------------------------------------------------

base_pb <- base_pb |>
  mutate(
    Class_Encerramento = case_when(
      is.na(Tempo_Encerramento) ~ "Não informado",
      Tempo_Encerramento <= 60 ~ "Oportuno (≤60 dias)",
      Tempo_Encerramento > 60 ~ "Inoportuno (> 61 dias)",
      TRUE ~ "Não informado"
    )
  )

tab_encerramento <- base_pb |>
  tabyl(Class_Encerramento) |>
  adorn_totals("row") |>
  adorn_pct_formatting(digits = 1)

write_xlsx(tab_encerramento, "encerramento.xlsx")

# Análises demográficas --------------------------------------------------

sexo_pe <- base_pb |>
  count(CS_SEXO, CS_GESTANT) |>
  pivot_wider(names_from = CS_SEXO, values_from = n) |>
  adorn_totals(c("row", "col"))

sexo_fet <- base_pb |>
  count(CS_SEXO, Fx_Etaria) |>
  pivot_wider(names_from = CS_SEXO, values_from = n) |>
  adorn_totals(c("row", "col"))

classificacao <- base_pb |>
  count(CLASSI_FIN, CRITERIO) |>
  pivot_wider(names_from = CLASSI_FIN, values_from = n) |>
  adorn_totals(c("row", "col"))

# Fim do script ----------------------------------------------------------



