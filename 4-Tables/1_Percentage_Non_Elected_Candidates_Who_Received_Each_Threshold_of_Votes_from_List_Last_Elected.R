# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(tidyr)

# Lista vazia para armazenamento dos dados
eleicoes_proporcionais_federais <- data.frame()

# Baixando os dados ------------------------------------------------------------

# Carregando os dados 
for(ciclos_eleitorais in seq(1998, 2022, 4)){
  
  cat("Lendo eleições proporcionais federais", ciclos_eleitorais, "\n")
  
  # Carregando os dados
  temp <- read.csv(paste0("https://raw.githubusercontent.com/CedricAntunes/MSc-Thesis-Making-Candidates-Count/main/3-Data/deputados_federais_ranqueados_",
                          ciclos_eleitorais,
                          ".csv"),
                   encoding = "UTF-8")
  
  # Empilhando os dados
  eleicoes_proporcionais_federais <- rbind(eleicoes_proporcionais_federais, temp)
  
  # Deletando temp
  rm(temp)
}

# Dropando indexador
eleicoes_proporcionais_federais <- eleicoes_proporcionais_federais |>
  select(-1) |>
  # Padronizando descrição de situação de totalização de turno
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO %in% c("MÉDIA", 
                                                               "ELEITO POR MÉDIA",
                                                               "ELEITO POR QP"), 
                                     "ELEITO", 
                                     DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# Manipulação dos dados --------------------------------------------------------

# Identificando apenas candidatos eleitos 
ELEITOS <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO == "ELEITO")

# Identificando último eleito, para cada lista, em cada distrito, em cada ano eleitoral
ULTIMOS_ELEITOS <- ELEITOS |>
  group_by(IDENTIFICADOR_LEGENDA, 
           UF, 
           ANO_ELEICAO) |>
  filter(TOTAL_VOTOS_NOMINAIS == min(TOTAL_VOTOS_NOMINAIS)) |>
  ungroup() |>
  mutate(ULTIMO_ELEITO_LISTA = 1)

# Join com a base original 
eleicoes_proporcionais_federais <- eleicoes_proporcionais_federais |>
  left_join(ULTIMOS_ELEITOS |>
              select(ID_CEPESP, 
                     IDENTIFICADOR_LEGENDA, 
                     UF, 
                     ANO_ELEICAO, 
                     ULTIMO_ELEITO_LISTA),
            by = c("ID_CEPESP", 
                   "IDENTIFICADOR_LEGENDA", 
                   "UF", 
                   "ANO_ELEICAO")) |>
  mutate(ULTIMO_ELEITO_LISTA = ifelse(is.na(ULTIMO_ELEITO_LISTA), 0, ULTIMO_ELEITO_LISTA))

# Extraindo votos dos últimos eleitos de cada lista 
ULTIMOS_ELEITOS_VOTOS <- eleicoes_proporcionais_federais |>
  filter(ULTIMO_ELEITO_LISTA == 1) |>
  select(IDENTIFICADOR_LEGENDA, 
         UF, 
         ANO_ELEICAO, 
         TOTAL_VOTOS_NOMINAIS) |>
  rename(ULTIMO_ELEITO_TOTAL_VOTOS_NOMINAIS = TOTAL_VOTOS_NOMINAIS)

# Join com a base original: votação percentual em relação à votação do
# último eleito da lista
PERCENTUAIS <- eleicoes_proporcionais_federais |>
  left_join(ULTIMOS_ELEITOS_VOTOS, 
            by = c("IDENTIFICADOR_LEGENDA", 
                   "UF", 
                   "ANO_ELEICAO")) |>
  mutate(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO = TOTAL_VOTOS_NOMINAIS / ULTIMO_ELEITO_TOTAL_VOTOS_NOMINAIS * 100)

# Filtrando candidatos não eleitos
PERCENTUAIS_NAO_ELEITOS <- PERCENTUAIS |>
  filter(DESC_SIT_TOT_TURNO %in% c("NÃO ELEITO", 
                                   "SUPLENTE"))

# Identificando limiares apenas para não eleitos 
LIMIARES_NAO_ELEITOS <- PERCENTUAIS_NAO_ELEITOS |>
  mutate(ACIMA_DE_90 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 90, 1, 0),
         ACIMA_DE_80 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 80, 1, 0),
         ACIMA_DE_70 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 70, 1, 0),
         ACIMA_DE_60 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 60, 1, 0),
         ACIMA_DE_50 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 50, 1, 0),
         ACIMA_DE_40 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 40, 1, 0),
         ACIMA_DE_30 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 30, 1, 0),
         ACIMA_DE_20 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 20, 1, 0),
         ACIMA_DE_10 = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 10, 1, 0),
         ZERO = ifelse(PERCENTUAL_VOTOS_NOMINAIS_ULTIMO_ELEITO >= 0, 1, 0))

# Sumarizando resultados por ano eleitoral 
TABELA_SUMARIO_NAO_ELEITOS <- LIMIARES_NAO_ELEITOS |>
  group_by(ANO_ELEICAO) |>
  summarise(
    CANDIDATOS_ACIMA_DE_90 = sum(ACIMA_DE_90, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_80 = sum(ACIMA_DE_80, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_70 = sum(ACIMA_DE_70, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_60 = sum(ACIMA_DE_60, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_50 = sum(ACIMA_DE_50, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_40 = sum(ACIMA_DE_40, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_30 = sum(ACIMA_DE_30, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_20 = sum(ACIMA_DE_20, na.rm = TRUE),
    CANDIDATOS_ACIMA_DE_10 = sum(ACIMA_DE_10, na.rm = TRUE),
    CANDIDATOS_ZERO = sum(ZERO, na.rm = TRUE)
  )

# Calculando o número total de não eleitos por ano eleitoral
TOTAL_NAO_ELEITOS <- PERCENTUAIS_NAO_ELEITOS |>
  group_by(ANO_ELEICAO) |>
  summarise(TOTAL_NAO_ELEITOS = n())

# Calculando a porcentagem de candidatos em cada limiar em relação ao total de não eleitos
TABELA_SUMARIO_NAO_ELEITOS <- TABELA_SUMARIO_NAO_ELEITOS |>
  left_join(TOTAL_NAO_ELEITOS, by = "ANO_ELEICAO") |>
  mutate(across(starts_with("CANDIDATOS_"), ~ round(. / TOTAL_NAO_ELEITOS * 100, 1)))

# Gerando tabela final com os anos em linhas e limiares em colunas
TABELA_FINAL_NAO_ELEITOS <- TABELA_SUMARIO_NAO_ELEITOS |>
  select(ANO_ELEICAO, starts_with("CANDIDATOS_")) |>
  pivot_longer(cols = starts_with("CANDIDATOS_"), 
               names_to = "THRESHOLD", 
               values_to = "PERCENTAGE") |>
  mutate(THRESHOLD = case_when(
    THRESHOLD == "CANDIDATOS_ACIMA_DE_90" ~ "90%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_80" ~ "80%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_70" ~ "70%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_60" ~ "60%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_50" ~ "50%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_40" ~ "40%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_30" ~ "30%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_20" ~ "20%",
    THRESHOLD == "CANDIDATOS_ACIMA_DE_10" ~ "10%",
    THRESHOLD == "CANDIDATOS_ZERO" ~ "0%"
  )) |>
  pivot_wider(names_from = THRESHOLD, 
              values_from = PERCENTAGE, 
              values_fill = 0)


# Salvando o dado ---------------------------------------------------------

# Salvando dado
write.csv(TABELA_FINAL_NAO_ELEITOS, "votos_nao_eleitos_percentual_ultimo_eleito.csv")
saveRDS(TABELA_FINAL_NAO_ELEITOS, "votos_nao_eleitos_percentual_ultimo_eleito")
