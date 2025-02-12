# AUTHOR: CEDRIC ANTUNES (FGV-CEPESP)
# DATE: FEBRUARY 2025

# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(rdrobust)
library(rddensity)
library(rddtools)
library(ineq)
library(ggridges)
library(viridis)
library(ggsci)
library(tidyr)
library(ggplot2)

# Lista vazia para armazenamento dos dados
eleicoes_proporcionais_federais <- data.frame()

# Baixando os dados ------------------------------------------------------------
for(ciclos_eleitorais in seq(1998, 2022, 4)){
  cat("Lendo eleições proporcionais federais", ciclos_eleitorais, "\n")
  temp <- read.csv(paste0("https://raw.githubusercontent.com/CedricAntunes/MSc-Thesis-Making-Candidates-Count/main/3-Data/deputados_federais_ranqueados_", ciclos_eleitorais, ".csv"), encoding = "UTF-8")
  eleicoes_proporcionais_federais <- rbind(eleicoes_proporcionais_federais, temp)
  rm(temp)
}

# Dropando indexador e ajustando dados
eleicoes_proporcionais_federais <- eleicoes_proporcionais_federais |>
  select(-1) |>
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO %in% c("MÉDIA", 
                                                               "ELEITO POR MÉDIA", 
                                                               "ELEITO POR QP"), "ELEITO", DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# Carregando dados de vagas federais 
vagas_federais <- read.csv("C:/Users/cedri/Downloads/vagas_depfed_aux.csv") |>
  # Selecionando apenas vagas de 1998, dado que é constante entre todos os ciclos eleitorais
  select(SG_UF,
         VAGAS_1998) |>
  # Renomeando variáveis 
  rename(SIGLA_UE = SG_UF,
         N_VAGAS_FEDERAIS = VAGAS_1998)


# Join --------------------------------------------------------------------
base_final <- left_join(eleicoes_proporcionais_federais,
                        vagas_federais,
                        by = "SIGLA_UE")

# ------------------------------------------------------------------------------
# VOTE SHARE DE PRIMEIROS NÃO-ELEITOS POR ANO ELEITORAL ------------------------
# ------------------------------------------------------------------------------

# Identificando primeiros não-eleitos em cada lista
first_non_elect_df <- base_final |>
  # Filtrando apenas candidatos não eleitos 
  filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO") |>
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA) |>
  # Ordenando não-eleitos dentro da lista com base em votos preferenciais 
  arrange(desc(TOTAL_VOTOS_NOMINAIS), .by_group = TRUE) |>  
  summarise(
    first_non_elect_votes = first(TOTAL_VOTOS_NOMINAIS),  
    total_list_votes = first(TOTAL_VOTOS_LISTA),
    # Identificando o share de votos de primeiros não-eleitos dentro da lista 
    vote_share_first_non_elect = first_non_elect_votes / total_list_votes,  
    district_magnitude = first(N_VAGAS_FEDERAIS),  
    .groups = "drop"
  ) |>
  # Dropand listas de candidadots únicos que não foram eleitos 
  filter(vote_share_first_non_elect < 1)  

# Scatterplot com intervalo de confiança
ggplot(first_non_elect_df, aes(x = district_magnitude, y = vote_share_first_non_elect)) +
  geom_point(alpha = 0.6, fill = "steelblue", color = "black", shape = 21) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "red", size = 1, fill = "pink", alpha = 0.2) +  # Linear fit with confidence interval
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +  # Facet by year
  labs(
    title = "Within-List Vote Share of First Non-Elected Candidate Per District Magnitude Per Electoral Year",
    x = "District Magnitude",
    y = "Vote Share of First Non-Elect as Share of List’s Votes"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )
