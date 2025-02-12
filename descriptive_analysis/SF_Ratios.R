# AUTHOR: CEDRIC ANTUNES (FGV-CEPESP)
# DATE: FEBRUARY 2025

# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(ineq)
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
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO %in% c("NÃO ELEITO",
                                                               "SUPLENTE"), "NÃO ELEITO", DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# ------------------------------------------------------------------------------
# COX SF RATIOS ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# Estimando razões SF dentro de cada lista para cada ano eleitoral
sf_ratio_df <- eleicoes_proporcionais_federais |>
  # Filtrando apenas candidatos não eleitos 
  filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO") |>  
  group_by(ANO_ELEICAO, 
           SIGLA_UE, 
           IDENTIFICADOR_LEGENDA) |>
  # Ranqueando-os com base em votos preferenciais 
  arrange(desc(TOTAL_VOTOS_NOMINAIS), .by_group = TRUE) |>  
  summarise(
    # Primeiro não-eleito de cada lista
    first_loser_votes = TOTAL_VOTOS_NOMINAIS[1],
    # Segundo não eleito de cada lista 
    second_loser_votes = ifelse(n() > 1, TOTAL_VOTOS_NOMINAIS[2], NA),  
    .groups = "drop"
  ) |>
  mutate(SF_ratio = second_loser_votes / first_loser_votes) |>
  filter(!is.na(SF_ratio))  # Remove NA cases

# Calculando estatísticas descritivas para cada ano eleitoral
sf_summary <- sf_ratio_df |>
  group_by(ANO_ELEICAO) |>
  summarise(
    Mean = mean(SF_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# Plotando razões SF por ano eleitoral 
ggplot(sf_ratio_df, aes(x = SF_ratio)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  # Addicionando linha vertical para média
  geom_vline(data = sf_summary, aes(xintercept = Mean), color = "red", linetype = "dashed", size = 1) +
  # Adicionando média
  geom_text(data = sf_summary, aes(x = Mean, y = Inf, label = paste0("Mean: ", round(Mean, 2))),
            color = "red", vjust = 1.5, hjust = -0.2, size = 4) +
  # Criando uma visualização únicas para cada ano eleitoral 
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +
  # Labels 
  labs(
    title = "Distribution of Within-List Cox's SF Ratios (Second-First Loser) by Electoral Year",
    x = "Cox's SF Ratio (Second Loser Votes / First Loser Votes)",
    y = "Frequency of Ratio Occurrence"
  ) +
  
  # Styling
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )
