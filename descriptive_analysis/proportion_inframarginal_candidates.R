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
# WITHIN-LIST PROPORTION OF INFRAMARGINAL CANDIDATES ---------------------------
# ------------------------------------------------------------------------------

# Identificando os votos dos últimos eleitos em cada lista
list_summary <- base_final |>
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA) |>
  summarise(
    last_elected_votes = suppressWarnings(
      min(TOTAL_VOTOS_NOMINAIS[DESC_SIT_TOT_TURNO %in% c("ELEITO", 
                                                         "ELEITO POR MÉDIA", 
                                                         "ELEITO POR QP",
                                                         "MÉDIA")], na.rm = TRUE)
    ),
    district_magnitude = max(N_VAGAS_FEDERAIS, na.rm = TRUE),  
    .groups = "drop"
  ) |>
  mutate(last_elected_votes = ifelse(is.infinite(last_elected_votes), NA, last_elected_votes)) |>
  # Dropando listas sem candidatos eleitos 
  filter(!is.na(last_elected_votes))  

# Calculando a proporção de candidatos infra-marginais por lists
infra_marginal_df <- base_final |>
  inner_join(list_summary, by = c("ANO_ELEICAO", 
                                  "UF", 
                                  "IDENTIFICADOR_LEGENDA")) |>
  mutate(
    # Candidatos que receberam 5% ou menos em votos preferenciais do último
    # eleito da lista
    infra_marginal = TOTAL_VOTOS_NOMINAIS <= (0.05 * last_elected_votes)
  ) |>
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA, 
           district_magnitude) |>
  summarise(
    infra_marginal_count = sum(infra_marginal, na.rm = TRUE),
    total_candidates = n(),
    # Calculando a proporção (%) de candidatos infra-marginais por lista
    infra_marginal_prop = ifelse(total_candidates > 0, infra_marginal_count / total_candidates, NA),  
    .groups = "drop"
  ) |>
  filter(!is.na(infra_marginal_prop))

# Plots
ggplot(infra_marginal_df, aes(x = district_magnitude, y = infra_marginal_prop)) +
  geom_point(alpha = 0.6, fill = "steelblue", color = "black", shape = 21) +  
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "red", size = 1, fill = "pink", alpha = 0.2) +
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +  
  theme_bw() +
  labs(
    title = "Within-List Proportion of Candidates Less than 5% of Last Elected’s Votes Per District Per Electoral Year",
    x = "District Magnitude",
    y = "Within-List Proportion of Candidates Less than 5% of Last Elected’s Votes"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )
