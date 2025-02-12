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
# COEFFICIENT OF VARIATION FOR EACH LIST ---------------------------------------
# ------------------------------------------------------------------------------

# Calculando coeficiente de variação (CV) para cada lista
vote_inequality_df <- base_final |>
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA) |>
  summarise(
    # Média de votos na lista
    vote_mean = mean(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE), 
    # SD de votos na lista 
    vote_sd = sd(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE),  
    # CV da lista 
    cv_votes = ifelse(vote_mean > 0, vote_sd / vote_mean, NA),  
    district_magnitude = max(N_VAGAS_FEDERAIS, na.rm = TRUE),  
    .groups = "drop"
  ) |>
  filter(!is.na(cv_votes) & !is.na(district_magnitude))  # Remove missing values

# Plotando CV por M por ano eleitoral 
ggplot(vote_inequality_df, aes(x = district_magnitude, y = cv_votes)) +
  geom_point(alpha = 0.6, fill = "steelblue", color = "black", shape = 21) +  
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "red", size = 1, fill = "pink", alpha = 0.2) +  
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +  
  theme_bw() +
  labs(
    title = "Within-List Vote Inequality (CV) Per District Magnitude Per Electoral Year",
    x = "District Magnitude",
    y = "Within-List Coefficient of Variation of Votes (CV)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )

# Histograms -------------------------------------------------------------------

# Descritivas: Média
mean_cv_per_year <- vote_inequality_df |>
  group_by(ANO_ELEICAO) |>
  summarise(mean_cv = mean(cv_votes, na.rm = TRUE), .groups = "drop")

# Plotando CV por M por ano eleitoral 
ggplot(vote_inequality_df, aes(x = cv_votes)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(data = mean_cv_per_year, aes(xintercept = mean_cv), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = mean_cv_per_year, aes(x = mean_cv, y = Inf, label = paste0("Mean: ", round(mean_cv, 2))),
            color = "red", vjust = 1.5, hjust = -0.2, size = 4) +
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +  
  theme_bw() +
  labs(
    title = "Within-List Vote Inequality (CV) Per District Magnitude Per Electoral Year",
    x = "Within-List Coefficient of Variation (CV) of Votes",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )

# Density ----------------------------------------------------------------------

# Criando breaks de magnitudes
vote_inequality_df <- vote_inequality_df %>%
  mutate(
    magnitude_group = cut(district_magnitude, breaks = c(0, 10, 20, 30, 40, 50, 60, 80), 
                          labels = c("M < 10", "10 < M < 20", "20 < M < 30", "30 < M < 40", 
                                     "40 < M < 50", "50 < M < 60", "M > 60"), 
                          include.lowest = TRUE)
  )

# Plot
ggplot(vote_inequality_df, aes(x = cv_votes, y = magnitude_group, fill = magnitude_group)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  scale_fill_viridis_d(option = "C") +  # Color scale for district magnitudes
  theme_bw() +
  labs(
    title = "Overall Within-List Vote Inequality (CV) Per District Magnitude",
    x = "Within-List Coefficient of Variation (CV) of Votes",
    y = "District Magnitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none"  
  )
