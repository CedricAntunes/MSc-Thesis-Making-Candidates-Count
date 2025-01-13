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
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO %in% c("MÉDIA", 
                                                               "ELEITO POR MÉDIA", 
                                                               "ELEITO POR QP"), "ELEITO", DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# Identificando apenas SUPLENTES e criando LARGEST_DROP ------------------------
SUPLENTES <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO == "SUPLENTE") |>
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA) |>  
  mutate(
    # Ranqueando suplentes com base na votação nominal
    RANK_VOTOS_NOMINAIS = rank(-TOTAL_VOTOS_NOMINAIS, ties.method = "min"),
    
    # Calculando a maior quebra dentro de cada lista
    LARGEST_DROP = ifelse(all(is.na(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA)), NA, which.max(abs(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA))),
    LARGEST_DROP_dummy = ifelse(row_number() == LARGEST_DROP, 1, 0)
  ) |>
  ungroup()

# Filtrar apenas suplentes com a maior quebra ----------------------------------
SUPLENTES_LARGEST_DROP <- SUPLENTES |>
  filter(LARGEST_DROP_dummy == 1)

# Função para estaísticas descritivas  -----------------------------------------
calculate_statistics <- function(df, rank_column) {
  return(
    df |>
      summarise(
        Total_N = n(),
        Mean = mean(.data[[rank_column]], na.rm = TRUE),
        Median = median(.data[[rank_column]], na.rm = TRUE),
        Min = min(.data[[rank_column]], na.rm = TRUE),
        Max = max(.data[[rank_column]], na.rm = TRUE),
        SD = sd(.data[[rank_column]], na.rm = TRUE)
      )
  )
}

# Descritivas da maior quebra --------------------------------------------------
descriptive_stats_suplentes_drop <- calculate_statistics(SUPLENTES_LARGEST_DROP, 
                                                         "RANK_VOTOS_NOMINAIS")

# Plot -------------------------------------------------------------------------
plot_largest_drop <- function(df, stats, title, rank_column) {
  ggplot(df, aes_string(x = rank_column)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    geom_vline(aes(xintercept = stats$Mean), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = stats$Median), color = "green", linetype = "dashed", size = 1) +
    labs(title = "Frequency of Within-List Ranks With Largest Drop in Votes Among Suplentes Only", 
         x = "Within-List Rank of the Largest Drop in Votes", 
         y = "Frequency of Within-List Ranks") +
    theme_bw() +
    annotate("text", x = stats$Mean + 0.5, y = Inf, 
             label = paste("Mean:", round(stats$Mean, 1),
                           "\nMedian:", round(stats$Median, 1),
                           "\nSD:", round(stats$SD, 1),
                           "\nMin:", stats$Min,
                           "\nMax:", stats$Max,
                           "\nTotal Lists:", stats$Total_N), 
             color = "red", vjust = 2, hjust = 0)
}

# Histograma da maior quebra entre suplentes -----------------------------------
plot_largest_drop(SUPLENTES_LARGEST_DROP, 
                  descriptive_stats_suplentes_drop, 
                  "Frequency of Largest Drop Ranks Among SUPLENTES", 
                  "RANK_VOTOS_NOMINAIS")

# Plot histograms by year with mean annotations
# Plot histograms by year with mean and max annotations
ggplot(SUPLENTES_LARGEST_DROP, aes(x = RANK_VOTOS_NOMINAIS)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(data = SUPLENTES_LARGEST_DROP %>%
               group_by(ANO_ELEICAO) %>%
               summarise(Mean = mean(RANK_VOTOS_NOMINAIS, na.rm = TRUE),
                         Max = max(RANK_VOTOS_NOMINAIS, na.rm = TRUE)),
             aes(xintercept = Mean), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = SUPLENTES_LARGEST_DROP %>%
              group_by(ANO_ELEICAO) %>%
              summarise(Mean = mean(RANK_VOTOS_NOMINAIS, na.rm = TRUE),
                        Max = max(RANK_VOTOS_NOMINAIS, na.rm = TRUE)),
            aes(x = Mean, y = Inf, label = paste0("Mean: ", round(Mean, 1))),
            color = "red", vjust = 1.5, hjust = -0.2, size = 4) +
  geom_text(data = SUPLENTES_LARGEST_DROP %>%
              group_by(ANO_ELEICAO) %>%
              summarise(Mean = mean(RANK_VOTOS_NOMINAIS, na.rm = TRUE),
                        Max = max(RANK_VOTOS_NOMINAIS, na.rm = TRUE)),
            aes(x = Mean, y = Inf, label = paste0("Max: ", round(Max, 1))),
            color = "blue", vjust = 3, hjust = -0.2, size = 4) +
  facet_wrap(~ANO_ELEICAO, ncol = 2, scales = "free_y") +
  labs(
    title = "Frequency of Within-List Ranks With Largest Drop in Votes Among Suplentes (By Year)",
    x = "Within-List Rank of the Largest Drop in Votes",
    y = "Frequency of Within-List Ranks"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

# Lorenz curve
# Prepare data for Lorenz curve by year
lorenz_yearly <- eleicoes_proporcionais_federais %>%
  group_by(ANO_ELEICAO, UF, IDENTIFICADOR_LEGENDA) %>%
  summarise(
    Total_Votes = sum(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE),
    Elected_Votes = sum(TOTAL_VOTOS_NOMINAIS[DESC_SIT_TOT_TURNO == "ELEITO"], na.rm = TRUE)
  ) %>%
  group_by(ANO_ELEICAO) %>%
  summarise(
    Gini = Gini(Elected_Votes),
    Lorenz_X = list(Lc(Elected_Votes)$p),  # Cumulative proportion of lists
    Lorenz_Y = list(Lc(Elected_Votes)$L)   # Cumulative proportion of votes
  ) %>%
  ungroup()

# Prepare data for ggplot
plot_data <- lorenz_yearly %>%
  unnest(cols = c(Lorenz_X, Lorenz_Y))

# Plot Lorenz curves by year
ggplot(plot_data, aes(x = Lorenz_X, y = Lorenz_Y, color = factor(ANO_ELEICAO))) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(
    title = "Lorenz Curve of Vote Concentration Among Elected Candidates (By Year)",
    x = "Cumulative Proportion of Lists",
    y = "Cumulative Proportion of Votes",
    color = "Year"
  ) +
  scale_color_startrek() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


# ------------------------------------------------------------------------------
# VOTE CONCENTRATION PER YEAR --------------------------------------------------
# ------------------------------------------------------------------------------

# Filter out lists without any elected candidates
vote_concentration <- eleicoes_proporcionais_federais %>%
  group_by(ANO_ELEICAO, UF, IDENTIFICADOR_LEGENDA) %>%
  summarise(
    Total_Votes = sum(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE),
    Total_Elected_Votes = sum(TOTAL_VOTOS_NOMINAIS[DESC_SIT_TOT_TURNO == "ELEITO"], na.rm = TRUE),
    Vote_Concentration = Total_Elected_Votes / Total_Votes
  ) %>%
  filter(Total_Elected_Votes > 0) %>%  # Drop lists without elected candidates
  ungroup()

# Calculate mean vote concentration by year
vote_concentration_means <- vote_concentration %>%
  group_by(ANO_ELEICAO) %>%
  summarise(Mean_Concentration = mean(Vote_Concentration, na.rm = TRUE))

# Plot histograms by year with mean lines and labels
ggplot(vote_concentration, aes(x = Vote_Concentration)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ ANO_ELEICAO, ncol = 3, scales = "free_y") +
  geom_vline(data = vote_concentration_means, aes(xintercept = Mean_Concentration), color = "red", linetype = "dashed", size = 1) +
  geom_text(
    data = vote_concentration_means,
    aes(x = Mean_Concentration, y = Inf, label = paste0("Mean: ", round(Mean_Concentration, 2))),
    color = "red", hjust = -0.2, vjust = 2, size = 3.5
  ) +
  labs(
    title = "Distribution of Vote Concentration Among Elected Candidates by List (By Year)",
    x = "Proportion of Votes Concentrated Among Elected Candidates",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 75

# Filter data to include elect and non-elect with at least 75% of the last elected's votes
vote_concentration_75 <- eleicoes_proporcionais_federais %>%
  group_by(ANO_ELEICAO, UF, IDENTIFICADOR_LEGENDA) %>%
  mutate(
    # Identify the total votes of the last elected candidate in each list
    Last_Elected_Votes = min(TOTAL_VOTOS_NOMINAIS[DESC_SIT_TOT_TURNO == "ELEITO"], na.rm = TRUE),
    
    # Check if a candidate is eligible (elected or >=75% of last elected's votes)
    Eligible = ifelse(
      DESC_SIT_TOT_TURNO == "ELEITO" | TOTAL_VOTOS_NOMINAIS >= 0.75 * Last_Elected_Votes,
      1, 0
    )
  ) %>%
  summarise(
    Total_Votes = sum(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE),
    Eligible_Votes = sum(TOTAL_VOTOS_NOMINAIS[Eligible == 1], na.rm = TRUE),
    Vote_Concentration = Eligible_Votes / Total_Votes
  ) %>%
  filter(Eligible_Votes > 0) %>%  # Drop lists without eligible candidates
  ungroup()

# Calculate mean vote concentration by year
vote_concentration_75_means <- vote_concentration_75 %>%
  group_by(ANO_ELEICAO) %>%
  summarise(Mean_Concentration = mean(Vote_Concentration, na.rm = TRUE))

# Plot histograms by year with mean lines and labels
ggplot(vote_concentration_75, aes(x = Vote_Concentration)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ ANO_ELEICAO, ncol = 3, scales = "free_y") +
  geom_vline(data = vote_concentration_75_means, aes(xintercept = Mean_Concentration), color = "red", linetype = "dashed", size = 1) +
  geom_text(
    data = vote_concentration_75_means,
    aes(x = Mean_Concentration, y = Inf, label = paste0("Mean: ", round(Mean_Concentration, 2))),
    color = "red", hjust = -0.2, vjust = 2, size = 3.5
  ) +
  labs(
    title = "Distribution of Vote Concentration Among Elect + Eligible Non-Elect (By Year)",
    x = "Proportion of Votes Concentrated Among Elect + Eligible Non-Elect",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 50

# Filter data to include elect and non-elect with at least 50% of the last elected's votes
vote_concentration_50 <- eleicoes_proporcionais_federais %>%
  group_by(ANO_ELEICAO, UF, IDENTIFICADOR_LEGENDA) %>%
  mutate(
    # Identify the total votes of the last elected candidate in each list
    Last_Elected_Votes = min(TOTAL_VOTOS_NOMINAIS[DESC_SIT_TOT_TURNO == "ELEITO"], na.rm = TRUE),
    
    # Check if a candidate is eligible (elected or >=50% of last elected's votes)
    Eligible = ifelse(
      DESC_SIT_TOT_TURNO == "ELEITO" | TOTAL_VOTOS_NOMINAIS >= 0.50 * Last_Elected_Votes,
      1, 0
    )
  ) %>%
  summarise(
    Total_Votes = sum(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE),
    Eligible_Votes = sum(TOTAL_VOTOS_NOMINAIS[Eligible == 1], na.rm = TRUE),
    Vote_Concentration = Eligible_Votes / Total_Votes
  ) %>%
  filter(Eligible_Votes > 0) %>%  # Drop lists without eligible candidates
  ungroup()

# Calculate mean vote concentration by year
vote_concentration_50_means <- vote_concentration_50 %>%
  group_by(ANO_ELEICAO) %>%
  summarise(Mean_Concentration = mean(Vote_Concentration, na.rm = TRUE))

# Plot histograms by year with mean lines and labels
ggplot(vote_concentration_50, aes(x = Vote_Concentration)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ ANO_ELEICAO, ncol = 3, scales = "free_y") +
  geom_vline(data = vote_concentration_50_means, aes(xintercept = Mean_Concentration), color = "red", linetype = "dashed", size = 1) +
  geom_text(
    data = vote_concentration_50_means,
    aes(x = Mean_Concentration, y = Inf, label = paste0("Mean: ", round(Mean_Concentration, 2))),
    color = "red", hjust = -0.2, vjust = 2, size = 3.5
  ) +
  labs(
    title = "Distribution of Vote Concentration Among Elect + Eligible Non-Elect (50% Threshold, By Year)",
    x = "Proportion of Votes Concentrated Among Elect + Eligible Non-Elect",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# -----------------------------------------------------------
# cdf

# CDF plot for total votes by lists (by year)
cdf_data <- eleicoes_proporcionais_federais %>%
  group_by(ANO_ELEICAO, IDENTIFICADOR_LEGENDA) %>%
  summarise(Total_Votes = sum(TOTAL_VOTOS_NOMINAIS, na.rm = TRUE)) %>%
  ungroup()

ggplot(cdf_data, aes(x = Total_Votes, color = factor(ANO_ELEICAO))) +
  stat_ecdf(geom = "step", size = 1.2) +  # Empirical CDF
  labs(
    title = "CDF of Total Votes by Lists (By Year)",
    x = "Total Votes",
    y = "Cumulative Proportion of Lists",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
