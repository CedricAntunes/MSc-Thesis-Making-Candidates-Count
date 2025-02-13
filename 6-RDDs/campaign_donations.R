# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(rdrobust)
library(rddensity)
library(stargazer)
library(arrow)
library(parquetize)
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

#-------------------------------------------------------------------------------
# OPERACIONALIZANDO A RUNNING VAR ----------------------------------------------
# ------------------------------------------------------------------------------

library(dplyr)

library(dplyr)

# Function to calculate distance from the largest drop within a list
DISTANCIA_LARGEST_DROP_LISTA <- function(df) {
  # Step 1: Filter only SUPLENTES
  suplentes_df <- df %>%
    filter(DESC_SIT_TOT_TURNO == "SUPLENTE") %>%  # Consider only suplentes
    arrange(desc(TOTAL_VOTOS_NOMINAIS)) %>%  # Order by votes (highest to lowest)
    mutate(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM = TOTAL_VOTOS_NOMINAIS - lag(TOTAL_VOTOS_NOMINAIS)) 
  
  # Step 2: Identify the candidate with the largest drop among suplentes
  largest_drop_candidate <- suplentes_df %>%
    slice_max(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM, n = 1, with_ties = FALSE)  # Select candidate at largest drop
  
  # Step 3: Double-check in case no valid candidate is found
  if (nrow(largest_drop_candidate) == 0) {
    df <- df %>%
      mutate(DISTANCIA_PARA_LARGEST_DROP = NA,
             CANDIDATO_LARGEST_DROP = 0)
  } else {
    # Step 4: Calculate distances from the candidate with the largest drop for ALL candidates in the list
    df <- df %>%
      mutate(DISTANCIA_PARA_LARGEST_DROP = TOTAL_VOTOS_NOMINAIS - largest_drop_candidate$TOTAL_VOTOS_NOMINAIS,
             CANDIDATO_LARGEST_DROP = ifelse(ID_CEPESP == largest_drop_candidate$ID_CEPESP, 1, 0))
  }
  
  return(df)
}

# Apply the function for each list-year
resultados_largest_drop <- eleicoes_proporcionais_federais %>%
  group_by(UF, IDENTIFICADOR_LEGENDA, ANO_ELEICAO) %>%
  group_modify(~ DISTANCIA_LARGEST_DROP_LISTA(.x)) %>%
  ungroup()

# Final dataset with absolute distance included
resultados_final_largest_drop <- resultados_largest_drop %>%
  mutate(DISTANCIA_ABSOLUTA_PARA_LARGEST_DROP = abs(DISTANCIA_PARA_LARGEST_DROP))

resultados_final_largest_drop <- resultados_final_largest_drop |>
  filter(!is.na(DISTANCIA_PARA_LARGEST_DROP)) |>
  mutate(ID_CEPESP = as.character(ID_CEPESP),
         ANO_ELEICAO = as.character(ANO_ELEICAO))

# ------------------------------------------------------------------------------
# BASE DE RECEITAS -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Initialize an empty dataframe for pooling
receitas_deputados_federais <- data.frame()

# Load and pool data for each electoral cycle
for (ciclos_eleitorais in seq(2002, 2022, 4)) {
  
  cat("Lendo receitas de campanha de deputados federais", ciclos_eleitorais, "\n")
  
  # Load the data
  temp_receitas <- read_parquet(paste0("F:/Public/Documents/repositorioTSE/data/output/Candidatos/Receitas/JoinFinal/receitas_final_",
                                       ciclos_eleitorais,
                                       ".parquet"))
  
  # Pool the data
  receitas_deputados_federais <- rbind(receitas_deputados_federais, temp_receitas)
  
  # Remove temporary data
  rm(temp_receitas)
}

# Remove rows where ID_CEPESP is NA
receitas_deputados_federais <- receitas_deputados_federais |>
  filter(!is.na(ID_CEPESP) & DESCRICAO_CARGO == "DEPUTADO FEDERAL")

# Summing total purchases per candidate
receitas_por_candidato <- receitas_deputados_federais |>
  group_by(ANO_ELEICAO,
           ID_CEPESP,
           NOME_CANDIDATO,
           SIGLA_UF) |>
  summarize(RECEITAS_TOTAL_CAMPANHA = sum(VALOR_RECEITA, na.rm = TRUE), .groups = 'drop') |>
  rename(UF = SIGLA_UF)

# ------------------------------------------------------------------------------
# JOIN -------------------------------------------------------------------------
# ------------------------------------------------------------------------------

receitas_por_candidato_2002 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2002")

largest_drop_2002 <- resultados_final_largest_drop |>
  filter(ANO_ELEICAO == "2002")

base_final_2002 <- largest_drop_2002 |>
  left_join(receitas_por_candidato_2002,
            by = c("NOME_CANDIDATO",
                   "UF"))



# Perform the join
base_final <- resultados_final_largest_drop |>
  left_join(receitas_por_candidato, 
            by = c("ID_CEPESP", 
                   "ANO_ELEICAO",
                   "UF"))

# ------------------------------------------------------------------------------
# RDD --------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# 2002 -------------------------------------------------------------------------

# Filter data for the year 2002
rdd_data_2002 <- base_final_2002 %>%
  filter(ANO_ELEICAO.x == "2002")

# Run the RDD using rdrobust()
rdd_result_2002 <- rdrobust(
  y = rdd_data_2002$RECEITAS_TOTAL_CAMPANHA,  # Outcome variable
  x = rdd_data_2002$DISTANCIA_PARA_LARGEST_DROP,  # Running variable
  c = 0,  # Cutoff at the largest drop
  kernel = "triangular",  # Recommended kernel for RDD
  p = 2,  # Local linear regression
  bwselect = "msetwo"  # Mean squared error optimal bandwidth selection
)

# Print summary
summary(rdd_result_2002)

# Generate the RDD plot
rdplot(
  y = rdd_data_2002$RECEITAS_TOTAL_CAMPANHA,
  x = rdd_data_2002$DISTANCIA_PARA_LARGEST_DROP,
  c = 0,  # Cutoff at the largest drop
  p = 2,  # Local linear regression
  binselect = "esmv",  # Even-spaced mean bins
  kernel = "triangular",
  title = "RDD - Electoral Resources and Largest Drop (2002)",
  y.label = "Campaign Resources (RECEITAS_DE_CAMPANHA)",
  x.label = "Distance to Largest Drop (Votes)"
)

# 2006 -------------------------------------------------------------------------

# Filter data for the year 2002
rdd_data_2006 <- base_final_2006 %>%
  filter(ANO_ELEICAO.x == "2006")

# Run the RDD using rdrobust()
rdd_result_2006 <- rdrobust(
  y = rdd_data_2006$RECEITAS_TOTAL_CAMPANHA,  # Outcome variable
  x = rdd_data_2006$DISTANCIA_PARA_LARGEST_DROP,  # Running variable
  c = 0,  # Cutoff at the largest drop
  kernel = "triangular",  # Recommended kernel for RDD
  p = 1,  # Local linear regression
  bwselect = "msetwo"  # Mean squared error optimal bandwidth selection
)

# Print summary
summary(rdd_result_2002)

# Generate the RDD plot
rdplot(
  y = rdd_data_2002$RECEITAS_TOTAL_CAMPANHA,
  x = rdd_data_2002$DISTANCIA_PARA_LARGEST_DROP,
  c = 0,  # Cutoff at the largest drop
  p = 2,  # Local linear regression
  binselect = "esmv",  # Even-spaced mean bins
  kernel = "triangular",
  title = "RDD - Electoral Resources and Largest Drop (2002)",
  y.label = "Campaign Resources (RECEITAS_DE_CAMPANHA)",
  x.label = "Distance to Largest Drop (Votes)"
)





library(dplyr)

# Remove lists where DISTANCIA_PARA_LARGEST_DROP is NA
NAs_check <- resultados_final_largest_drop %>%
  filter(is.na(DISTANCIA_PARA_LARGEST_DROP) & DESC_SIT_TOT_TURNO == "NÃO ELEITO")

# Check the new dataset
summary(filtered_results$DISTANCIA_PARA_LARGEST_DROP)
