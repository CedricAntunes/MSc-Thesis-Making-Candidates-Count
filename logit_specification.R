# AUTOR: CEDRIC ANTUNES (FGV-SP)
# DATA: ABRIL DE 2025

# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(rdrobust)
library(rddensity)
library(stargazer)
library(rddensity)
library(arrow)
library(parquetize)
library(tidyr)
library(ggplot2)
library(caret)
library(pROC)
library(lmtest)
library(sandwich)

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

# ------------------------------------------------------------------------------
# BASE DE RECEITAS -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Lista vazia para empilhamento dos dados 
receitas_deputados_federais <- data.frame()

# Carregando e empilhando os dados de cada ano eleitoral 
for (ciclos_eleitorais in seq(2002, 2022, 4)) {
  
  cat("Lendo receitas de campanha de deputados federais", ciclos_eleitorais, "\n")
  
  # Carregando os dados 
  temp_receitas <- read_parquet(paste0("F:/Public/Documents/repositorioTSE/data/output/Candidatos/Receitas/JoinFinal/receitas_final_",
                                       ciclos_eleitorais,
                                       ".parquet"))
  
  # Empilhando os dados 
  receitas_deputados_federais <- rbind(receitas_deputados_federais, temp_receitas)
  
  # Removendo dados temporários 
  rm(temp_receitas)
}

# Removendo casos em que não há ID_CEPESP
receitas_deputados_federais <- receitas_deputados_federais |>
  filter(!is.na(ID_CEPESP) & DESCRICAO_CARGO == "DEPUTADO FEDERAL")

# Somando receitas totais de cada candidato 
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

# 2002 -------------------------------------------------------------------------
receitas_por_candidato_2002 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2002")

largest_drop_2002 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2002")

base_final_2002 <- largest_drop_2002 |>
  left_join(receitas_por_candidato_2002,
            by = c("NOME_CANDIDATO",
                   "UF"))

# 2006 -------------------------------------------------------------------------
receitas_por_candidato_2006 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2006")

largest_drop_2006 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2006")

base_final_2006 <- largest_drop_2006 |>
  left_join(receitas_por_candidato_2006,
            by = c("NOME_CANDIDATO",
                   "UF"))

# 2010 -------------------------------------------------------------------------
receitas_por_candidato_2010 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2010")

largest_drop_2010 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2010")

base_final_2010 <- largest_drop_2010 |>
  left_join(receitas_por_candidato_2010,
            by = c("NOME_CANDIDATO",
                   "UF"))

base_final_2010$RECEITAS_TOTAL_CAMPANHA <- as.numeric(base_final_2010$RECEITAS_TOTAL_CAMPANHA) / 100

# 2014 -------------------------------------------------------------------------
receitas_por_candidato_2014 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2014")

largest_drop_2014 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2014")

base_final_2014 <- largest_drop_2014 |>
  left_join(receitas_por_candidato_2014,
            by = c("NOME_CANDIDATO",
                   "UF"))

base_final_2014$RECEITAS_TOTAL_CAMPANHA <- as.numeric(base_final_2014$RECEITAS_TOTAL_CAMPANHA) / 10

# 2018 -------------------------------------------------------------------------
receitas_por_candidato_2018 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2018")

largest_drop_2018 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2018")

base_final_2018 <- largest_drop_2018 |>
  left_join(receitas_por_candidato_2018,
            by = c("NOME_CANDIDATO",
                   "UF"))

# 2022 -------------------------------------------------------------------------
receitas_por_candidato_2022 <- receitas_por_candidato |>
  filter(ANO_ELEICAO == "2022")

largest_drop_2022 <- eleicoes_proporcionais_federais |>
  filter(ANO_ELEICAO == "2022")

base_final_2022 <- largest_drop_2022 |>
  left_join(receitas_por_candidato_2022,
            by = c("NOME_CANDIDATO",
                   "UF"))

# ------------------------------------------------------------------------------
# CONTINUANDO: EMPILHANDO BASES E AJUSTANDO VARIÁVEIS
# ------------------------------------------------------------------------------

# Juntando todos os anos em uma única base
base_final <- bind_rows(
  base_final_2002,
  base_final_2006,
  base_final_2010,
  base_final_2014,
  base_final_2018,
  base_final_2022
)

# Criando variáveis de interesse
base_final <- base_final |>
  mutate(
    RECEITAS_TOTAL_CAMPANHA = as.numeric(RECEITAS_TOTAL_CAMPANHA),
    log_RECURSO = log(RECEITAS_TOTAL_CAMPANHA + 1),
    VIAVEL_ELEITO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO", 1, 0)
  )

# ------------------------------------------------------------------------------
# IDENTIFICANDO CANDIDATOS VIÁVEIS PELO MAIOR SALTO
# ------------------------------------------------------------------------------

# Criando ID único para merge
base_final <- base_final |>
  mutate(ID_UNICO = paste0(ANO_ELEICAO.y, "_", UF, "_", NOME_CANDIDATO))

eleicoes_proporcionais_federais <- eleicoes_proporcionais_federais |>
  mutate(ID_UNICO = paste0(ANO_ELEICAO, "_", UF, "_", NOME_CANDIDATO))

# Identificando SUPLENTES acima do maior salto
SUPLENTES <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO == "SUPLENTE") |>
  group_by(ANO_ELEICAO, UF, IDENTIFICADOR_LEGENDA) |>
  mutate(
    RANK_VOTOS_NOMINAIS = rank(-TOTAL_VOTOS_NOMINAIS, ties.method = "min"),
    LARGEST_DROP_POSITION = ifelse(all(is.na(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA)), NA, which.max(abs(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA))),
    VOTE_THRESHOLD = TOTAL_VOTOS_NOMINAIS[LARGEST_DROP_POSITION]
  ) |>
  ungroup()

SUPLENTES_ABOVE_DROP <- SUPLENTES |>
  filter(TOTAL_VOTOS_NOMINAIS >= VOTE_THRESHOLD)

ELEITOS <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO == "ELEITO")

VIAVEIS_DROP <- bind_rows(ELEITOS, SUPLENTES_ABOVE_DROP) |>
  mutate(VIAVEL_LARGESTDROP = 1) |>
  select(ID_UNICO, VIAVEL_LARGESTDROP)

# Merge com base_final
base_final <- base_final |>
  left_join(VIAVEIS_DROP, by = "ID_UNICO") |>
  mutate(VIAVEL_LARGESTDROP = ifelse(is.na(VIAVEL_LARGESTDROP), 0, VIAVEL_LARGESTDROP))

# ------------------------------------------------------------------------------
# DISTRICT MAGNITUDE -----------------------------------------------------------
# ------------------------------------------------------------------------------

# Carregando dados de vagas federais 
vagas_federais <- read.csv("https://raw.githubusercontent.com/GV-CEPESP/cepespdata_dados_eleitorais/refs/heads/main/tabelas_auxiliares/2-Dados/vagas_depfed_aux.csv?token=GHSAT0AAAAAAC5FSITKLRVQM5RVLLOWZWOS2ASEMJA") |>
  # Selecionando apenas vagas de 1998, dado que é constante entre todos os ciclos eleitorais
  select(SG_UF,
         VAGAS_1998) |>
  # Renomeando variáveis 
  rename(SIGLA_UE = SG_UF,
         N_VAGAS_FEDERAIS = VAGAS_1998)

# Join --------------------------------------------------------------------
base_final <- left_join(base_final,
                        vagas_federais,
                        by = "SIGLA_UE")

# ------------------------------------------------------------------------------
# REGRESSÕES LOGÍSTICAS BINÁRIAS
# ------------------------------------------------------------------------------

# Removendo candidatos sem receita declarada
base_final_model <- base_final |>
  filter(!is.na(log_RECURSO) & !is.na(VIAVEL_ELEITO) & !is.na(VIAVEL_LARGESTDROP))

modelo_eleito <- glm(VIAVEL_ELEITO ~ log_RECURSO, data = base_final_model, family = "binomial")

modelo_eleito_controles <- glm(VIAVEL_ELEITO ~ log_RECURSO +
                                 N_TOTAL_COMPETIDORES_DISTRITO +
                                 N_VAGAS_FEDERAIS,
                               data = base_final_model,
                               family = "binomial")

modelo_viavel <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO, data = base_final_model, family = "binomial")

modelo_viavel_controles <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO +
                                 N_TOTAL_COMPETIDORES_DISTRITO +
                                 N_VAGAS_FEDERAIS,
                               data = base_final_model,
                               family = "binomial")

# ------------------------------------------------------------------------------
# RESULTADOS
# ------------------------------------------------------------------------------

# modelos de base 
summary(modelo_eleito)
summary(modelo_viavel)

# Razões de chances (odds ratios)
exp(coef(modelo_eleito))
exp(confint(modelo_eleito))

exp(coef(modelo_viavel))
exp(confint(modelo_viavel))

# ------------------------------------------------------------------------------
# PROBABILIDADES PREDITAS E PLOTS
# ------------------------------------------------------------------------------

base_final_model <- base_final_model |>
  mutate(
    prob_pred_eleito = predict(modelo_eleito, type = "response"),
    prob_pred_viavel = predict(modelo_viavel, type = "response")
  )

ggplot(base_final_model, aes(x = log_RECURSO, y = prob_pred_eleito)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Probabilidade Prevista de Ser Eleito",
       x = "Log(Receita de Campanha + 1)",
       y = "Probabilidade") +
  theme_minimal()

ggplot(base_final_model, aes(x = log_RECURSO, y = prob_pred_viavel)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Probabilidade Prevista do Candidato Ser Viável",
       x = "Log(Receita de Campanha + 1)",
       y = "Probabilidade") +
  theme_minimal()

# ------------------------------------------------------------------------------
# CONFUSION MATRIX -------------------------------------------------------------
# ------------------------------------------------------------------------------
# Predicted classes (threshold = 0.5)
pred_class_viavel <- ifelse(predict(modelo_viavel, type = "response") >= 0.5, 1, 0)

# Actual classes
actual_class <- base_final_model$VIAVEL_LARGESTDROP

# Confusion matrix
confusionMatrix(factor(pred_class_viavel), factor(actual_class), positive = "1")

# ------------------------------------------------------------------------------
# ROC CURVE --------------------------------------------------------------------
# ------------------------------------------------------------------------------

roc_viavel <- roc(actual_class, predict(modelo_viavel, type = "response"))

roc_eleito <- roc(actual_class, predict(modelo_eleito, type = "response"))

# Plot ROC
plot(roc_viavel, col = "blue", main = "ROC Curve - Viável")
auc(roc_viavel)

plot(roc_eleito, col = "blue", main = "ROC Curve - Elected")
auc(roc_eleito)

# ------------------------------------------------------------------------------
# QUADRATIC SPECIFICATION ------------------------------------------------------
# ------------------------------------------------------------------------------
modelo_quad <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO + I(log_RECURSO^2), 
                   data = base_final_model, 
                   family = "binomial")

summary(modelo_quad)

modelo_quad_controles <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO + 
                               I(log_RECURSO^2) +
                               N_TOTAL_COMPETIDORES_DISTRITO +
                               N_VAGAS_FEDERAIS, 
                             data = base_final_model, 
                             family = "binomial")

# ------------------------------------------------------------------------------
# DROPPING TOP 1% --------------------------------------------------------------
# ------------------------------------------------------------------------------
top_1_percent <- quantile(base_final_model$RECEITAS_TOTAL_CAMPANHA, 0.99, na.rm = TRUE)

base_sens <- base_final_model |>
  filter(RECEITAS_TOTAL_CAMPANHA <= top_1_percent)

modelo_sens <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO, data = base_sens, family = "binomial")

summary(modelo_sens)

modelo_sens_controles <- glm(VIAVEL_LARGESTDROP ~ log_RECURSO +
                               N_TOTAL_COMPETIDORES_DISTRITO +
                               N_VAGAS_FEDERAIS, 
                             data = base_sens, 
                             family = "binomial")

# ------------------------------------------------------------------------------
# DECILE PLOT ------------------------------------------------------------------
# ------------------------------------------------------------------------------
base_final_model <- base_final_model |>
  mutate(prob_pred_viavel = predict(modelo_viavel, type = "response"),
         decile = ntile(log_RECURSO, 10))

decile_summary <- base_final_model |>
  group_by(decile) |>
  summarize(
    avg_prob = mean(prob_pred_viavel, na.rm = TRUE),
    avg_resource = mean(RECEITAS_TOTAL_CAMPANHA, na.rm = TRUE)
  )

ggplot(decile_summary, aes(x = decile, y = avg_prob)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Predicted Probability of Viability by Resource Decile",
       x = "Decile of Log(Campaign Resources)",
       y = "Predicted Probability") +
  theme_minimal()

# ------------------------------------------------------------------------------
# ROBUST SE --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Clustered by UF
coeftest(modelo_viavel, vcov = vcovCL, cluster = ~UF, data = base_final_model)

# Clustered by year
coeftest(modelo_viavel, vcov = vcovCL, cluster = ~ANO_ELEICAO, data = base_final_model)

# ------------------------------------------------------------------------------
# FINAL TABLES -----------------------------------------------------------------
# ------------------------------------------------------------------------------
stargazer(modelo_eleito, 
          modelo_viavel, 
          modelo_quad, 
          modelo_sens, 
          type = "text", 
          title = "Logit Model",
          dep.var.labels = c("Baseline", "Full Sample"),
          column.labels = c("Elected Only", "All Viable", "All Viable", "Without Top 1%"),
          covariate.labels = c("Campaign revenue", "Campaign revenue quadratic"),
          out = "robustness_checks.txt")

stargazer(modelo_viavel_controles, 
          modelo_eleito_controles,
          modelo_quad_controles,
          type = "text",
          dep.var.labels = c("Viable", "Only Elected", "Viable"),
          covariate.labels = c("Campaign Resources",
                               "Campgaign Resources Quadratic",
                               "N Candidates District",
                               "District Magnitude"),
          column.labels = c("Full Sample", "Baseline", "Quadratic", "Without Top 1%"),
          title = "Robustness Test - Models With Controls",
          out = "robustness_checks.txt")
