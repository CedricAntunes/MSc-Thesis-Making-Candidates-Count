# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(plm)
library(lme4)
library(tidyr)
library(stargazer)
library(ggplot2)

# Lista vazia para armazenamento dos dados
eleicoes_proporcionais_federais <- data.frame()

# Baixando os dados -------------------------------------------------------

# Carregando os dados 
for(ciclos_eleitorais in seq(1998, 2022, 4)){
  
  cat("Lendo eleições proporcionais federais", ciclos_eleitorais, "\n")
  
  # Carregando os dados
  temp <- read.csv(paste0("https://raw.githubusercontent.com/CedricAntunes/MSc-Thesis-Making-Candidates-Count/main/3-Data/deputados_federais_ranqueados_",
                          ciclos_eleitorais,
                          ".csv"),
                   encoding = "latin1")
  
  # Empilhando os dados
  eleicoes_proporcionais_federais <- rbind(eleicoes_proporcionais_federais,
                                           temp)
  
  # Deletando temp
  rm(temp)
}

# Dropando indexador
eleicoes_proporcionais_federais <- eleicoes_proporcionais_federais |>
  select(-1) |>
  # Padronizando descrição de situação de totalização de turno
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO %in% c("MÉDIA", 
                                                               "ELEITO POR MÉDIA"), 
                                     "ELEITO POR QP", 
                                     DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# Carregando dados de vagas federais 
vagas_federais <- read.csv("C:/Users/cedri/OneDrive/Documentos/MSc_Thesis/vagas_depfed_aux.csv") |>
  # Selecionando apenas vagas de 1998, dado que é constante entre todos os ciclos eleitorais
  select(SG_UF,
         VAGAS_1998) |>
  # Renomeando variáveis 
  rename(UF = SG_UF,
         N_VAGAS_FEDERAIS = VAGAS_1998)



# Join --------------------------------------------------------------------
base_final <- left_join(eleicoes_proporcionais_federais,
                        vagas_federais,
                        by = "UF") |>
  mutate(DISTANCIA_ABSOLUTA_PARA_ULTIMO_ELEITO = abs(DISTANCIA_PARA_ULTIMO_ELEITO_EM_VOTOS))

# Removendo base original
rm(eleicoes_proporcionais_federais)

# Removendo base de cadeiras federais
rm(vagas_federais)



# Manipulação dos dados --------------------------------------------------------

# Criando limiares do quociente eleitoral 
thresholds <- c(200, 150, 100, 90, 75, 50, 25, 5) / 100

# Calculando quociente eleitoral para cada distrito e ano eleitoral
base_final <- base_final |>
  # Agredando dados port distrito e ano eleitoral
  group_by(ANO_ELEICAO,
           UF) |>
  mutate(EQ = sum(TOTAL_VOTOS_NOMINAIS) / N_VAGAS_FEDERAIS)

# Função para enumerar candidatos por quociente eleitoral
n_candidatos <- function(df, Quotient) {
  df |>
    group_by(ANO_ELEICAO) |>
    summarize(count = sum(TOTAL_VOTOS_NOMINAIS >= EQ * Quotient))
}

# Aplicando a função para cada limiar 
result <- lapply(thresholds, function(t) {
  n_candidatos(base_final, t)
})

# Combinando resultados numa tabela única
threshold_labels <- c("200%", 
                      "150%", 
                      "100%", 
                      "90%", 
                      "75%", 
                      "50%", 
                      "25%", 
                      "5%")

# Tabela final
tabela_final <- bind_rows(result, .id = "Quotient")

tabela_final$Quotient <- factor(tabela_final$Quotient, 
                                labels = threshold_labels)

# Tornando a tabela wider
tabela_final <- tabela_final |>
  pivot_wider(names_from = ANO_ELEICAO, 
              values_from = count) |>
  rename("District Quota Ratio" = Quotient)

# Salvando o dado --------------------------------------------------------------
saveRDS(tabela_final, file = "votes_district_quota_1998_2022")
