# Limpando o environment
rm(list = ls())

# Pacotes úteis
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)

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
                                                               "ELEITO POR MÉDIA",
                                                               "ELEITO POR QP"), 
                                     "ELEITO", 
                                     DESC_SIT_TOT_TURNO)) |>
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) |>
  filter(!is.na(ID_LEGENDA))

# Manipulação dos dados --------------------------------------------------------

# Apenas Não-Eleitos -----------------------------------------------------------

# Identificando apenas candidatos não eleitos 
NAO_ELEITOS <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO %in% c("NÃO ELEITO",
                                   "SUPLENTE"))

# Ranqueando apenas candidatos não eleitos dentro das listas 
NAO_ELEITOS <- NAO_ELEITOS |>
  group_by(ANO_ELEICAO,
           UF,
           IDENTIFICADOR_LEGENDA) |>     
  arrange(desc(TOTAL_VOTOS_NOMINAIS)) |>  
  mutate(RANK_ENTRE_NAO_ELEITOS = row_number()) |>  
  ungroup()

# Identificando maior quebra em cada lista
NAO_ELEITOS <- NAO_ELEITOS |>
  # Agrupando por lista, distrito e ano eleitoral
  group_by(ANO_ELEICAO, 
           UF, 
           IDENTIFICADOR_LEGENDA) |>  
  mutate(
    MAIOR_QUEBRA_LISTA = ifelse(all(is.na(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA)), 
                                NA, 
                                which.max(abs(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM_LISTA))),
    # Atribuiondo 1 a maior quebra dentro da lista
    MAIOR_QUEBRA_LISTA_dummy = ifelse(row_number() == MAIOR_QUEBRA_LISTA, 1, 0)
  ) |>
  ungroup()

# Dataframe reduzido para plots 
NAO_ELEITOS_PLOT <- NAO_ELEITOS |>
  # Selecionando apenas variáveis de intresse
  select(RANK_ENTRE_NAO_ELEITOS,
         MAIOR_QUEBRA_LISTA_dummy) |>
  # Filtrando apenas para maiores quebras dentro de cada lista 
  filter(MAIOR_QUEBRA_LISTA_dummy == 1)


MEDIA_NAO_ELEITOS <- mean(NAO_ELEITOS_PLOT$RANK_ENTRE_NAO_ELEITOS, na.rm = TRUE)

ggplot(NAO_ELEITOS_PLOT, aes(x = RANK_ENTRE_NAO_ELEITOS)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = mean_rank), color = "red", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(title = "Frequency of Within-List Ranks with the Largest Drop in Votes Among Non Elected", 
       x = "Within-List Rank of the Largest Drop in Votes", 
       y = "Frequency of Within-List Ranks") +
  theme_classic() +
  annotate("text", 
           x = MEDIA_NAO_ELEITOS + 0.5, 
           y = Inf, 
           label = paste("Mean for Non Elected:", 
                         round(MEDIA_NAO_ELEITOS, 1)), 
           color = "red", 
           vjust = 2, 
           hjust = 0)

# Apenas Eleitos ---------------------------------------------------------------

ELEITOS <- eleicoes_proporcionais_federais |>
  filter(DESC_SIT_TOT_TURNO == "ELEITO")

# Dataframe reduzido para plots 
ELEITOS_PLOT <- NAO_ELEITOS |>
  # Selecionando apenas variáveis de intresse
  select(RANK_LISTA,
         QUEBRA_MAXIMA_LISTA) |>
  # Filtrando apenas para maiores quebras dentro de cada lista 
  filter(QUEBRA_MAXIMA_LISTA == 1)

MEDIA_ELEITOS <- mean(ELEITOS_PLOT$RANK_LISTA, na.rm = TRUE)

ggplot(ELEITOS_PLOT, aes(x = RANK_LISTA)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = mean_rank), color = "red", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(title = "Frequency of Within-List Ranks with the Largest Drop in Votes Among Elected", 
       x = "Within-List Rank of the Largest Drop in Votes", 
       y = "Frequency of Within-List Ranks") +
  theme_classic() +
  annotate("text", 
           x = MEDIA_NAO_ELEITOS + 0.5, 
           y = Inf, 
           label = paste("Mean for Elected:", 
                         round(MEDIA_ELEITOS, 1)), 
           color = "red", 
           vjust = 2, 
           hjust = 0)


# Todos os candidatos ----------------------------------------------------------

# Dataframe reduzido para plots 
CANDIDATOS_PLOT <- eleicoes_proporcionais_federais |>
  # Selecionando apenas variáveis de intresse
  select(RANK_LISTA,
         QUEBRA_MAXIMA_LISTA) |>
  # Filtrando apenas para maiores quebras dentro de cada lista 
  filter(QUEBRA_MAXIMA_LISTA == 1)

MEDIA_CANDIDATOS <- mean(CANDIDATOS_PLOT$RANK_LISTA, na.rm = TRUE)

ggplot(CANDIDATOS_PLOT, aes(x = RANK_LISTA)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = mean_rank), color = "red", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(title = "Frequency of Within-List Ranks with the Largest Drop in Votes Among All Candidates", 
       x = "Within-List Rank of the Largest Drop in Votes", 
       y = "Frequency of Within-List Ranks") +
  theme_classic() +
  annotate("text", 
           x = MEDIA_NAO_ELEITOS + 0.5, 
           y = Inf, 
           label = paste("Mean for All Candidates:", 
                         round(MEDIA_CANDIDATOS, 1)), 
           color = "red", 
           vjust = 2, 
           hjust = 0)
