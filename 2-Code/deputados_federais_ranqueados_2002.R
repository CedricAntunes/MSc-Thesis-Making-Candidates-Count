# Limpando o environment
rm(list = ls())

# Liberando memória 
gc()

# Pacotes úteis à análise ------------------------------------------------------
library(dplyr)
library(tidyr)
library(parquetize)
library(arrow)
library(stringr)

# Carregando os dados ----------------------------------------------------------
path <- "F:/Public/Documents/repositorioTSE/data/output/JoinFinal/votacao_secao_coli_cand_2002_DEPUTADO FEDERAL_CEPESPv202410.parquet"

# Selecionando apenas colunas úteis à análise 
colunas_uteis <- c("ID_CEPESP", 
                   "ANO_ELEICAO",
                   "SIGLA_UE",
                   "UF",
                   "NUMERO_CANDIDATO",
                   "CODIGO_CARGO",
                   "NOME_CANDIDATO",
                   "DES_SITUACAO_CANDIDATURA",
                   "COD_SIT_TOT_TURNO",
                   "DESC_SIT_TOT_TURNO",
                   'COD_SITUACAO_CANDIDATO_TOT',
                   "DES_SITUACAO_CANDIDATO_TOT",
                   "NUMERO_PARTIDO",
                   "NOME_PARTIDO",
                   "SIGLA_PARTIDO",
                   "TIPO_LEGENDA",
                   "CODIGO_LEGENDA",
                   "SIGLA_LEGENDA",
                   "COMPOSICAO_LEGENDA",
                   "SEQUENCIA_COLIGACAO",
                   "NOME_COLIGACAO",
                   "COMPOSICAO_COLIGACAO",
                   "COMPOSICAO_COLIGACAO",
                   "QTDE_VOTOS")

# Carregando os dados 
deputados_federais_2002 <- open_dataset(path) |>
  # Selecionando apenas colunas relevantes à análise
  select(all_of(colunas_uteis)) |>
  # Gerando o dataframe 
  as.data.frame()

# Manipulando a base  -----------------------------------------------------

# Criando colunas para votos nominais e votos na legenda
deputados_federais_2002$QTDE_VOTO_NOMINAL <- 0
deputados_federais_2002$QTDE_VOTO_LEGENDA <- 0
deputados_federais_2002$QTDE_VOTO_LEGENDA <- ifelse(deputados_federais_2002$NOME_CANDIDATO == "VOTO LEGENDA", 
                                                    deputados_federais_2002$QTDE_VOTOS, 0)

# Atribuíndo votos nominais 
deputados_federais_2002$QTDE_VOTO_NOMINAL <- ifelse(
  # Checando se há NAs para NOME_CANDIDATO
  !is.na(deputados_federais_2002$NOME_CANDIDATO) & 
    # Atribuíndo zero votos nominais a votos na legenda
    deputados_federais_2002$NOME_CANDIDATO != "VOTO LEGENDA" &
    # Atribuíndo zero votos nominais a votos brancos
    deputados_federais_2002$NOME_CANDIDATO != "VOTO BRANCO" &
    # Atribuíndo zero votos nominais a votos nulos 
    deputados_federais_2002$NOME_CANDIDATO != "VOTO NULO",
  deputados_federais_2002$QTDE_VOTOS, 
  0
)

# Identificando coligações e partidos isolados
deputados_federais_2002 <- deputados_federais_2002 |>
  # Variável categórica que identifica nome da legenda
  mutate(IDENTIFICADOR_LEGENDA = ifelse(TIPO_LEGENDA == "COLIGAÇÃO", 
                                        NOME_COLIGACAO, 
                                        ifelse(TIPO_LEGENDA == "PARTIDO ISOLADO", 
                                               NOME_PARTIDO, 
                                               NA)),
         # Variável dicotômica que identifica se legenda é partido
         # isolado ou coligação 
         COLIGACAO = ifelse(TIPO_LEGENDA == "COLIGAÇÃO", 1, 
                            ifelse(TIPO_LEGENDA == "PARTIDO ISOLADO", 0, NA)),
         # Criando identificador numérico único para legendas
         ID_LEGENDA = ifelse(TIPO_LEGENDA == "COLIGAÇÃO", 
                             sprintf("%03d", as.numeric(factor(NOME_COLIGACAO))), 
                             ifelse(TIPO_LEGENDA == "PARTIDO ISOLADO", 
                                    as.character(NUMERO_PARTIDO), 
                                    NA)),
         # Criando variável numérica para o N de partidos na legenda
         NUM_PARTIDOS_COLIGACAO = ifelse(is.na(COMPOSICAO_LEGENDA) | COMPOSICAO_LEGENDA == "" | !str_detect(COMPOSICAO_LEGENDA, "/"),
                                         1,
                                         str_count(COMPOSICAO_LEGENDA, "/") + 1))

# Soma votos na legenda: coligações
votos_legenda <- deputados_federais_2002 |> 
  group_by(ID_LEGENDA,
           SIGLA_UE) |>
  summarise(TOTAL_VOTOS_LEGENDA = sum(as.numeric(QTDE_VOTO_LEGENDA), na.rm = TRUE))

# Soma total de votos preferenciais: coligações e partidos isolados
votos_nominais <- deputados_federais_2002 |> 
  group_by(SIGLA_UE, 
           ID_CEPESP) |>
  summarise(TOTAL_VOTOS_NOMINAIS = sum(as.numeric(QTDE_VOTO_NOMINAL), na.rm = TRUE))

# Soma total de votos no partido: legenda + preferenciais 
votos_partido <- deputados_federais_2002 |>
  group_by(NOME_PARTIDO,
           SIGLA_UE) |>
  summarise(TOTAL_VOTOS_PARTIDO = sum(as.numeric(QTDE_VOTOS), na.rm = TRUE))

# Join
deputados_federais_2002 <- deputados_federais_2002 |>
  left_join(votos_legenda, 
            by = c("SIGLA_UE", 
                   "ID_LEGENDA"))

# Join
deputados_federais_2002 <- deputados_federais_2002 |>
  left_join(votos_nominais,
            by = c("SIGLA_UE",
                   "ID_CEPESP"))

# Soma total de votos na legenda: partidos isolados 
votos_legenda_partidos <- deputados_federais_2002 |>
  filter(TIPO_LEGENDA == "PARTIDO ISOLADO") |>
  group_by(NUMERO_CANDIDATO,
           SIGLA_UE) |>
  summarise(TOTAL_VOTOS_LEGENDA_PARTIDO_ISOLADO = sum(as.numeric(QTDE_VOTO_LEGENDA), na.rm = TRUE)) |>
  rename(NUMERO_PARTIDO = NUMERO_CANDIDATO)

# Join
deputados_federais_2002 <- deputados_federais_2002 |>
  left_join(votos_legenda_partidos,
            by = c("SIGLA_UE",
                   # Note que aqui a chave é NUMERO_PARTIDO!
                   "NUMERO_PARTIDO")) 

# Join
deputados_federais_2002 <- deputados_federais_2002 |>
  left_join(votos_partido,
            by = c("NOME_PARTIDO",
                   "SIGLA_UE")) |>
  mutate(TOTAL_VOTOS_LEGENDA_PARTIDO_ISOLADO = replace_na(TOTAL_VOTOS_LEGENDA_PARTIDO_ISOLADO, 0))

# Soma total de votos preferenciais da legenda
total_votos_preferenciais <- deputados_federais_2002 |>
  group_by(ID_LEGENDA) |>
  summarise(TOTAL_NOMINAIS_LEGENDA = sum(as.numeric(QTDE_VOTO_NOMINAL), na.rm = TRUE)) |>
  ungroup()

# Soma total de votos preferenciais em candidatos individuais
total_votos_candidato <- deputados_federais_2002 |>
  group_by(ID_CEPESP,
           SIGLA_UE) |>
  mutate(TOTAL_VOTOS_CANDIDATO = sum(as.numeric(QTDE_VOTOS), na.rm = TRUE))

# Join
deputados_federais_2002 <- deputados_federais_2002 |>
  left_join(total_votos_preferenciais, 
            by = "ID_LEGENDA") |>
  # Soma o total de votos na lista: partido isolado ou coligação
  mutate(TOTAL_VOTOS_LISTA = TOTAL_NOMINAIS_LEGENDA + TOTAL_VOTOS_LEGENDA)

# Identifica candidatos individuais
candidatos_individuais <- deputados_federais_2002 |>
  filter(DESC_SIT_TOT_TURNO %in% c("SUPLENTE", 
                                   "ELEITO",
                                   # Capturamos as sobras com a MÉDIA!
                                   "MÉDIA",
                                   "ELEITO POR MÉDIA",
                                   "ELEITO POR QP",
                                   "NÃO ELEITO")) |>
  distinct(SIGLA_UE,
           NOME_CANDIDATO,
           .keep_all = TRUE)

# Ranquea candidatos dentro da lista
listas_2002_ranqueadas <- candidatos_individuais |>
  group_by(UF, 
           ID_LEGENDA) |>
  arrange(UF, 
          ID_LEGENDA, 
          # Ranqueando candidatos de modo descendente
          desc(TOTAL_VOTOS_NOMINAIS)) |>
  mutate(RANK_LISTA = row_number()) |>
  ungroup()

# Ranquea candidatos dentro dos partidos
listas_partidos_2002_ranqueados <- listas_2002_ranqueadas |> 
  group_by(UF, 
           NOME_PARTIDO) |>
  arrange(UF, 
          NOME_PARTIDO, 
          # Ranqueando candidatos de modo descendente
          desc(TOTAL_VOTOS_NOMINAIS)) |>  
  mutate(RANK_PARTIDO = row_number()) |>
  ungroup()

# Estimando distâncias entre candidatos  ---------------------------------------

# Função para estimar distância (em votos) para o último eleito em cada lista
DISTANCIA_ULTIMO_ELEITO <- function(df) {
  # Identificando o último eleito a partir do rank 
  ultimo_eleito <- df |>
    filter(DESC_SIT_TOT_TURNO == "ELEITO" | DESC_SIT_TOT_TURNO == "MÉDIA") |>
    arrange(desc(RANK_LISTA)) |>
    slice(1)
  
  # Identificando último eleito
  if (nrow(ultimo_eleito) == 0) {
    df <- df |>
      mutate(DISTANCIA_PARA_ULTIMO_ELEITO_EM_VOTOS = NA,
             ULTIMO_ELEITO = 0)
  } else {
    # Calculando a distância para o último eleito da lista para cada candidato
    df <- df |>
      mutate(DISTANCIA_PARA_ULTIMO_ELEITO_EM_VOTOS = ultimo_eleito$TOTAL_VOTOS_NOMINAIS - TOTAL_VOTOS_NOMINAIS,
             ULTIMO_ELEITO = ifelse(ID_CEPESP == ultimo_eleito$ID_CEPESP, 1, 0))
  }
  
  return(df)
}

# Aplica a função para cada combinação distinta de partido e unidade eleitoral
resultados <- listas_partidos_2002_ranqueados |>
  group_by(UF, 
           ID_LEGENDA) |>
  do(data.frame(DISTANCIA_ULTIMO_ELEITO(.)))

# Dropando NAs
resultados_filtrados <- resultados |>
  filter(!is.na(DISTANCIA_PARA_ULTIMO_ELEITO_EM_VOTOS))

# Adiciona coluna para distâncias em votos em módulo
resultados_filtrados_final <- resultados_filtrados |>
  mutate(DISTANCIA_ABSOLUTA_PARA_ULTIMO_ELEITO_EM_VOTOS = abs(DISTANCIA_PARA_ULTIMO_ELEITO_EM_VOTOS)) |>
  select(-COD_SITUACAO_CANDIDATO_TOT, 
         -DES_SITUACAO_CANDIDATURA,
         -COD_SIT_TOT_TURNO,
         -DES_SITUACAO_CANDIDATO_TOT,
         -CODIGO_LEGENDA,
         -SIGLA_LEGENDA,
         -QTDE_VOTOS,
         -QTDE_VOTO_NOMINAL,
         -QTDE_VOTO_LEGENDA)

# Esima as distâncias em votos entre ranques para candidatos de uma mesma lista
final <- resultados_filtrados_final |>
  arrange(SIGLA_UE, 
          ID_LEGENDA, 
          RANK_LISTA) |>
  group_by(SIGLA_UE, 
           ID_LEGENDA) |> 
  mutate(DISTANCIA_EM_VOTOS_PARA_N_MENOS_UM = TOTAL_VOTOS_NOMINAIS - lag(TOTAL_VOTOS_NOMINAIS, 
                                                                         order_by = RANK_LISTA)) |> 
  ungroup()

# Estima o share de votos do candidato dentro da lista
final_final_2002 <- final |>
  group_by(SIGLA_UE,
           NOME_CANDIDATO) |>
  mutate(SHARE_CANDIDATO_LISTA = round((TOTAL_VOTOS_NOMINAIS / TOTAL_VOTOS_LISTA) * 100, 2))

# Estima o N de cadeiras ganhas por cada partido
N_cadeiras_partidos <- final_final_2002 %>%
  group_by(UF, 
           NOME_PARTIDO) |>
  summarise(N_CADEIRAS_GANHAS_PARTIDO = sum(DESC_SIT_TOT_TURNO %in% c("ELEITO", 
                                                                      "MÉDIA",
                                                                      "ELEITO POR MÉDIA",
                                                                      "ELEITO POR QP"))) |>
  ungroup()

# Estima o N de cadeiras ganhas por cada coligação
N_cadeiras_coligacao <- final_final_2002 %>%
  group_by(UF, 
           NOME_COLIGACAO) |>
  summarise(N_CADEIRAS_GANHAS_COLIGACAO = sum(DESC_SIT_TOT_TURNO %in% c("ELEITO", 
                                                                        "MÉDIA",
                                                                        "ELEITO POR MÉDIA",
                                                                        "ELEITO POR QP"))) |>
  ungroup()

# Join
final_final_2002 <- final_final_2002 |>
  left_join(N_cadeiras_partidos, 
            by = c("UF",
                   "NOME_PARTIDO"))

# Join
base_final_2002 <- final_final_2002 |>
  left_join(N_cadeiras_coligacao, 
            by = c("UF",
                   "NOME_COLIGACAO"))

# Salvando o dado ---------------------------------------------------------
saveRDS(base_final_2002, file = "deputados_federais_ranqueados_2002")
write.csv(base_final_2002, "deputados_federais_ranqueados_2002.csv")
