# Função para estimar distância (em votos) para o último eleito em cada lista
DISTANCIA_ULTIMO_ELEITO <- function(df) {
  # Identificando o último eleito a partir do rank 
  ultimo_eleito <- df |>
    filter(DESC_SIT_TOT_TURNO == "ELEITO") |>
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
