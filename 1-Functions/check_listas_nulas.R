# Removendo listas em que DISTANCIA_PARA_LARGEST_DROP é NA
NAs_check <- resultados_final_largest_drop |>
  filter(is.na(DISTANCIA_PARA_LARGEST_DROP) & DESC_SIT_TOT_TURNO == "NÃO ELEITO")

# Checando
summary(filtered_results$DISTANCIA_PARA_LARGEST_DROP)
