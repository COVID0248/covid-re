library(dplyr)
library(magrittr)
is_integer64 <- function(x) {class(x)[1] == "integer64"}

# Comunas vecinas, según comuna
df_vecinos <- 
  readRDS("data/vecinos.rds") %>% {
    rbind(
      select(., comuna = codigo_comuna, vecino = codigo_vecino),
      select(., comuna = codigo_vecino, vecino = codigo_comuna)
    )}

# Población y el status de cuarentena, según semana y comuna
df_semana_comuna <- 
  readRDS("data/vw_base1.rds") %>%
  dplyr::arrange(codigo_comuna, semana, fecha) %>%
  dplyr::group_by(codigo_comuna, semana) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(semana = sub('SE', '', semana) %>% as.integer) %>%
  dplyr::select(semana, comuna = codigo_comuna, cuarentena, poblacion) %>%
  dplyr::arrange(semana, comuna)

# Porcentaje de población vecina en cuarentena, según semana y comuna
df_pp_vecinos_cuarentena <-
  df_semana_comuna %>%
  dplyr::inner_join(df_vecinos) %>%
  dplyr::arrange(semana, vecino) %>%
  dplyr::group_by(semana, vecino) %>%
  dplyr::summarize(pvc = sum(cuarentena * poblacion) / sum(poblacion)) %>%
  dplyr::rename(comuna = vecino) %>%
  dplyr::ungroup()

# ID de cuarentena y semana dentro de esa cuarentena, según comuna y semana
df_semana_cuarentena <-
  df_semana_comuna %>%
  dplyr::select(comuna, semana, ctn = cuarentena) %>%
  dplyr::arrange(comuna, semana) %>%
  dplyr::group_by(comuna) %>%
  dplyr::mutate(id_cuarentena = dplyr::lag(ctn, default = 1) != ctn) %>%
  dplyr::filter(ctn == 1) %>%
  dplyr::mutate(id_cuarentena = cumsum(id_cuarentena)) %>%
  dplyr::group_by(comuna, id_cuarentena) %>%
  dplyr::mutate(semana_cuarentena = dplyr::row_number())

# Porcentaje de población veicna en cuarentena, 
# según comuna, id de cuarentena y semana de cuarentena
df <-
  df_pp_vecinos_cuarentena %>%
  dplyr::inner_join(df_semana_cuarentena) %>%
  dplyr::select(
    codigo_comuna = comuna, 
    id_cuarentena, 
    semana = semana_cuarentena, 
    pvc
  ) %>%
  dplyr::mutate(dplyr::across(where(is_integer64), as.integer))

# Resultado final
saveRDS(df, file = "data/pp_vecinos_cuarentena.rds") 
