# Retorna un conector con AWS
get_conn <- function() {
  conn <-
    noctua::athena() %>%
    noctua::dbConnect(
      profile_name   = "covid-anid",
      s3_staging_dir = "s3://covid-anid/athena-results/",
      schema_name    = "struct_covid",
      region_name    = "us-east-1",
      bigint         = "integer"
    )
}

#' Todos los pares de comunas vecinas
#' 
#' @format Un tibble con 1702 filas y 2 columnas:
#' \describe{
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_vecino}{vecino (código cut)}
#' }
#' @note No incluye la antártica (12202)
get_vecinos <- function() {
  readRDS("data/vecinos.rds")
}

#' Población entre 20 y 64, según comuna
#' 
#' @format Un tibble con 345 filas y 2 columnas:
#' \describe{
#'   \item{codigo_comuna}{comuna (código cut)}
#'   \item{pob_20_64}
#' }
#' @note No incluye la antártica (12202)
get_pob_20_64 <- function(conn) {
  conn %>%
    dplyr::tbl("poblacion_edad") %>% 
    dplyr::rename(n = total_poblacion_efectivamente_censada) %>%
    dplyr::select(codigo_comuna, edad, n) %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(
      tidyselect:::where(bit64::is.integer64), 
      as.integer
    )) %>%
    dplyr::filter(edad %in% 20:64) %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::summarise(pob_20_a_64 = sum(n)) %>%
    dplyr::filter(codigo_comuna != 12202)
}

#' Número de inmigrantes, según comuna
#' 
#' @format Un tibble con 345 filas y 2 columnas:
#' \describe{
#'   \item{codigo_comuna}{comuna (código cut)}
#'   \item{pob_20_64}
#' }
#' @note No incluye la antártica (12202)
get_inmigrantes <- function(conn) {
  conn %>%
    dplyr::tbl("censo") %>% 
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(
      where(bit64::is.integer64), 
      as.integer
    )) %>%
    dplyr::select(
      codigo_comuna = comuna, 
      inmigrantes = inmigrantes
    ) %>%
    dplyr::filter(codigo_comuna != 12202)
}

#' Información comunal, según comuna
#' 
#' @format Un tibble con 345 filas y 31 columnas:
#' \describe{
#'   \item{codigo_comuna}{comuna (código cut)}
#'   \item{codigo_region}
#'   \item{region}
#'   \item{comuna}
#'   \item{provincia}
#'   \item{superficie}
#'   \item{poblacion}
#'   \item{capital_regional}
#'   \item{capital_provincial}
#'   \item{aeropuerto}
#'   \item{puerto}
#'   \item{idse}{Índice de desarrollo socioeconómico (0-1000)}
#'   \item{ingreso_per_capita}
#'   \item{escolaridad}
#'   \item{material_bueno_o_aceptable}
#'   \item{esperanza_vida_al_nacer}
#'   \item{tasa_avvp}
#'   \item{defunciones}
#'   \item{mortalidad_infantil}
#'   \item{pobreza}
#'   \item{alcantarillado}
#'   \item{indice_desarrollo_humano}
#'   \item{porcentake_rural}
#'   \item{porcentaje_sector_primario}
#'   \item{densidad_poblacional}
#'   \item{porcentaje_rural_estandarizado}
#'   \item{porcentaje_rural_estandarizado.1}
#'   \item{porcentaje_sector_primario_estandarizado}
#'   \item{indice_ruralidad}
#'   \item{inmigrantes}
#'   \item{pob_20_64}
#' }  
#' @note No incluye la antártica (12202)
#' @note Hay 21 comunas (todas pequeñas) con NA en varias variables
get_comunas <- function(conn, inmigrantes, pob_20_64) {
  conn %>%
    dplyr::tbl("maestra_comunas") %>% 
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(
      where(bit64::is.integer64), 
      as.integer
    )) %>%
    dplyr::rename(
      codigo_comuna = cod_comuna, 
      codigo_region = cod_region
    ) %>%
    dplyr::inner_join(inmigrantes) %>%
    dplyr::inner_join(pob_20_64) %>%
    dplyr::filter(!is.na(codigo_region)) %>%
    dplyr::filter(codigo_comuna != 12202)
}

#' Población, según comuna
#' 
#' @format Un tibble con 345 filas y 2 columnas:
#' \describe{
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{poblacion}
#' }
#' @note No incluye la antártica (12202)
get_poblacion <- function(comunas) {
  poblacion <-
    dplyr::select(comunas, codigo_comuna, poblacion) %>%
    dplyr::filter(codigo_comuna != 12202)
}

#' Fase en plan paso a paso, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 14,490 filas y 3 columnas
#' \describe{
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{paso}{fase en plan paso a paso (la peor durante la semana)}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 31-72
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 74
get_pasos <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto74/paso_a_paso_std.csv") %>%
    read.csv() %>%
    dplyr::rename_with(tolower) %>%
    dplyr::group_by(fecha, codigo_comuna) %>%
    dplyr::summarise(paso = min(paso), .groups = "drop") %>%
    dplyr::mutate(codigo_semana = date_to_sepi(fecha)) %>%
    dplyr::filter(codigo_comuna != 0) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(paso = min(paso), .groups = "drop") %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    tibble::as_tibble()
}

#' Casos nuevos, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 22,425 filas y 5 columnas
#' \describe {
#' \item{codigo_region}{región (código)}
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{n}{población comunal}
#' \item{casos_nuevos}{casos nuevos (según semana de inicio de síntomas)}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 7-71
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 15
get_casos0 <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto15/FechaInicioSintomas_std.csv") %>%
    read.csv() %>%
    dplyr::rename_with(~ gsub("\\.", "_", tolower(.x))) %>%
    dplyr::filter(codigo_comuna != 0) %>%
    dplyr::mutate(
      sepi_week     = semana_epidemiologica %% 100,
      sepi_year     = floor(semana_epidemiologica / 100),
      codigo_semana = sepi_week + max(sepi_week) * (sepi_year == 2021),
      casos_nuevos  = casos_confirmados,
      n             = poblacion
    ) %>%
    dplyr::select(dplyr::contains("codigo"), n, casos_nuevos) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    tibble::as_tibble()
}

#' max{1, casos nuevos}, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 22,425 filas y 5 columnas
#' \describe {
#' \item{codigo_region}{región (código)}
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{n}{población comunal}
#' \item{casos_nuevos}{max{1, casos nuevos} (según semana de inicio de síntomas)}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 7-71
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 15
get_casos <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto15/FechaInicioSintomas_std.csv") %>%
    read.csv() %>%
    dplyr::rename_with(~ gsub("\\.", "_", tolower(.x))) %>%
    dplyr::filter(codigo_comuna != 0) %>%
    dplyr::mutate(
      sepi_week     = semana_epidemiologica %% 100,
      sepi_year     = floor(semana_epidemiologica / 100),
      codigo_semana = sepi_week + max(sepi_week) * (sepi_year == 2021),
      casos_nuevos  = pmax(casos_confirmados, 1),
      n             = poblacion
    ) %>%
    dplyr::select(dplyr::contains("codigo"), n, casos_nuevos) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    tibble::as_tibble()
}

#' (n. exámenes pcr / población comunal), según región y semana epidemiológica
#' 
#' @format Un tibble con 928 filas y 3 columnas
#' \describe {
#' \item{codigo_region}{región (código)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{pcr}{número de exámenes pcr, como fracción de la población comunal}
#' }
#' @note Cubre las semanas epidemiológicas 15-72
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 7
get_pcr <- function(comunas) {
  pob_region <-
    comunas %>%
    dplyr::group_by(codigo_region) %>%
    dplyr::summarise(poblacion = sum(poblacion))
    
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto7/PCR_std.csv") %>%
    read.csv() %>%
    dplyr::mutate(
      codigo_region = Codigo.region, 
      codigo_semana = date_to_sepi(fecha)
    ) %>%
    dplyr::group_by(codigo_region, codigo_semana) %>%
    dplyr::summarise(pcr = sum(numero, na.rm = TRUE), .groups = "drop") %>%
    dplyr::inner_join(pob_region) %>%
    dplyr::mutate(pcr = pcr / poblacion) %>%
    dplyr::arrange(codigo_region, codigo_semana) %>%
    dplyr::select(-poblacion) %>%
    tibble::as_tibble()
}

#' Personas con 1ra vacuna, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 7,245 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{vacunados1}{personas con 1ra vacuna (acumulado)}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 52-72
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 80
get_vacunados1 <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto80/vacunacion_comuna_1eraDosis_std.csv") %>%
    read.csv() %>%
    dplyr::mutate(
      codigo_comuna = Codigo.comuna,
      codigo_semana = date_to_sepi(Fecha)
    ) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(vacunados1 = sum(Primera.Dosis)) %>%
    dplyr::mutate(vacunados1 = cumsum(vacunados1)) %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    dplyr::as_tibble()
}

#' Personas con 2da vacuna, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 7,245 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{vacunados2}{personas con 2da vacuna (acumulado)}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 52-72
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 80
get_vacunados2 <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto80/vacunacion_comuna_2daDosis_std.csv") %>%
    read.csv() %>%
    dplyr::mutate(
      codigo_comuna = Codigo.comuna,
      codigo_semana = date_to_sepi(Fecha)
    ) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(vacunados2 = sum(Segunda.Dosis)) %>%
    dplyr::mutate(vacunados2 = cumsum(vacunados2)) %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    dplyr::as_tibble()
}

#' Índice de movilidad interno, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 14,104 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{im_interno}{índice de movilidad interno}
#' }
#' @note No incluye la antártica (12202) ni O'Higgins (11302)
#' @note Cubre las semanas epidemiológicas 9-49
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 33
get_im_interno <- function() {
  im_interno <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto33/IndiceDeMovilidad_std.csv") %>%
    read.csv() %>%
    dplyr::filter(variable == "IM") %>%
    dplyr::mutate(
      codigo_comuna = Codigo.comuna,
      codigo_semana = date_to_sepi(Fecha)
    ) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(im_interno = mean(value), .groups = NULL) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(!codigo_comuna %in% c(11302, 12202))
}

#' Índice de movilidad externo, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 14,104 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{im_externo}{índice de movilidad externo}
#' }
#' @note No incluye la antártica (12202) ni O'Higgins (11302)
#' @note Cubre las semanas epidemiológicas 9-49
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 33
get_im_externo <- function() {
  im_externo <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto33/IndiceDeMovilidad_std.csv") %>%
    read.csv() %>%
    dplyr::filter(variable == "IM_externo") %>%
    dplyr::mutate(
      codigo_comuna = Codigo.comuna,
      codigo_semana = date_to_sepi(Fecha)
    ) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(im_externo = mean(value), .groups = NULL) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(!codigo_comuna %in% c(11302, 12202))
}

#' MP10, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 1,352 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{mp10}{MP10 (microgramos por metro cúbico normalizado)}
#' }
#' @note No incluye la antártica (12202)
#' @note Hay comunas y regiones llenas de NAs
#' @note Cubre las semanas epidemiológicas 2-53
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 43
get_mp10 <- function() {
  data2020 <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto43/MP10-2020_std.csv") %>%
    read.csv()
  row.names(data2020) <- data2020$Nombre.de.estacion
  mp10_2020 <- 
    data2020 %>%
    dplyr::select(!Nombre.de.estacion) %>%
    t() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(codigo_comuna = `Codigo comuna`) %>%
    dplyr::select(!c(Region, `Codigo region`, Comuna, UTM_Este, UTM_Norte)) %>%
    tidyr::pivot_longer(-codigo_comuna, names_to = "t", values_to = "mp10") %>%
    dplyr::mutate(
      codigo_comuna = as.integer(codigo_comuna),
      codigo_semana = date_to_sepi(t),
      mp10 = as.numeric(mp10)
    ) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(mp10 = mean(mp10, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!codigo_comuna %in% c(12202))
}

#' Status de cuarentena, según comuna y semana epidemiológica
#' 
#' @format Un tibble con 24,840 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{cuarentena}{= 1 si se ha estado en cuarentena durante la semana}
#' }
#' @note No incluye la antártica (12202)
#' @note Cubre las semanas epidemiológicas 1-72
#' @source https://github.com/MinCiencia/Datos-COVID19 - Producto 29
get_cuarentenas <- function(comunas, pasos) {
  cuarentenas <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/" %>%
    paste0("/output/producto29/Cuarentenas-Totales.csv") %>%
    read.csv() %>%
    dplyr::mutate(
      codigo_comuna = Código.CUT.Comuna,
      inicio        = date_to_sepi(Fecha.de.Inicio),
      termino       = date_to_sepi(Fecha.de.Término)
    ) %>%
    dplyr::select(codigo_comuna, inicio, termino)
  
  comunas <-
    expand.grid(
      codigo_comuna = unique(comunas$codigo_comuna),
      codigo_semana = 1:max(cuarentenas$termino),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    dplyr::as_tibble()
  
  cuarentenas %<>%
    dplyr::right_join(comunas) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::mutate(
      cuarentena = codigo_semana >= inicio & codigo_semana <= termino
    ) %>%
    tidyr::replace_na(list(cuarentena = FALSE)) %>%
    dplyr::group_by(codigo_comuna, codigo_semana) %>%
    dplyr::summarise(cuarentena = max(cuarentena), .groups = NULL)
  
  cuarentenas %<>%
    dplyr::full_join(pasos) %>%
    dplyr::mutate(
      cuarentena = dplyr::if_else(
        is.na(cuarentena), 
        as.integer(paso == 1), 
        cuarentena
      )
    ) %>%
    dplyr::select(!paso) %>%
    dplyr::filter(!codigo_comuna %in% c(12202))
}


#' Fracción de la población vecina en cuarentena, 
#' según comuna y semana epidemiológica
#' 
#' @format Un tibble con 24,840 filas y 3 columnas
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{pp_vecinos_cuarentena}{Fracción de la población vecina en cuarentena}
#' }
#' @note No incluye la antártica (12202)
#' @note pp_vecinos_cuarentena == 0 para islas
#' @note Cubre las semanas epidemiológicas 1-72
#' @source Elaboración propia (ver R/targets.R)
get_pp_vecinos_cuarentena <- function(poblacion, vecinos, cuarentenas) {
  comunas <-
    expand.grid(
      codigo_comuna = unique(cuarentenas$codigo_comuna),
      codigo_semana = 1:max(cuarentenas$codigo_semana),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    dplyr::as_tibble()
  
  pp_vecinos_cuarentena <- 
    list(cuarentenas, poblacion, vecinos) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::arrange(codigo_semana, codigo_vecino) %>%
    dplyr::group_by(codigo_semana, codigo_vecino) %>%
    dplyr::summarise(
      pp_vecinos_cuarentena = sum(poblacion * cuarentena) / sum(poblacion)
    ) %>%
    dplyr::rename(codigo_comuna = codigo_vecino) %>%
    dplyr::right_join(comunas) %>%
    tidyr::replace_na(list(pp_vecinos_cuarentena = 0.0)) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::filter(!codigo_comuna %in% c(12202))
}

#' Status de período de vacaciones, según semana epidemiológica
#' 
#' @format Un tibble con 72 filas y 2 columnas
#' \describe {
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{vacaciones}{=1 si la semana toca al intervalo (2021-01-01, 2021-02-28)}
#' }
#' @note Cubre las semanas epidemiológicas 1-72
#' @source Elaboración propia (ver R/targets.R)
get_vacaciones <- function() {
  inicio  <- date_to_sepi(as.Date("2021-01-01"))
  termino <- date_to_sepi(as.Date("2021-02-28"))
  dplyr::tibble(
    codigo_semana = 1:date_to_sepi(Sys.Date()),
    vacaciones    = dplyr::between(codigo_semana, inicio, termino)
  )
}

get_r_systrom_model <- function(r_systrom_stanfile) {
  rstan::stan_model(r_systrom_stanfile)
}

get_r_systrom <- function(data, r_systrom_model) {
  data     <- dplyr::arrange(data, codigo_comuna, codigo_semana)
  semanas  <- unique(data$codigo_semana)
  comunas  <- unique(data$codigo_comuna)
  Nsemanas <- length(semanas)
  comunas %>%
    purrr::map(
      ~ list(
        gamma  = 1,
        sigma  = 0.1,
        Ntimes = Nsemanas,
        I      = data$casos_nuevos[data$codigo_comuna == .x]
      ) %>%
        rstan::sampling(r_systrom_model, data = ., seed = 1L, iter = 10000) %>%
        {tidyr::tibble(
          method        = "systrom",
          codigo_comuna = .x,
          codigo_semana = semanas,
          r             = rstan::get_posterior_mean(., "R")[, 5]
        )}
    ) %>%
    purrr::reduce(rbind) %>%
    dplyr::arrange(codigo_comuna, codigo_semana)
  
}

get_r_cislaghi <- function(data) {
  re_cislaghi <-
    data %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::mutate(
      r = casos_nuevos / dplyr::lag(casos_nuevos, 1),
      method = "cislaghi"
    ) %>%
    tibble::as_tibble()
}

get_r_jrc <- function(data) {
  re_jrc <-
    data %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::mutate(
      r = log(casos_nuevos / dplyr::lag(casos_nuevos)) + 1,
      method = "jrc"
    ) %>%
    tibble::as_tibble()
}

get_r_rki <- function(data) {
  re_rki <-
    data %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::mutate(
      r = casos_nuevos / dplyr::lag(casos_nuevos, 1),
      method = "rki"
    ) %>%
    tibble::as_tibble()
}

get_r_wallinga <- function(data) {
  re_wallinga <-
    unique(data$codigo_comuna) %>%
    purrr::map(
      ~ data %>%
        dplyr::filter(codigo_comuna == .x) %$%
        est_re_exp(
          setNames(casos_nuevos, codigo_semana),
          GT_obj = R0::generation.time("gamma", c(6.6 / 7, 1.5 / 7^2)),
          half_window_width = 3
        ) %>%
        dplyr::tibble(
          method        = "wallinga",
          codigo_semana = as.numeric(names(.)),
          codigo_comuna = .x,
          r            = .
        )
    ) %>%
    purrr::reduce(rbind) %>%
    dplyr::inner_join(data, by = c("codigo_comuna", "codigo_semana")) %>%
    dplyr::mutate(r = unname(r)) %>%
    tibble::as_tibble()
}

get_r <- function(...) {
  r_final <-
    list(...) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-c(n, casos_nuevos)) %>%
    dplyr::ungroup()
  
  regiones <-
    r_final %>%
    dplyr::distinct(codigo_comuna, codigo_region) %>%
    dplyr::rename(codigo_region2 = codigo_region) %>%
    na.omit()
  
  r_final <-
    r_final %>%
    dplyr::inner_join(regiones) %>%
    dplyr::select(!codigo_region) %>%
    dplyr::rename(codigo_region = codigo_region2)
}

#' Covariables a utilizar en nuestro modelo para el R efectivo
#' 
#' @format Un tibble con 22,425 filas y 85 variables
#' \describe {
#' \item{codigo_comuna}{comuna (código cut)}
#' \item{codigo_semana}{semana (epidemiologica)}
#' \item{casos_nuevos}{casos nuevos (según semana de inicio de síntomas)}
#' \item{pasos}{fase en plan paso a paso}
#' \item{pcr}{n. de test pcr / tamaño poblacional}
#' \item{vacunados1}{n. personas con la 1ra vacuna}
#' \item{vacunados2}{n. personas con la 2da vacuna}
#' \item{vacaciones}{=1 si la semana toca al intervalo (2021-01-01, 2021-02-28)}
#' \item{cuarentena}{=1 si se ha estado en cuarentena durante la semana}
#' \item{im_interno}{índice de movilidad interno}
#' \item{im_externo}{índice de movilidad externo}
#' \item{pp_vecinos_cuarentena}{Fracción de la población vecina en cuarentena}
#' \item{todas las variables en el tibble comunas}
#' }
#' @note Cubre las semanas epidemiológicas 7-71
#' @source Elaboración propia (ver R/targets.R)
get_covariates <- function(casos, ...) {
  df <- 
    list(casos, ...) %>%
    purrr::reduce(dplyr::left_join) %>%
    dplyr::ungroup() %>%
    dplyr::filter(codigo_comuna != 12202) %>%
    tidyr::replace_na(list(
      pcr        = 0,
      vacunados1 = 0,
      vacunados2 = 0
    )) %>%
    dplyr::mutate(
      paso = dplyr::if_else(is.na(paso), 4L - 3L * cuarentena, paso),
      idse = idse / 1000,
      densidad_1em4 = densidad_poblacional * 1e-4,
      across(
        .cols = c(pob_20_a_64, inmigrantes, vacunados1, vacunados2),
        .fns  = ~ .x / poblacion
      ),
      dplyr::across(c(codigo_comuna, codigo_region, paso), as.factor)
    ) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::mutate(
      !!!lags(vacunados1, 5),
      !!!lags(vacunados2, 5),
      !!!lags(paso, 5),
      !!!lags(pcr, 5),
      !!!lags(im_interno, 8),
      !!!lags(im_externo, 8),
      !!!lags(pp_vecinos_cuarentena, 5)
    ) %>%
    dplyr::ungroup() #%>%
    # dplyr::select(
    #   casos_nuevos,
    #   codigo_semana,
    #   codigo_comuna,
    #   codigo_region,
    #   capital_regional,
    #   capital_provincial,
    #   pob_20_a_64,
    #   inmigrantes,
    #   aeropuerto,
    #   puerto,
    #   idse,
    #   indice_ruralidad,
    #   dplyr::contains("lag")
    # )
}

get_fit <- function(df) {
  covariates <- c(
    "capital_regional",
    "capital_provincial",
    "pob_20_a_64",
    "inmigrantes",
    "aeropuerto",
    "puerto",
    "idse",
    "indice_ruralidad",
    paste0("paso_lag", 1:5),
    paste0("vacunados1_lag", 1:5),
    paste0("vacunados2_lag", 1:5),
    paste0("pvc_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("im_interno_lag", 6:8),
    paste0("im_externo_lag", 6:8)
  )
  mystepwise(
    yvar0    = "r",
    xvar0    = covariates,
    preserve = "",
    random   = "(1 | codigo_region / codigo_comuna)",
    max_pval = 0.1,
    data     = na.omit(df)
  )
}

get_cov <- function(fit) {
  nlme::VarCorr(fit)
}

get_b <- function(fit) {
  broom.mixed::tidy(fit, effects = "fixed")
}

get_plot_r_p50 <- function(r) {
  df <-
    r %>%
    dplyr::group_by(codigo_comuna, method) %>%
    dplyr::summarise(r = quantile(r, 0.5, na.rm = TRUE), .groups = NULL)

  mapa <-
    sf::st_read("data/mapa/mapa.shp", quiet = TRUE) %>%
    dplyr::filter(
      !(NOM_PROVIN %in% c("ANTÁRTICA CHILENA", "ISLA DE PASCUA")),
      !(NOM_COMUNA %in% c("JUAN FERNANDEZ"))
    ) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    dplyr::mutate(codigo_comuna = CUT) %>%
    dplyr::inner_join(df)

  ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.1) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")
}

get_plot_r_p10 <- function(r) {
  df <-
    r %>%
    dplyr::group_by(codigo_comuna, method) %>%
    dplyr::summarise(r = quantile(r, 0.10, na.rm = TRUE), .groups = NULL)

  mapa <-
    sf::st_read("data/mapa/mapa.shp", quiet = TRUE) %>%
    dplyr::filter(
      !(NOM_PROVIN %in% c("ANTÁRTICA CHILENA", "ISLA DE PASCUA")),
      !(NOM_COMUNA %in% c("JUAN FERNANDEZ"))
    ) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    dplyr::mutate(codigo_comuna = CUT) %>%
    dplyr::inner_join(df)

  ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.1) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")
}

get_plot_r_p90 <- function(r) {
  df <-
    r %>%
    dplyr::group_by(codigo_comuna, method) %>%
    dplyr::summarise(r = quantile(r, 0.90, na.rm = TRUE), .groups = NULL)

  mapa <-
    sf::st_read("data/mapa/mapa.shp", quiet = TRUE) %>%
    dplyr::filter(
      !(NOM_PROVIN %in% c("ANTÁRTICA CHILENA", "ISLA DE PASCUA")),
      !(NOM_COMUNA %in% c("JUAN FERNANDEZ"))
    ) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    dplyr::mutate(codigo_comuna = CUT) %>%
    dplyr::inner_join(df)

  ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.1) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")
}

get_plot_r_ts <- function(r, comunas) {
  regiones <-
    targets::tar_read(comunas) %>%
    dplyr::filter(capital_regional == 1) %>%
    dplyr::select(codigo_comuna, codigo_region, comuna)

  targets::tar_read(r) %>%
    dplyr::filter(codigo_region %in% c(5, 6, 8, 13)) %>%
    dplyr::select(!codigo_region) %>%
    dplyr::inner_join(regiones) %>%
    ggplot2::ggplot(aes(y = r, x = codigo_semana)) +
    ggplot2::facet_grid(rows = ggplot2::vars(method)) +
    ggplot2::geom_line(aes(colour = comuna)) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "epidemiological week", y = "effective R")
}

get_plot_r_bp <- function(r, comunas) {
  r %>%
    ggplot2::ggplot(aes(y = r, group = codigo_semana)) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::facet_grid(rows = ggplot2::vars(method)) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "epidemiological week", y = "effective R")
}

# TODO:
# Crear dos BBDD de casos, con y sin max(1, .)
# Revisar qué pasa con la comuna 12202 en "maestra de comunas". 
# R: Era la antártica (mejor sacarla del análisis)