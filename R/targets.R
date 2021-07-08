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
    dplyr::select(method, codigo_semana, codigo_comuna, r) %>%
    tibble::as_tibble()
}

get_r_martinez_leroux <- function(casos0, vecinos, comunas) {
  r_eff_inla <- function(regions) {
    # Retrieve the areas in the selected regions
    communes <- 
      targets::tar_read("comunas") %>%
      dplyr::filter(codigo_region %in% regions) %>%
      dplyr::pull(codigo_comuna)
    
    # Retrieve the links
    edges <- 
      targets::tar_read("vecinos") %>%
      dplyr::rename(edge1 = codigo_comuna, edge2 = codigo_vecino) %>%
      dplyr::filter(
        edge1 %in% communes, 
        edge2 %in% communes
      )
    
    # Retrieve the cases
    cases <- 
      targets::tar_read("casos0") %>%
      dplyr::filter(
        codigo_comuna %in% c(edges$edge1, edges$edge2),
        codigo_region %in% regions
      ) %>%
      dplyr::rename(y = casos_nuevos)
    
    # Compute a standardized index for areas
    id_area <- 
      cases %>% 
      dplyr::mutate(id_area = dplyr::dense_rank(codigo_comuna)) %>%
      dplyr::distinct(codigo_comuna, id_area)
    
    # Compute a standardized index for times
    id_time <- 
      cases %>% 
      dplyr::mutate(id_time = dplyr::dense_rank(codigo_semana)) %>%
      dplyr::distinct(codigo_semana, id_time)
    
    # Add standardized ids to edges
    edges <- 
      edges %>%
      dplyr::inner_join(id_area, by = c("edge1" = "codigo_comuna")) %>%
      dplyr::inner_join(id_area, by = c("edge2" = "codigo_comuna"))
    
    # Compute adjacency matrix
    adj_mat <- 
      edges %$%
      sparseMatrix(i = id_area.x, j = id_area.y) %>%
      as("dgCMatrix")
    
    # Add standardized ids to cases
    cases <- 
      cases %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::arrange(id_area, id_time) %>%
      dplyr::select(id_area, id_time, y, n)
    
    # Compute B-splines design matrix for cases$id_time
    x <- unique(cases$id_time)
    knots <- seq.int(min(x), max(x), by = 12)
    bsMat <- splines2::bSpline(cases$id_time, knots = knots, degree = 3)
    bsMat0 <- splines2::bSpline(x, knots = knots, degree = 3)
    Nvars <- ncol(bsMat)
    bsdf <- 
      as.data.frame(bsMat) %>%
      magrittr::set_colnames(paste0("x", 1:Nvars))
    
    df <- cbind(cases, bsdf)

    for (i in 1:Nvars) {
      df[[paste0("id", i)]] <- df$id_area
    }
    
    # Create the matrix used in Leroux's model
    ICARmatrix <- Diagonal(nrow(adj_mat), apply(adj_mat, 1, sum)) - adj_mat
    Cmatrix <- Diagonal(nrow(ICARmatrix), 1) -  ICARmatrix
    
    fis  <- paste0("f(id", 1:Nvars, ", x", 1:Nvars, ", model = 'generic1', Cmatrix = Cmatrix)", collapse = " + ")
    fmla <- paste0("y ~ 0 + ", fis)
    fmla
    
    fit <- 
      INLA::inla(
        formula = as.formula(fmla),
        data = df,
        family = "poisson",
        E = n, 
        control.predictor = list(compute = TRUE),
        control.compute = list(
          dic = TRUE, 
          waic = TRUE, 
          config = TRUE, 
          openmp.strategy = "pardiso.parallel"
        )
      )
    
    nsims <- 2000L
    R_samples <- 
      inla.posterior.sample(n = nsims, fit, add.names = FALSE, seed = 1L) %>%
      purrr::map(function(x) {
        b <- 
          x[["latent"]] %>%
          tail(Nvars * nrow(id_area)) %>%
          matrix(Nvars, nrow(id_area))
        
        expXb <-
          exp(bsMat0 %*% b)
        
        ws <- 
          sim_weights()
        
        R_samples <- 
          my_slice(expXb, 5, 1) / (
            ws[1] * my_slice(expXb, 5, 2) + 
              ws[2] * my_slice(expXb, 5, 3) + 
              ws[3] * my_slice(expXb, 5, 4) + 
              ws[4] * my_slice(expXb, 5, 5)
          )
      }) %>%
      purrr::reduce(`+`) %>%
      magrittr::divide_by(nsims) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(id_time = 4 + 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        cols = -id_time, 
        values_to = "r",
        names_prefix = "V", 
        names_to = "id_area",
        names_transform = list(id_area = as.integer)
      ) %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::mutate(id_region = regions, method = "martinez 1") %>%
      dplyr::select(-c(id_area, id_time, id_region))
  }
  
  R_samples <- 
    1:16 %>%
    purrr::map(r_eff_inla) %>%
    purrr::reduce(rbind)
}

get_r_martinez_besag <- function(casos0, vecinos, comunas) {
  r_eff_inla <- function(regions) {
    # Retrieve the areas in the selected regions
    communes <- 
      targets::tar_read("comunas") %>%
      dplyr::filter(codigo_region %in% regions) %>%
      dplyr::pull(codigo_comuna)
    
    # Retrieve the links
    edges <- 
      targets::tar_read("vecinos") %>%
      dplyr::rename(edge1 = codigo_comuna, edge2 = codigo_vecino) %>%
      dplyr::filter(
        edge1 %in% communes, 
        edge2 %in% communes
      )
    
    # Retrieve the cases
    cases <- 
      targets::tar_read("casos0") %>%
      dplyr::filter(
        codigo_comuna %in% c(edges$edge1, edges$edge2),
        codigo_region %in% regions
      ) %>%
      dplyr::rename(y = casos_nuevos)
    
    # Compute a standardized index for areas
    id_area <- 
      cases %>% 
      dplyr::mutate(id_area = dplyr::dense_rank(codigo_comuna)) %>%
      dplyr::distinct(codigo_comuna, id_area)
    
    # Compute a standardized index for times
    id_time <- 
      cases %>% 
      dplyr::mutate(id_time = dplyr::dense_rank(codigo_semana)) %>%
      dplyr::distinct(codigo_semana, id_time)
    
    # Add standardized ids to edges
    edges <- 
      edges %>%
      dplyr::inner_join(id_area, by = c("edge1" = "codigo_comuna")) %>%
      dplyr::inner_join(id_area, by = c("edge2" = "codigo_comuna"))
    
    # Compute adjacency matrix
    adj_mat <- 
      edges %$%
      sparseMatrix(i = id_area.x, j = id_area.y) %>%
      as("dgCMatrix")
    
    # Add standardized ids to cases
    cases <- 
      cases %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::arrange(id_area, id_time) %>%
      dplyr::select(id_area, id_time, y, n)
    
    # Compute B-splines design matrix for cases$id_time
    x <- unique(cases$id_time)
    knots <- seq.int(min(x), max(x), by = 12)
    bsMat <- splines2::bSpline(cases$id_time, knots = knots, degree = 3)
    bsMat0 <- splines2::bSpline(x, knots = knots, degree = 3)
    Nvars <- ncol(bsMat)
    bsdf <- 
      as.data.frame(bsMat) %>%
      magrittr::set_colnames(paste0("x", 1:Nvars))
    
    df <- cbind(cases, bsdf)
    prec_prior <- list(prec = list(param = c(0.001, 0.001)))
    
    for (i in 1:Nvars) {
      df[[paste0("id", i)]] <- df$id_area
    }
    
    fis  <- paste0("f(id", 1:Nvars, ", x", 1:Nvars, ", model = 'besag', graph = adj_mat, hyper = prec_prior)", collapse = " + ")
    fmla <- paste0("y ~ 0 + ", fis)
    fmla
    
    fit <- 
      INLA::inla(
        formula = as.formula(fmla),
        data = df,
        family = "poisson",
        E = n, 
        control.predictor = list(compute = TRUE),
        control.compute = list(
          dic = TRUE, 
          waic = TRUE, 
          config = TRUE, 
          openmp.strategy = "pardiso.parallel"
        )
      )
    
    nsims <- 2000L
    R_samples <- 
      inla.posterior.sample(n = nsims, fit, add.names = FALSE, seed = 1L) %>%
      purrr::map(function(x) {
        b <- 
          x[["latent"]] %>%
          tail(Nvars * nrow(id_area)) %>%
          matrix(Nvars, nrow(id_area))
        
        expXb <-
          exp(bsMat0 %*% b)
        
        ws <- 
          sim_weights()
        
        R_samples <- 
          my_slice(expXb, 5, 1) / (
            ws[1] * my_slice(expXb, 5, 2) + 
              ws[2] * my_slice(expXb, 5, 3) + 
              ws[3] * my_slice(expXb, 5, 4) + 
              ws[4] * my_slice(expXb, 5, 5)
          )
      }) %>%
      purrr::reduce(`+`) %>%
      magrittr::divide_by(nsims) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(id_time = 4 + 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        cols = -id_time, 
        values_to = "r",
        names_prefix = "V", 
        names_to = "id_area",
        names_transform = list(id_area = as.integer)
      ) %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::mutate(id_region = regions, method = "martinez 2") %>%
      dplyr::select(-c(id_area, id_time, id_region))
  }
  
  R_samples <- 
    1:16 %>%
    purrr::map(r_eff_inla) %>%
    purrr::reduce(rbind)
}

get_r_martinez_bym <- function(casos0, vecinos, comunas) {
  r_eff_inla <- function(regions) {
    # Retrieve the areas in the selected regions
    communes <- 
      targets::tar_read("comunas") %>%
      dplyr::filter(codigo_region %in% regions) %>%
      dplyr::pull(codigo_comuna)
    
    # Retrieve the links
    edges <- 
      targets::tar_read("vecinos") %>%
      dplyr::rename(edge1 = codigo_comuna, edge2 = codigo_vecino) %>%
      dplyr::filter(
        edge1 %in% communes, 
        edge2 %in% communes
      )
    
    # Retrieve the cases
    cases <- 
      targets::tar_read("casos0") %>%
      dplyr::filter(
        codigo_comuna %in% c(edges$edge1, edges$edge2),
        codigo_region %in% regions
      ) %>%
      dplyr::rename(y = casos_nuevos)
    
    # Compute a standardized index for areas
    id_area <- 
      cases %>% 
      dplyr::mutate(id_area = dplyr::dense_rank(codigo_comuna)) %>%
      dplyr::distinct(codigo_comuna, id_area)
    
    # Compute a standardized index for times
    id_time <- 
      cases %>% 
      dplyr::mutate(id_time = dplyr::dense_rank(codigo_semana)) %>%
      dplyr::distinct(codigo_semana, id_time)
    
    # Add standardized ids to edges
    edges <- 
      edges %>%
      dplyr::inner_join(id_area, by = c("edge1" = "codigo_comuna")) %>%
      dplyr::inner_join(id_area, by = c("edge2" = "codigo_comuna"))
    
    # Compute adjacency matrix
    adj_mat <- 
      edges %$%
      sparseMatrix(i = id_area.x, j = id_area.y) %>%
      as("dgCMatrix")
    
    # Add standardized ids to cases
    cases <- 
      cases %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::arrange(id_area, id_time) %>%
      dplyr::select(id_area, id_time, y, n)
    
    # Compute B-splines design matrix for cases$id_time
    x <- unique(cases$id_time)
    knots <- seq.int(min(x), max(x), by = 12)
    bsMat <- splines2::bSpline(cases$id_time, knots = knots, degree = 3)
    bsMat0 <- splines2::bSpline(x, knots = knots, degree = 3)
    Nvars <- ncol(bsMat)
    bsdf <- 
      as.data.frame(bsMat) %>%
      magrittr::set_colnames(paste0("x", 1:Nvars))
    
    df <- cbind(cases, bsdf)

    for (i in 1:Nvars) {
      df[[paste0("id", i)]] <- df$id_area
    }
    
    fis  <- paste0("f(id", 1:Nvars, ", x", 1:Nvars, ", model = 'bym', graph = adj_mat)", collapse = " + ")
    fmla <- paste0("y ~ 0 + ", fis)
    fmla
    
    fit <- 
      INLA::inla(
        formula = as.formula(fmla),
        data = df,
        family = "poisson",
        E = n, 
        control.predictor = list(compute = TRUE),
        control.compute = list(
          dic = TRUE, 
          waic = TRUE, 
          config = TRUE, 
          openmp.strategy = "pardiso.parallel"
        )
      )
    
    nsims <- 2000L
    R_samples <- 
      inla.posterior.sample(n = nsims, fit, add.names = FALSE, seed = 1L) %>%
      purrr::map(function(x) {
        b <- 
          x[["latent"]] %>%
          tail(Nvars * nrow(id_area)) %>%
          matrix(Nvars, nrow(id_area))
        
        expXb <-
          exp(bsMat0 %*% b)
        
        ws <- 
          sim_weights()
        
        R_samples <- 
          my_slice(expXb, 5, 1) / (
            ws[1] * my_slice(expXb, 5, 2) + 
              ws[2] * my_slice(expXb, 5, 3) + 
              ws[3] * my_slice(expXb, 5, 4) + 
              ws[4] * my_slice(expXb, 5, 5)
          )
      }) %>%
      purrr::reduce(`+`) %>%
      magrittr::divide_by(nsims) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(id_time = 4 + 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        cols = -id_time, 
        values_to = "r",
        names_prefix = "V", 
        names_to = "id_area",
        names_transform = list(id_area = as.integer)
      ) %>%
      dplyr::inner_join(id_area) %>%
      dplyr::inner_join(id_time) %>%
      dplyr::mutate(id_region = regions, method = "martinez 3") %>%
      dplyr::select(-c(id_area, id_time, id_region))
  }
  
  R_samples <- 
    1:16 %>%
    purrr::map(r_eff_inla) %>%
    purrr::reduce(rbind)
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
    dplyr::rename(codigo_region = codigo_region2) %>%
    dplyr::mutate(method = replace(method, method == "martinez 3", "M-B"))
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
      paso =  as.factor(paso)
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
    dplyr::ungroup()
}

get_model_df <- function(r_wallinga, covariates) {
  model_df <- 
    dplyr::inner_join(r_wallinga, covariates) %>%
    dplyr::mutate(dplyr::across(
      c(codigo_comuna, codigo_region), as.factor
    )) %>%
    dplyr::select(!dplyr::starts_with("im_")) %>%
    dplyr::select(
      r,
      casos_nuevos,
      comuna,
      codigo_semana,
      codigo_comuna,
      codigo_region,
      capital_regional,
      capital_provincial,
      pob_20_a_64,
      inmigrantes,
      aeropuerto,
      puerto,
      idse,
      indice_ruralidad,
      dplyr::contains("lag")
    )
  
  knots <- quantile(model_df$codigo_semana, p = (1:9) / 10)
  bsx <- splines2::bSpline(model_df$codigo_semana, knots = knots)
  colnames(bsx) <- paste0("bs", 1:ncol(bsx))
  cbind(model_df, bsx)
}

get_fit_oscar <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector(
    data     = na.omit(model_df),
    yvar0    = "r",
    xvar0    = covariates,
    random   = "(1 | codigo_region / codigo_comuna)",
    max_lag  = 5
  )
}

get_fit_oscar_gamma <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector_gamma(
    data     = na.omit(model_df),
    yvar0    = "r",
    xvar0    = covariates,
    random   = "(1 | codigo_region / codigo_comuna)",
    max_lag  = 5
  )
}

get_fit_oscar_nlme <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector_nlme(
    data     = na.omit(model_df),
    yvar0    = "r",
    xvar0    = covariates,
    random   = "~ 1 | codigo_region / codigo_comuna",
    max_lag  = 5
  )
}

get_fit_oscar_nlme_boxcox <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  lambdas <- seq(from = 0.00, to = 3, by = 0.1)
  best_lambda <- 0.0
  best_aic <- Inf
  for (lambda in lambdas) {
    if (lambda == 0) {
      model_df$rboxcox <- log(model_df$r)
    } else {
      model_df$rboxcox <- (model_df$r^lambda - 1) / lambda
    }
    fit <- 
      oscar_selector_nlme(
        data     = na.omit(model_df),
        yvar0    = "rboxcox",
        xvar0    = covariates,
        random   = "~ 1 | codigo_region / codigo_comuna",
        max_lag  = 5
      )
    if (AIC(fit) < best_aic) {
      best_lambda <- lambda
      best_aic <- AIC(fit)    
    }
  }
  return(list(best_lambda = best_lambda, fit = fit))
}

get_fit_oscar_nlme_boxcox_ar1 <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  lambdas <- seq(from = 0.00, to = 3, by = 0.1)
  best_lambda <- 0.0
  best_aic <- Inf
  for (lambda in lambdas) {
    if (lambda == 0) {
      model_df$rboxcox <- log(model_df$r)
    } else {
      model_df$rboxcox <- (model_df$r^lambda - 1) / lambda
    }
    fit <- 
      oscar_selector_nlme(
        data     = na.omit(model_df),
        yvar0    = "rboxcox",
        xvar0    = covariates,
        random   = "~ 1 | codigo_region / codigo_comuna",
        correlation = corAR1(form =  ~ codigo_semana | codigo_region / codigo_comuna),
        max_lag  = 5
      )
    if (AIC(fit) < best_aic) {
      best_lambda <- lambda
      best_aic <- AIC(fit)    
    }
  }
  return(list(best_lambda = best_lambda, fit = fit))
}

get_fit_oscar_nlme_ar1 <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector_nlme(
    data        = na.omit(model_df),
    yvar0       = "r",
    xvar0       = covariates,
    random      = "~ 1 | codigo_region / codigo_comuna",
    correlation = corAR1(form =  ~ codigo_semana | codigo_region / codigo_comuna),
    max_lag     = 5
  )
}

get_fit_oscar_nlme_ma1 <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector_nlme(
    data        = na.omit(model_df),
    yvar0       = "r",
    xvar0       = covariates,
    random      = "~ 1 | codigo_region / codigo_comuna",
    correlation = corARMA(p = 0, q = 1),
    max_lag     = 5
  )
}

get_fit_oscar_nlme_arma <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  oscar_selector_nlme(
    data        = na.omit(model_df),
    yvar0       = "r",
    xvar0       = covariates,
    random      = "~ codigo_semana | codigo_region / codigo_comuna",
    correlation = corARMA(p = 2, q = 2),
    max_lag     = 5
  )
}

get_fit_ivan <- function(model_df) {
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
    paste0("pp_vecinos_cuarentena_lag", 1:5),
    paste0("pcr_lag", 1:5),
    paste0("bs", 1:12)
  )
  mystepwise(
    yvar0    = "r",
    xvar0    = covariates,
    preserve = paste0("paso_lag", 1:5),
    random   = "(1 | codigo_region / codigo_comuna)",
    max_pval = 0.1,
    data     = na.omit(model_df)
  )
}

get_cov <- function(fit) {
  nlme::VarCorr(fit)
}

get_b <- function(fit) {
  broom.mixed::tidy(fit, effects = "fixed")
}

get_plot_r <- function(r) {
  plot <- long_boxplot(r, "r")
  ggsave("images/plot_r.png", plot, width = 7, height = 7)
  return("images/plot_r.png")  
}

get_plot_pp_vecinos_cuarentena <- function(pp_vecinos_cuarentena) {
  plot <- long_boxplot(pp_vecinos_cuarentena, "pp_vecinos_cuarentena")
  ggsave("images/plot_pp_vecinos_cuarentena.png", plot, width = 7, height = 7)
  return("images/plot_pp_vecinos_cuarentena.png")  
}

get_plot_pcr <- function(pcr) {
  plot <- long_boxplot(pcr, "pcr")
  ggsave("images/plot_pcr.png", plot, width = 7, height = 7)
  return("images/plot_pcr.png")  
}

get_plot_vacunados1 <- function(vacunados1) {
  plot <- long_boxplot(vacunados1, "vacunados1")
  ggsave("images/plot_vacunados1.png", plot, width = 7, height = 7)
  return("images/plot_vacunados1.png")  
}

get_plot_vacunados2 <- function(vacunados2) {
  plot <- long_boxplot(vacunados2, "vacunados2")
  ggsave("images/plot_vacunados2.png", plot, width = 7, height = 7)
  return("images/plot_vacunados2.png")  
}

get_plot_casos <- function(casos) {
  plot <- long_boxplot(casos, "casos_nuevos")
  ggsave("images/plot_casos.png", plot, width = 7, height = 7)
  return("images/plot_casos.png")  
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

  plot <- 
    ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.05) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")
  
  ggsave("images/plot_r_p10.png", plot, width = 7, height = 4)
  return("images/plot_r_p10.png")
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
  
  plot <- 
    ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.05) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")
  
  ggsave("images/plot_r_p50.png", plot, width = 7, height = 4)
  return("images/plot_r_p50.png")
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

  plot <- 
    ggplot2::ggplot(data = mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = r), size = 0.05) +
    ggplot2::facet_grid(cols = ggplot2::vars(method)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_grey() +
    ggplot2::labs(fill = "R efectivo")

  ggsave("images/plot_r_p90.png", plot, width = 7, height = 4)
  return("images/plot_r_p90.png")
}

get_plot_r_ts <- function(r, comunas) {
  regiones <-
    targets::tar_read(comunas) %>%
    dplyr::filter(capital_regional == 1) %>%
    dplyr::select(codigo_comuna, codigo_region, comuna) %>%
    dplyr::mutate(
      comuna = comuna %>%
        dplyr::recode(
          Concepcion = "Concepción",
          Valparaiso = "Valparaíso"
        )
    )

  plot <- 
    targets::tar_read(r) %>%
    dplyr::filter(codigo_region %in% c(5, 6, 8, 13)) %>%
    dplyr::select(!codigo_region) %>%
    dplyr::inner_join(regiones) %>%
    ggplot2::ggplot(aes(y = r, x = codigo_semana)) +
    ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y") +
    ggplot2::geom_line(aes(colour = comuna)) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "epidemiological week", y = "effective R")
  
  ggsave("images/plot_r_ts.png", plot, width = 7, height = 7)
  return("images/plot_r_ts.png")
}

get_plot_r_bp <- function(r, comunas) {
  plot <- 
    r %>%
    ggplot2::ggplot(aes(y = r, group = codigo_semana)) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y") +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "epidemiological week", y = "effective R")
  
  ggsave("images/plot_r_bp.png", plot, width = 7, height = 7)
  return("images/plot_r_bp.png")
}

get_plot_r_hist <- function(model_df) {
  T <- max(model_df$codigo_semana)
  plot <- 
    model_df %>%
    dplyr::filter(codigo_semana %in% (T - 4 * (0:3))) %>%
    ggplot2::ggplot(aes(x = r)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_grid(cols = ggplot2::vars(codigo_semana), scales = "free") +
    ggplot2::theme_classic()# +
   # ggplot2::labs(x = "epidemiological week", y = "effective R")
  # 
  ggsave("images/plot_r_hist.png", plot, width = 7, height = 7)
  return("images/plot_r_hist.png")
}

get_plot_fit_qqnorm <- function(fit_nlme) {
  png("images/plot_fit_qqnorm.png")
  qqnorm(resid(fit_nlme))
  qqline(resid(fit_nlme))
  dev.off()
  return("images/plot_fit_qqnorm.png")
}

get_plot_fit_acf <- function(fit_nlme) {
  png("images/plot_fit_acf.png")
  plot(nlme::ACF(fit_nlme))
  dev.off()
  return("images/plot_fit_acf.png")
}

get_plot_fit_yh <- function(fit_nlme, model_df) {
  plot <-
    model_df %>%
    na.omit() %>%
    dplyr::mutate(
      fitted = fitted(fit_nlme),
      comuna = comuna %>%
        dplyr::recode(
          Concepcion = "Concepción",
          Valparaiso = "Valparaíso"
        )
    ) %>%
    dplyr::filter(capital_regional == 1, codigo_region %in% c(5, 6, 8, 13)) %>%
    dplyr::select(actual = r, codigo_semana, comuna, fitted) %>%
    tidyr::pivot_longer(c(actual, fitted), names_to = "series") %>%
    ggplot2::ggplot(aes(x = codigo_semana, y = value, color = series)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(cols = ggplot2::vars(comuna))
  ggsave("images/plot_fit_yh.png", plot, width = 7, height = 7)
  return("images/plot_fit_yh.png")
}

get_plot_fit_boxcox_qqnorm <- function(fit_nlme_boxcox) {
  png("images/plot_fit_boxcox_qqnorm.png")
  qqnorm(resid(fit_nlme_boxcox$fit))
  qqline(resid(fit_nlme_boxcox$fit))
  dev.off()
  return("images/plot_fit_boxcox_qqnorm.png")
}

get_plot_fit_boxcox_acf <- function(fit_nlme_boxcox) {
  png("images/plot_fit_boxcox_acf.png")
  plot(nlme::ACF(fit_nlme_boxcox$fit))
  dev.off()
  return("images/plot_fit_boxcox_acf.png")
}

get_plot_fit_boxcox_ar1_qqnorm <- function(fit_nlme_boxcox_ar1) {
  png("images/plot_fit_boxcox_ar1_qqnorm.png")
  qqnorm(resid(fit_nlme_boxcox_ar1$fit))
  qqline(resid(fit_nlme_boxcox_ar1$fit))
  dev.off()
  return("images/plot_fit_boxcox_ar1_qqnorm.png")
}

get_plot_fit_boxcox_ar1_acf <- function(fit_nlme_boxcox_ar1) {
  png("images/plot_fit_boxcox_ar1_acf.png")
  plot(nlme::ACF(fit_nlme_boxcox_ar1$fit))
  dev.off()
  return("images/plot_fit_boxcox_ar1_acf.png")
}

tbl_b <- function(fit, name) {
  tbl <- broom.mixed::tidy(fit, effects = "fixed")
  write.csv(tbl, name)
  return(name)
}
