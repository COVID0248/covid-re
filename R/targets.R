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

get_vecinos <- function() {
  readRDS("data/vecinos.rds")
}

get_pob_20_64 <- function(conn) {
  conn %>%
    dplyr::tbl("poblacion_edad") %>% 
    dplyr::select(
      codigo_comuna, 
      edad, 
      n = total_poblacion_efectivamente_censada
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(
      where(bit64::is.integer64), 
      as.integer
    )) %>%
    dplyr::filter(edad %in% 20:64) %>%
    dplyr::group_by(codigo_comuna) %>%
    dplyr::summarise(pob_20_a_64 = sum(n))
}

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
    )
}

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
    dplyr::inner_join(pob_20_64)
}

get_poblacion <- function(comunas) {
  dplyr::select(comunas, codigo_comuna, poblacion)
}

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
    tibble::as_tibble()
}

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
    tibble::as_tibble()
}

get_pcr <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto7/PCR_std.csv") %>%
    read.csv() %>%
    dplyr::mutate(
      codigo_region = Codigo.region, 
      codigo_semana = date_to_sepi(fecha)
    ) %>%
    dplyr::group_by(codigo_region, codigo_semana) %>%
    dplyr::summarise(pcr = sum(numero), .groups = "drop") %>%
    dplyr::arrange(codigo_region, codigo_semana) %>%
    tibble::as_tibble()
}

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
    dplyr::summarise(vacunados1 = sum(Primera.Dosis), .groups = "drop") %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::as_tibble()
}

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
    dplyr::summarise(vacunados2 = sum(Segunda.Dosis), .groups = "drop") %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::as_tibble()
}

get_cuarentenas <- function() {
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
    cuarentenas %$%
    expand.grid(
      codigo_comuna = unique(codigo_comuna),
      codigo_semana = min(inicio):max(termino),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    dplyr::as_tibble()
  
  cuarentenas %<>%
    dplyr::inner_join(comunas) %>%
    dplyr::arrange(codigo_comuna, codigo_semana) %>%
    dplyr::mutate(
      cuarentena = codigo_semana >= inicio & codigo_semana <= termino
    ) %>%
    dplyr::select(codigo_comuna, codigo_semana, cuarentena)
}

get_pvc <- function(poblacion, vecinos, cuarentenas, pasos) {
  pvc <- 
    list(cuarentenas, pasos, poblacion, vecinos) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(
      paso = dplyr::if_else(is.na(paso), 4L - 3L * cuarentena, paso)
    ) %>%
    dplyr::arrange(codigo_semana, codigo_vecino) %>%
    dplyr::group_by(codigo_semana, codigo_vecino) %>%
    dplyr::summarise(pvc = sum(poblacion * (paso == 1)) / sum(poblacion)) %>%
    dplyr::rename(codigo_comuna = codigo_vecino) %>%
    dplyr::arrange(codigo_comuna, codigo_semana)
}

get_vacaciones <- function() {
  inicio  <- date_to_sepi(as.Date("2021-01-01"))
  termino <- date_to_sepi(as.Date("2021-02-28"))
  dplyr::tibble(
    codigo_semana = 1:date_to_sepi(Sys.Date()),
    vacaciones    = dplyr::between(codigo_semana, inicio, termino)
  )
}

get_r_systrom <- function(data) {
  Nareas <- dplyr::n_distinct(data$codigo_comuna)
  Ntimes <- dplyr::n_distinct(data$codigo_semana)
  stan_data <-
    list(
      gamma  = 1,
      Ntimes = Ntimes,
      Nareas = Nareas,
      I      = array(data$casos_nuevos, c(Ntimes, Nareas))
    )
  stan_model <- rstan::stan_model("stan/systrom.stan")
  stan_fit <- rstan::sampling(
    object = stan_model, 
    data   = stan_data, 
    seed   = 1L
  )
  
  re_systrom <-
    data %>%
    dplyr::arrange(codigo_semana, codigo_comuna) %>%
    dplyr::mutate(
      r = rstan::get_posterior_mean(stan_fit, "R")[, 5],
      method = "systrom"
    ) %>%
    tibble::as_tibble()
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
}

get_df <- function(df_r, ...) {
  df <- 
    list(df_r, ...) %>%
    purrr::reduce(dplyr::left_join) %>%
    tidyr::replace_na(list(
      vacunados1 = 0, 
      vacunados2 = 0,
      cuarentena = FALSE
    )) %>%
    dplyr::mutate(
      paso = dplyr::if_else(is.na(paso), 4L - 3L * cuarentena, paso),
      idse = idse / 1000,
      densidad_1em4 = densidad_poblacional * 1e-4,
      across(
        .cols = c(pob_20_a_64, inmigrantes, vacunados1, vacunados2),
        .fns  = ~ .x / poblacion
      ),
      dplyr::across(c(codigo_comuna, codigo_region), as.factor)
    ) #%>%
    # dplyr::arrange(codigo_comuna, codigo_semana) %>%
    # dplyr::group_by(codigo_comuna) %>%
    # dplyr::mutate(
    #   !!!lags(vacunados1, 5),
    #   !!!lags(vacunados2, 5),
    #   !!!lags(paso, 5),
    #   !!!lags(pvc, 5)
    # ) %>%
    # dplyr::ungroup() %>%
    # dplyr::select(
    #   r,
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
    # ) #%>%
  # na.omit()    
  
  
  

  
  
    # dplyr::group_by(codigo_comuna) %>%
    # dplyr::mutate(
    #   comuna_fct = as.factor(codigo_comuna),
    #   region_fct = as.factor(codigo_region)
    # )
  
  # purrr::reduce(
  #   1:4,
  #   ~ dplyr::mutate(., dplyr::across(
  #     .cols  = c(paso, vacuna1, vacuna2),
  #     .fns   = ~dplyr::lag(.x, n = 1),
  #     .names = "{.col}_lag"
  #   ))
  # )
  
  
  # paso_lag1  = as.factor(dplyr::lag(paso, 1)),
  # paso_lag3  = as.factor(dplyr::lag(paso, 3)),
  # paso_lag5  = as.factor(dplyr::lag(paso, 5))
  # dplyr::ungroup() #%>%
  # dplyr::select(
  #   r,
  #   codigo_semana,
  #   region_fct,
  #   comuna_fct,
  #   comuna,
  #   capital_regional,
  #   capital_provincial,
  #   pob_20_a_64,
  #   inmigrantes,
  #   aeropuerto,
  #   puerto,
  #   idse,
  #   indice_ruralidad,
  #   paso_lag1,
  #   paso_lag3,
  #   paso_lag5
  # ) %>%
  # na.omit()
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
    "paso_lag1",
    "paso_lag3",
    "paso_lag5"
  )
  mystepwise(
    yvar0    = "r",
    xvar0    = covariates,
    preserve = "",
    random   = "(1 | region_fct / comuna_fct)",
    max_pval = 0.1,
    data     = df
  )
}

get_cov <- function(fit) {
  nlme::VarCorr(fit)
}

get_b <- function(fit) {
  broom.mixed::tidy(fit, effects = "fixed")
}

# TODO:
# 1. Añadir lags de paso, vacuna1, vacuna2