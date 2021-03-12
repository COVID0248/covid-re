R_from_r <- function (r, GT) {
  Tmax <- length(GT$GT)
  R <- 1/sum(GT$GT * (exp(-r * (0:(Tmax - 1)))))
}

est_re_exp <- function(ts, GT_obj, half_window_width = 3L) {
  res <- sapply(
    (half_window_width + 1):(length(ts) - half_window_width), 
    function(t) {
      idx <- (t - half_window_width):(t + half_window_width)
      data <- data.frame(Date = 1:length(ts), y = as.numeric(ts))
      m <- glm(y ~ 1 + Date, poisson, dplyr::slice(data, idx))
      r <- as.numeric(coef(m)["Date"])
      R <- R_from_r(r, GT_obj)
    }
  )
  names(res) <- 
    names(ts)[(half_window_width + 1):(length(ts) - half_window_width)]
  return(res)
}

date_to_sepi <- function(x) {
  diff <- difftime(
    strptime(x, format = "%Y-%m-%d"),
    strptime("2020-02-16", format = "%Y-%m-%d"),
    units="weeks"
  )
  return(as.integer(diff) + 8)
}

mystepwise <- function(yvar0, xvar0, preserve, max_pval, random, data) {
  xdel = " "
  while (xdel != "") {
    fmla <- paste0(yvar0, " ~ ", paste(xvar0, collapse = "+"), " + ", random)
    xdel <-
      lme4::lmer(as.formula(fmla), data = data) %>%
      drop1(test = "Chisq") %>%
      broom.mixed::tidy(effects = "fixed") %>%
      dplyr::filter(!(term %in% preserve)) %>%
      na.omit() %>%
      dplyr::filter(Pr.Chi. == max(Pr.Chi.)) %$%
      ifelse(Pr.Chi. > max_pval, term, "")
    xvar0 <- xvar0[xvar0 != xdel]
  }
  fmla <- paste0(yvar0, " ~ ", paste(xvar0, collapse = "+"), " + ", random)
  do.call(lme4::lmer, list(formula = as.formula(fmla), data = data))
}

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
  stan_fit   <- rstan::sampling(object = stan_model, data = stan_data)
  
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

get_r <- function(data_list) {
  r_final <-
    data_list %>%
    dplyr::bind_rows() %>%
    dplyr::select(-c(n, casos_nuevos)) %>%
    dplyr::ungroup()
}

get_df <- function(df_r, df_pasos, df_comunas) {
  df_r %>%
  dplyr::inner_join(df_pasos) %>%
  dplyr::inner_join(df_comunas) %>%
  dplyr::arrange(codigo_comuna, codigo_semana) %>%
  dplyr::mutate(across(
    c(idse, inmigrantes, densidad_poblacional), 
    ~ .x / max(.x, na.rm = TRUE)
  )) %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(
    comuna_fct = as.factor(codigo_comuna),
    region_fct = as.factor(codigo_region),
    paso_l1    = as.factor(dplyr::lag(paso, 1)),
    paso_l3    = as.factor(dplyr::lag(paso, 3)),
    paso_l5    = as.factor(dplyr::lag(paso, 5))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    r,
    codigo_semana,
    region_fct,
    comuna_fct,
    comuna,
    capital_regional,
    capital_provincial,
    pob_20_a_64,
    inmigrantes,
    aeropuerto,
    puerto,
    idse,
    indice_ruralidad,
    paso_l1,
    paso_l3,
    paso_l5
  ) %>%
  na.omit()
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
    "paso_l1",
    "paso_l3",
    "paso_l5"
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

# Probar rezagos más grandes (5)
# Añadr espacios entre los rezagos (e.g. 1, 3, 5)
# Añadir un par de gráficos comparando el R con las variables que queden en el modelo
# Buscar variables que varíen en el tiempo
