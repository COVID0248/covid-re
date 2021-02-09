# Head ----

import::from(magrittr, "%>%")
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Body ----

# Calcula los casos nuevos, según comuna y semana epidemiológica
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
    casos_nuevos  = pmax(casos_confirmados, 1) 
  ) %>%
  dplyr::select(codigo_comuna, codigo_semana, casos_nuevos) %>%
  dplyr::arrange(codigo_comuna, codigo_semana)

# Prepara los datos para Stan 
Nareas <- length(unique(data$codigo_comuna))
Ntimes <- length(unique(data$codigo_semana))
stan_data <- 
  list(
    gamma  = 1,
    Ntimes = Ntimes,
    Nareas = Nareas,
    I      = array(data$casos_nuevos, c(Ntimes, Nareas))
  )
stan_model <- rstan::stan_model("stan/systrom.stan")
stan_fit   <- rstan::sampling(object = stan_model, data = stan_data)

# Cislaghi ----

rt_cislaghi <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1))

# JRC ----

rt_jrc <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = log(casos_nuevos / dplyr::lag(casos_nuevos)) + 1)


# RKI ----

rt_rki <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1))

# RKI ----
