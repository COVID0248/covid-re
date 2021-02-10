# Setup ----

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
import::from(magrittr, "%>%", "%$%", "%T>%")

source("R/wallinga.R")

# Datos ----

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

# R efectivo (Systrom) ----

# Nareas <- length(unique(data$codigo_comuna))
# Ntimes <- length(unique(data$codigo_semana))
# stan_data <- 
#   list(
#     gamma  = 1,
#     Ntimes = Ntimes,
#     Nareas = Nareas,
#     I      = array(data$casos_nuevos, c(Ntimes, Nareas))
#   )
# stan_model <- rstan::stan_model("stan/systrom.stan")
# stan_fit   <- rstan::sampling(object = stan_model, data = stan_data)

# R efectivo (Cislaghi) ----

rt_cislaghi <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1)) %T>%
  saveRDS("data/cislaghi.rds")

# R efectivo (JRC) ----

rt_jrc <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = log(casos_nuevos / dplyr::lag(casos_nuevos)) + 1) %T>%
  saveRDS("data/jrc.rds")

# R efectivo (RKI) ----

rt_rki <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1)) %T>%
  saveRDS("data/rki.rds")

# R efectivo (Wallinga) ----

rt_wallinga <- 
  unique(data$codigo_comuna) %>%
  purrr::map(
    ~ data %>%
      dplyr::filter(codigo_comuna == .x) %$%
      est_rt_exp(
        setNames(casos_nuevos, codigo_semana),
        GT_obj = R0::generation.time("gamma", c(6.6 / 7, 1.5 / 7^2)),
        half_window_width = 3
      ) %>%
    data.frame(Date = names(.), R_hat = ., codigo_comuna = .x)
  ) %>%
  purrr::reduce(rbind) %T>%
  saveRDS("data/wallinga.rds")
  