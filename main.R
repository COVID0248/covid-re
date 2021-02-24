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
    casos_nuevos  = pmax(casos_confirmados, 1),
    n             = poblacion
  ) %>%
  dplyr::select(dplyr::contains("codigo"), n, casos_nuevos) %>%
  dplyr::arrange(codigo_comuna, codigo_semana)

# R efectivo (Systrom) ----

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

re_systrom <-
  data %>%
  dplyr::arrange(codigo_semana, codigo_comuna) %>%
  dplyr::mutate(
    Rt = rstan::get_posterior_mean(stan_fit, "R")[, 5],
    method = "systrom"
  ) %>%
  dplyr::as_tibble() %T>%
  saveRDS("data/re_systrom.rds")

# R efectivo (Cislaghi) ----

re_cislaghi <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(
    Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1),
    method = "cislaghi"
  ) %T>%
  saveRDS("data/re_cislaghi.rds")

# R efectivo (JRC) ----

re_jrc <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(
    Rt = log(casos_nuevos / dplyr::lag(casos_nuevos)) + 1,
    method = "jrc"
  ) %T>%
  saveRDS("data/re_jrc.rds")

# R efectivo (RKI) ----

re_rki <-
  data %>%
  dplyr::group_by(codigo_comuna) %>%
  dplyr::mutate(
    Rt = casos_nuevos / dplyr::lag(casos_nuevos, 1),
    method = "rki"
  ) %T>%
  saveRDS("data/re_rki.rds")

# R efectivo (Wallinga) ----

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
        Rt            = .
      )
  ) %>%
  purrr::reduce(rbind) %>%
  dplyr::inner_join(data, by = c("codigo_comuna", "codigo_semana")) %T>%
  saveRDS("data/re_wallinga.rds")

# Base final ----

re <- 
  list.files("data", full.names = TRUE) %>% 
  purrr::map(readRDS) %>%
  dplyr::bind_rows() %>%  
  dplyr::ungroup() %T>%
  saveRDS("data/re.rds")

# # R efectivo (filtrado) ----
# 
# # Solo codigo_region == 13
# plot_01 <- 
#   re %>%
#   dplyr::filter(codigo_region == 13) %>%
#   dplyr::mutate(method = as.factor(method)) %>%
#   dplyr::group_by(method, codigo_semana) %>%
#   dplyr::summarize(
#     r_min = min(Rt), 
#     r_max = max(Rt)
#   ) %>%
#   tidyr::gather("stat", "value", -c(codigo_semana, method)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(
#     x      = codigo_semana, 
#     y      = value, 
#     colour = stat
#   )) +
#   ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y")
# ggplot2::ggsave(plot_01, file = "images/plot_01.pdf")
# 
# # Solo codigo_semana >= 10
# plot_02 <- 
#   re %>%
#   dplyr::filter(codigo_semana >= 10) %>%
#   dplyr::mutate(method = as.factor(method)) %>%
#   dplyr::group_by(method, codigo_semana) %>%
#   dplyr::summarize(
#     r_min = min(Rt), 
#     r_max = max(Rt)
#   ) %>%
#   tidyr::gather("stat", "value", -c(codigo_semana, method)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(
#     x      = codigo_semana, 
#     y      = value, 
#     colour = stat
#   )) +
#   ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y")
# ggplot2::ggsave(plot_02, file = "images/plot_02.pdf")
