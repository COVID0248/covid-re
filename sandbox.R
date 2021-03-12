# Setup ----

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
import::from(magrittr, "%>%", "%$%", "%T>%")

source("R/wallinga.R")
source("R/utils.R")

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

# R efectivo (filtrado) ----

# Solo codigo_region == 13
plot_01 <-
  re %>%
  dplyr::filter(codigo_region == 13) %>%
  dplyr::mutate(method = as.factor(method)) %>%
  dplyr::group_by(method, codigo_semana) %>%
  dplyr::summarize(
    r_min = min(Rt),
    r_max = max(Rt)
  ) %>%
  tidyr::gather("stat", "value", -c(codigo_semana, method)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(
    x      = codigo_semana,
    y      = value,
    colour = stat
  )) +
  ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y")
ggplot2::ggsave(plot_01, file = "images/plot_01.pdf")

# Solo codigo_semana >= 10
plot_02 <-
  re %>%
  dplyr::filter(codigo_semana >= 10) %>%
  dplyr::mutate(method = as.factor(method)) %>%
  dplyr::group_by(method, codigo_semana) %>%
  dplyr::summarize(
    r_min = min(Rt),
    r_max = max(Rt)
  ) %>%
  tidyr::gather("stat", "value", -c(codigo_semana, method)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(
    x      = codigo_semana,
    y      = value,
    colour = stat
  )) +
  ggplot2::facet_grid(rows = ggplot2::vars(method), scales = "free_y")
ggplot2::ggsave(plot_02, file = "images/plot_02.pdf")

# Primera versión de un modelo ----

# Número de habitantes, según comuna y región
poblacion_comuna <-
  readRDS("data/comunas.rds") %>%
  dplyr::select(cod_comuna, cod_region, poblacion)

# Porcentaje de población veicna en cuarentena,
# según comuna, id de cuarentena y semana de cuarentena
pp_vecinos_cuarentena <-
  readRDS("data/pp_vecinos_cuarentena.rds")

# Limpia la bbdd suministrada por Oscar
data <-
  read.csv("data/tabla_7nov.csv") %>%
  dplyr::arrange(comuna, id_cuarentena, semana) %>%
  dplyr::mutate(comuna_cuarentena = paste(comuna, id_cuarentena)) %>%
  dplyr::select(-ips) %>%
  dplyr::group_by(comuna_cuarentena) %>%
  dplyr::mutate(
    duracion_cuarentena = dplyr::n(),
    log_nuevos_x10mil = log(1 + nuevos_x10mil),
    lag_log_nuevos_x10mil = dplyr::lag(log_nuevos_x10mil)
  ) %>%
  dplyr::filter(duracion_cuarentena > 2) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(
    poblacion_comuna,
    by = c("codigo_comuna" = "cod_comuna")
  ) %>%
  dplyr::mutate(
    inmigrantes =
      inmigrantes / poblacion,
    poblacion_edad_20_a_64 =
      poblacion_edad_15_a_64 / poblacion -
      poblacion_edad_15_a_19 / poblacion,
    semana_fct = as.factor(semana),
    region_fct = as.factor(cod_region),
    comuna_fct = as.factor(codigo_comuna),
    semana2 = semana^2
  ) %>%
  # Reescalamientos cosméticos
  dplyr::mutate(
    densidad_poblacional = densidad_poblacional / 1000,
    hacinamiento = hacinamiento / 100,
    inmigrantes = inmigrantes / 100,
    idse = idse / 1000,
    indice_ruralidad = indice_ruralidad / 100
  ) %>%
  # Agrega el rezago de pp de vecinos en cuarentena
  dplyr::inner_join(pp_vecinos_cuarentena) %>%
  dplyr::arrange(codigo_comuna, id_cuarentena, semana) %>%
  dplyr::group_by(codigo_comuna, id_cuarentena) %>%
  dplyr::mutate(pvcl1 = dplyr::lag(pvc)) %>%
  dplyr::ungroup() #%>%
  #na.omit()

# Une los RE calculados previamente con id_cuarentena x semana_cuarentena
df_final <-
  readRDS("data/re_wallinga.rds") %>%
  dplyr::rename(semana = codigo_semana, comuna = codigo_comuna) %>%
  dplyr::inner_join(readRDS("data/semana_cuarentena.rds")) %>%
  dplyr::select(Rt, id_cuarentena, semana = semana_cuarentena, codigo_comuna = comuna) %>%
  dplyr::inner_join(data) %>%
  na.omit() %>%
  dplyr::ungroup()

# Crea un listado con los ajustes de los dos modelos considerados hasta ahora
source("R/utils.R")
fit <- 
  c(
    "activos_comienzo",
    "capital_regional",
    "capital_provincial",
    "aeropuerto",
    "puerto",
    "poblacion_edad_20_a_64",
    "densidad_poblacional",
    "hacinamiento",
    "inmigrantes",
    "idse",
    "indice_ruralidad",
    "pvcl1"
  ) %>%
  {list(c(., "semana_fct"), c(., "semana", "semana2"))} %>%
  purrr::map(~ mystepwise(
    yvar0       = "Rt",
    xvar0       = .x,
    preserve    = c("pvcl1", "semana", "semana2", "semana_fct"),
    random      = "(1 | region_fct / comuna_fct)", 
    max_pval    = 0.1,
    data        = df_final
  ))

# Guarda las tablas con los resultados (coeficientes)
file <- 'data/resumen modelo {.x} - efectos fijos.csv'
1:2 %>%
  purrr::walk(
    ~ broom.mixed::tidy(fit[[.x]], effects = "fixed") %>%
      write.csv(glue::glue(file), row.names = FALSE)
  )

# Guarda las tablas con los resultados (varianzas de los re)
file <- 'data/resumen modelo {.x} - efectos aleatorios.csv'
1:2 %>%
  purrr::walk(
    ~ nlme::VarCorr(fit[[.x]]) %>% 
      write.csv(glue::glue(file))
  )

# Grafica los valores ajustados
1:2 %>%
  purrr::map(
    ~ df_final %>%
      dplyr::mutate(
        yhat = fitted(fit[[.x]]), 
        model_id = paste("Modelo", .x)
      )) %>%
  purrr::reduce(rbind) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = semana, 
    y = yhat, 
    group = comuna_cuarentena, 
    color = comuna_cuarentena
  )) +
  ggplot2::geom_line(size = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(
    y = "Casos nuevos (por 10.000 habitantes) ajustados por el modelo",
    x = "Semanas desde el inicio de la cuarentena",
    color = "comuna, según id de cuarentena"
  ) +
  ggplot2::facet_wrap(~ model_id)
ggplot2::ggsave("images/plot-01.pdf")


