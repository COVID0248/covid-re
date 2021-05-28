library(dplyr)
library(rstan)
library(magrittr)
library(splines2)
library(posterior)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Retrieve the cases and the neighbors
targets::tar_make("casos0")
targets::tar_make("vecinos")
casos0 <- targets::tar_read("casos0") 
vecinos <- targets::tar_read("vecinos") %>%
  dplyr::filter(codigo_comuna < codigo_vecino)

# Compute standardized indices for communes
id_area <- 
  targets::tar_read("vecinos") %>% 
  dplyr::distinct(codigo_comuna) %>%
  dplyr::mutate(id_area = 1:dplyr::n())

# Compute standardized indices for weeks
id_time <-
  targets::tar_read("casos0") %>% 
  dplyr::distinct(codigo_semana) %>%
  dplyr::arrange(codigo_semana) %>%
  dplyr::mutate(id_time = 1:dplyr::n())

# Add standardized ids to vecinos
edges <- 
  targets::tar_read("vecinos") %>%
  dplyr::inner_join(id_area, by = "codigo_comuna") %>%
  dplyr::inner_join(id_area, by = c("codigo_vecino" = "codigo_comuna"))

# Add standardized ids to casos0
cases <- 
  casos0 %>%
  inner_join(id_area) %>%
  inner_join(id_time) %>%
  arrange(id_area, id_time) %>%
  select(id_area, id_time, y = casos_nuevos, n)

# Compute B-splines design matrix for cases$id_week
x <- unique(cases$id_time)
knots <- seq.int(min(x), max(x), by = 3)
bsMat <- splines2::bSpline(x, knots = knots, degree = 3)

# Specify the infectivity profile
w <- c(0.5, 0.5)

# Prepare the dataset
Nareas = dplyr::n_distinct(cases$id_area)
Ntimes = dplyr::n_distinct(cases$id_time)
stan_data <- list(
  Nlags  = length(w),
  Nedges = nrow(edges),
  Nareas = Nareas,
  Ntimes = Ntimes,
  Npreds = ncol(bsMat),
  y      = array(cases$y, c(Ntimes, Nareas)),
  n      = filter(cases, id_time == 1) %>% pull(n),
  x      = bsMat,
  edge1  = edges$id_area.x,
  edge2  = edges$id_area.y,
  w      = w
)

fit <- stan(file = "stan/model_beneito.stan", data = stan_data, iter = 4)
Res <- rstan::extract(fit, "R")$R
Re_mean <- apply(Res, c(2, 3), mean)
