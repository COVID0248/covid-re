library(dplyr)
library(rstan)
library(magrittr)
library(splines2)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Build the relevant part of the project
targets::tar_make("casos0")
targets::tar_make("vecinos")
targets::tar_make("comunas")

# Set the target regions
regions <- 13

# Retrieve the areas in the selected regions
communes <- 
  targets::tar_read("comunas") %>%
  dplyr::filter(codigo_region %in% regions) %>%
  pull(codigo_comuna)

# Retrieve the links
edges <- 
  targets::tar_read("vecinos") %>%
  dplyr::rename(edge1 = codigo_comuna, edge2 = codigo_vecino) %>%
  dplyr::filter(
    edge1 < edge2, 
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

# Add standardized ids to cases
cases <- 
  cases %>%
  dplyr::inner_join(id_area) %>%
  dplyr::inner_join(id_time) %>%
  dplyr::arrange(id_area, id_time) %>%
  dplyr::select(id_area, id_time, y, n)

# Compute B-splines design matrix for cases$id_time
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

fit <-
  "stan/model_beneito.stan" %>%
  rstan::stan(
    data = stan_data,
    iter = 10,
    seed = 1,
    control = list(
      max_treedepth = 15,
      adapt_delta = 0.90
    )
  )
