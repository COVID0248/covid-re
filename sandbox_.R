library(dplyr)
library(rstan)
library(magrittr)
library(splines2)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Retrieve the cases and the neighbors
targets::tar_make("casos0")
targets::tar_make("vecinos")
casos0 <- targets::tar_read("casos0") 
vecinos <- targets::tar_read("vecinos") %>%
  dplyr::filter(codigo_comuna < codigo_vecino)

# Standardize the comunne indices
id_comuna <- 
  targets::tar_read("vecinos") %>% 
  dplyr::distinct(codigo_comuna) %>%
  dplyr::mutate(id = 1:dplyr::n())
vecinos <- 
  targets::tar_read("vecinos") %>%
  dplyr::inner_join(id_comuna, by = "codigo_comuna") %>%
  dplyr::inner_join(id_comuna, by = c("codigo_vecino" = "codigo_comuna"))

# B-spline example
x <- unique(casos0$codigo_semana)
knots <- seq.int(min(x), max(x), by = 3)
bsMat <- splines2::bSpline(x, knots = knots, degree = 3, intercept = TRUE)
matplot(x, bsMat, type = "l", ylab = "y")
abline(v = knots, lty = 2, col = "gray")

head(casos0)
head(vecinos)

# Infectivity rates
w <- c(0.5, 0.5)

# Prepare the dataset
Nareas = dplyr::n_distinct(casos0$codigo_comuna)
Ntimes = dplyr::n_distinct(casos0$codigo_semana)
stan_data <- list(
  Nlags  = length(w),
  Nedges = nrow(vecinos),
  Nareas = Nareas,
  Ntimes = Ntimes,
  Npreds = ncol(bsMat),
  i      = unique(casos0$codigo_comuna),
  t      = unique(casos0$codigo_semana),
  Y      = array(casos0$casos_nuevos, c(Ntimes, Nareas)),
  X      = bsMat,
  edge1  = vecinos$id.x,
  edge2  = vecinos$id.y,
  w      = w
)

object <- rstan::stan_model("stan/model_beneito.stan")
fit <- rstan::sampling(object = object, data = stan_data, iter = 4)
