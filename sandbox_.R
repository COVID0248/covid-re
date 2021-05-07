library(dplyr)
library(rstan)
library(splines2)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Retrieve the cases and the neighbors
targets::tar_make("casos0")
targets::tar_make("vecinos")
casos0 <- targets::tar_read("casos0")
vecinos <- targets::tar_read("vecinos")

# B-spline example
x <- unique(casos0$codigo_semana)
knots <- seq.int(min(x), max(x), by = 3)
bsMat <- splines2::bSpline(x, knots = knots, degree = 3, intercept = TRUE)
matplot(x, bsMat, type = "l", ylab = "y")
abline(v = knots, lty = 2, col = "gray")

head(casos0)
head(vecinos)

# Prepare the dataset
w <- c(0.5, 0.5)
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
  edge1  = vecinos$codigo_comuna,
  edge2  = vecinos$codigo_vecino,
  w      = c(0.5, 0.5)
)

# fit <- rstan::stan(file = 'stan/model_bym.stan', data = stan_data)
object <- rstan::stan_model("stan/model_bym.stan")
fit <- rstan::sampling(object = object, data = stan_data, iter = 20)
