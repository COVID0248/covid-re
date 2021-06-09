# Install INLA
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(dplyr)
library(purrr)
library(INLA)
library(sf)
library(sn)
library(magrittr)
library(splines2)
inla.setOption(pardiso.license = "~/sys/licenses/pardiso.lic")
options(mc.cores = parallel::detectCores())

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

# Compute adjacency matrix
adj_mat <- 
  edges %$%
  sparseMatrix(i = id_area.x, j = id_area.y) %>%
  as("dgCMatrix")

# Add standardized ids to cases
cases <- 
  cases %>%
  dplyr::inner_join(id_area) %>%
  dplyr::inner_join(id_time) %>%
  dplyr::arrange(id_area, id_time) %>%
  dplyr::select(id_area, id_time, y, n)

# Compute B-splines design matrix for cases$id_time
x <- unique(cases$id_time)
knots <- seq.int(min(x), max(x), by = 12)
bsMat <- splines2::bSpline(cases$id_time, knots = knots, degree = 3)
Nvars <- ncol(bsMat)
bsdf <- 
  as.data.frame(bsMat) %>%
  magrittr::set_colnames(paste0("x", 1:Nvars))

df <- cbind(cases, bsdf)
prec_prior <- list(prec = list(param = c(0.001, 0.001)))

for (i in 1:Nvars) {
  df[[paste0("id", i)]] <- df$id_area
}

fis  <- paste0("f(id", 1:Nvars, ", x", 1:Nvars, ", model = 'besag', graph = adj_mat, hyper = prec_prior)", collapse = " + ")
fmla <- paste0("y ~ 0 + ", fis)
fmla

fit <- 
  INLA::inla(
    formula = as.formula(fmla),
    data = df,
    family = "poisson",
    E = n, 
    control.predictor = list(compute = TRUE),
    control.compute = list(
      dic = TRUE, 
      waic = TRUE, 
      config = TRUE, 
      openmp.strategy = "pardiso.parallel"
    )
  )

expXB_samples <- 
  inla.posterior.sample(n = 2L, fit, add.names = FALSE) %>%
  purrr::map(function(x) {
    b <- 
      x[["latent"]] %>%
      tail(Nvars * nrow(id_area)) %>%
      matrix(Nvars, nrow(id_area))
    exp(bsMat %*% b)
  })


w <- numeric(4)
get_w <- function(a0, b0, maxlag = 24) {
  
}