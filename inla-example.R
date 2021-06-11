# Install INLA
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(dplyr)
library(purrr)
library(INLA)
library(sf)
library(sn)
library(EpiEstim)
library(magrittr)
library(splines2)
inla.setOption(pardiso.license = "~/sys/licenses/pardiso.lic")
options(mc.cores = parallel::detectCores())
set.seed(1)

# Build the relevant part of the project
targets::tar_make("casos0")
targets::tar_make("vecinos")
targets::tar_make("comunas")

# Set the target regions
regions <- 13

r_eff_inla <- function(regions) {
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
  bsMat0 <- splines2::bSpline(x, knots = knots, degree = 3)
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
  
  nsims <- 2000L
  R_samples <- 
    inla.posterior.sample(n = nsims, fit, add.names = FALSE, seed = 1L) %>%
    purrr::map(function(x) {
      b <- 
        x[["latent"]] %>%
        tail(Nvars * nrow(id_area)) %>%
        matrix(Nvars, nrow(id_area))
      
      expXb <-
        exp(bsMat0 %*% b)
      
      ws <- 
        sim_weights()
      
      R_samples <- 
        my_slice(expXb, 5, 1) / (
          ws[1] * my_slice(expXb, 5, 2) + 
            ws[2] * my_slice(expXb, 5, 3) + 
            ws[3] * my_slice(expXb, 5, 4) + 
            ws[4] * my_slice(expXb, 5, 5)
        )
    }) %>%
    purrr::reduce(`+`) %>%
    magrittr::divide_by(nsims) %>%
    as_tibble() %>%
    dplyr::mutate(id_time = 4 + 1:dplyr::n()) %>%
    tidyr::pivot_longer(
      cols = -id_time, 
      values_to = "r",
      names_prefix = "V", 
      names_to = "id_area",
      names_transform = list(id_area = as.integer)
    ) %>%
    dplyr::inner_join(id_area) %>%
    dplyr::inner_join(id_time) %>%
    dplyr::mutate(id_region = regions)
}

R_samples <- 
  1:16 %>%
  purrr::map(r_eff_inla) %>%
  purrr::reduce(rbind)

saveRDS(R_samples, "r_mb.rds")
