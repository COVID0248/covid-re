suppressMessages(library(targets))
suppressMessages(library(magrittr))
suppressMessages(library(dbplyr))
suppressMessages(library(rstan))
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("R/utils.R")

list(
  tar_target(vecinos, "data/vecinos.rds", format = "file"),
  tar_target(conn, get_conn()),
  tar_target(pob_20_64, get_pob_20_64(conn)),
  tar_target(inmigrantes, get_inmigrantes(conn)),
  tar_target(comunas, get_comunas(conn, inmigrantes, pob_20_64)),
  tar_target(pasos, get_pasos()),
  tar_target(casos, get_casos()),
  tar_target(pcr, get_pcr()),
  tar_target(vacuna1, get_vacuna1()),
  tar_target(vacuna2, get_vacuna2()),
  tar_target(cuarentenas, get_cuarentenas()),
  tar_target(vacaciones, get_vacaciones()),
  tar_target(r_systrom, get_r_systrom(casos)),
  tar_target(r_cislaghi, get_r_cislaghi(casos)),
  tar_target(r_jrc, get_r_jrc(casos)),
  tar_target(r_rki, get_r_rki(casos)),
  tar_target(r_wallinga, get_r_wallinga(casos)),
  tar_target(r, get_r(
    r_systrom, r_cislaghi, r_jrc, r_rki, r_wallinga
  )),
  targets::tar_target(df, get_df(
    r_wallinga,
    comunas,
    pasos,
    casos,
    pcr,
    vacuna1,
    vacuna2,
    cuarentenas,
    vacaciones
  )),  
  targets::tar_target(fit, get_fit(df)),
  targets::tar_target(cov, get_cov(fit)),
  targets::tar_target(b, get_b(fit))
)
