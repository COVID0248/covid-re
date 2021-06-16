suppressMessages(library(targets))
suppressMessages(library(magrittr))
suppressMessages(library(dbplyr))
suppressMessages(library(rstan))
suppressMessages(library(sf))
suppressMessages(library(sn))
suppressMessages(library(INLA))
suppressMessages(library(splines2))
inla.setOption(pardiso.license = "~/sys/licenses/pardiso.lic")
set.seed(1)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("R/utils.R")
source("R/targets.R")

list(
  targets::tar_target(conn, get_conn()),
  targets::tar_target(vecinos, get_vecinos()),
  targets::tar_target(pob_20_64, get_pob_20_64(conn)),
  targets::tar_target(inmigrantes, get_inmigrantes(conn)),
  targets::tar_target(comunas, get_comunas(conn, inmigrantes, pob_20_64)),
  targets::tar_target(poblacion, get_poblacion(comunas)),
  targets::tar_target(pasos, get_pasos()),
  targets::tar_target(casos, get_casos()), # version censurada
  targets::tar_target(casos0, get_casos0()), # versión no-censurada
  targets::tar_target(pcr, get_pcr(comunas)), # debiese cubrir todas las semanas (añadir ceros está bien)
  targets::tar_target(vacunados1, get_vacunados1()), # debiese cubrir todas las semanas (añadir ceros está bien)
  targets::tar_target(vacunados2, get_vacunados2()), # debiese cubrir todas las semanas (añadir ceros está bien)
  targets::tar_target(im_interno, get_im_interno()),
  targets::tar_target(im_externo, get_im_externo()),
  targets::tar_target(mp10, get_mp10()),
  targets::tar_target(cuarentenas, get_cuarentenas(comunas, pasos)),
  targets::tar_target(vacaciones, get_vacaciones()),
  targets::tar_target(pp_vecinos_cuarentena, get_pp_vecinos_cuarentena(
    poblacion, vecinos, cuarentenas
  )),
  targets::tar_target(r_systrom_stanfile, "stan/systrom.stan", format = "file"),
  targets::tar_target(r_systrom_model, get_r_systrom_model(r_systrom_stanfile)),
  targets::tar_target(r_systrom, get_r_systrom(casos, r_systrom_model)),
  targets::tar_target(r_cislaghi, get_r_cislaghi(casos)),
  targets::tar_target(r_jrc, get_r_jrc(casos)),
  targets::tar_target(r_rki, get_r_rki(casos)),
  targets::tar_target(r_wallinga, get_r_wallinga(casos)),
  targets::tar_target(r_martinez_leroux, get_r_martinez_leroux(casos0, vecinos, comunas)),
  targets::tar_target(r_martinez_besag, get_r_martinez_besag(casos0, vecinos, comunas)),
  targets::tar_target(r_martinez_bym, get_r_martinez_bym(casos0, vecinos, comunas)),
  targets::tar_target(r, get_r(
    r_systrom, 
    r_cislaghi, 
    r_jrc, 
    r_rki, 
    r_wallinga, 
    r_martinez_leroux,
    r_martinez_besag,
    r_martinez_bym
  )),
  targets::tar_target(covariates, get_covariates(
    casos0,
    comunas,
    pasos,
    pcr,
    vacunados1,
    vacunados2,
    vacaciones,
    cuarentenas,
    im_interno,
    im_externo,
    pp_vecinos_cuarentena
  )),
  targets::tar_target(model_df, get_model_df(r_wallinga, covariates)),
  targets::tar_target(fit, get_fit_oscar(model_df)),
  targets::tar_target(cov, get_cov(fit)),
  targets::tar_target(b, get_b(fit)),
  targets::tar_target(plot_r, long_boxplot(r_wallinga, "r")),
  targets::tar_target(plot_pvc, long_boxplot(pp_vecinos_cuarentena, "pp_vecinos_cuarentena")),
  targets::tar_target(plot_pcr, long_boxplot(pcr, "pcr")),
  targets::tar_target(plot_vacunados1, long_boxplot(vacunados1, "vacunados1")),
  targets::tar_target(plot_vacunados2, long_boxplot(vacunados2, "vacunados2")),
  targets::tar_target(plot_casos, long_boxplot(casos, "casos_nuevos")),
  targets::tar_target(plot_r_p10, get_plot_r_p10(r)),
  targets::tar_target(plot_r_p50, get_plot_r_p50(r)),
  targets::tar_target(plot_r_p90, get_plot_r_p90(r)),
  targets::tar_target(plot_r_ts, get_plot_r_ts(r, comunas)),
  targets::tar_target(plot_r_bp, get_plot_r_bp(r, comunas))
)
