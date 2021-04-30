suppressMessages(library(targets))
suppressMessages(library(magrittr))
suppressMessages(library(dbplyr))
suppressMessages(library(rstan))
suppressMessages(library(sf))
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("R/utils.R")
source("R/targets.R")

list(
  tar_target(conn, get_conn()),
  tar_target(vecinos, get_vecinos()),
  tar_target(pob_20_64, get_pob_20_64(conn)),
  tar_target(inmigrantes, get_inmigrantes(conn)),
  tar_target(comunas, get_comunas(conn, inmigrantes, pob_20_64)),
  tar_target(poblacion, get_poblacion(comunas)),
  tar_target(pasos, get_pasos()),
  tar_target(casos, get_casos()), # bien (pero ver si hacer otra version si redondeo)
  tar_target(pcr, get_pcr(comunas)), # debiese cubrir todas las semanas (añadir ceros está bien)
  tar_target(vacunados1, get_vacunados1()), # debiese cubrir todas las semanas (añadir ceros está bien)
  tar_target(vacunados2, get_vacunados2()), # debiese cubrir todas las semanas (añadir ceros está bien)
  tar_target(im_interno, get_im_interno()),
  tar_target(im_externo, get_im_externo()),
  # tar_target(mp10, get_mp10()),
  tar_target(cuarentenas, get_cuarentenas(comunas)),
  tar_target(vacaciones, get_vacaciones()),
  tar_target(pvc, get_pvc(poblacion, vecinos, cuarentenas, pasos)), # debiese cubrir todas las semanas (añadir ceros está bien)
  tar_target(r_systrom_stanfile, "stan/systrom.stan", format = "file"),
  tar_target(r_systrom_model, get_r_systrom_model(r_systrom_stanfile)),
  tar_target(r_systrom, get_r_systrom(casos, r_systrom_model)),
  tar_target(r_cislaghi, get_r_cislaghi(casos)),
  tar_target(r_jrc, get_r_jrc(casos)),
  tar_target(r_rki, get_r_rki(casos)),
  tar_target(r_wallinga, get_r_wallinga(casos)),
  tar_target(r, get_r(r_systrom, r_cislaghi, r_jrc, r_rki, r_wallinga)),
  targets::tar_target(df, get_df(
    r_wallinga,
    comunas,
    casos,
    pasos,
    pcr,
    vacunados1,
    vacunados2,
    vacaciones,
    pvc,
    cuarentenas,
    im_interno,
    im_externo
  )),
  targets::tar_target(fit, get_fit(df)),
  targets::tar_target(cov, get_cov(fit)),
  targets::tar_target(b, get_b(fit)),
  targets::tar_target(plot_r, long_boxplot(r_wallinga, "r")),
  targets::tar_target(plot_pvc, long_boxplot(pvc, "pvc")),
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

# TODO:
# Añadir un gráfico con las series de tiempo de unas 3 comunas "interesantes".
