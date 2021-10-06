suppressMessages(library(targets))
suppressMessages(library(magrittr))
suppressMessages(library(dbplyr))
suppressMessages(library(rstan))
suppressMessages(library(sf))
suppressMessages(library(sn))
suppressMessages(library(INLA))
suppressMessages(library(splines))
suppressMessages(library(splines2))
suppressMessages(library(nlme))
suppressMessages(library(lme4))
suppressMessages(library(visNetwork))
suppressMessages(library(xtable))
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
  targets::tar_target(pp_vacunados_completo, get_pp_vacunados_completo(conn)),
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
  targets::tar_target(r_martinez_besag, get_r_martinez_besag(casos0, vecinos, comunas)),
  targets::tar_target(r, get_r(
    r_systrom, 
    r_cislaghi, 
    r_jrc, 
    r_rki, 
    r_wallinga, 
    r_martinez_besag
  )),
  targets::tar_target(covariates, get_covariates(
    casos0,
    comunas,
    pasos,
    pcr,
    pp_vacunados_completo,
    vacaciones,
    cuarentenas,
    im_interno,
    im_externo,
    pp_vecinos_cuarentena
  )),
  targets::tar_target(model_df, get_model_df(r_wallinga, covariates)),
  targets::tar_target(fit, get_fit_oscar(model_df)),
  targets::tar_target(fit_nlme, get_fit_oscar_nlme(model_df)),
  targets::tar_target(fit_nlme_boxcox, get_fit_oscar_nlme_boxcox(model_df)),
  targets::tar_target(fit_nlme_boxcox_ar1, get_fit_oscar_nlme_boxcox_ar1(model_df)),
  targets::tar_target(fit_nlme_ar1, get_fit_oscar_nlme_ar1(model_df)),
  targets::tar_target(fit_nlme_ma1, get_fit_oscar_nlme_ma1(model_df)),
  targets::tar_target(fit_nlme_arma, get_fit_oscar_nlme_arma(model_df)),
  targets::tar_target(fit_gamma, get_fit_oscar_gamma(model_df)),
  targets::tar_target(fit_gamma_no_vaccine, get_fit_oscar_gamma_no_vaccine(model_df)),
  targets::tar_target(cov, get_cov(fit_nlme)),
  targets::tar_target(b, get_b(fit_nlme)),
  targets::tar_target(b_gamma, get_b(fit_gamma)),
  targets::tar_target(b_gamma_no_vaccine, get_b(fit_gamma_no_vaccine)),
  targets::tar_target(predict_gamma, get_predict_gamma(model_df, fit_gamma)),
  targets::tar_target(predict_gamma_no_vaccine, get_predict_gamma(model_df, fit_gamma_no_vaccine)),
  targets::tar_target(plot_r, get_plot_r(r_wallinga), format = "file"),
  targets::tar_target(plot_pp_vecinos_cuarentena, get_plot_pp_vecinos_cuarentena(pp_vecinos_cuarentena), format = "file"),
  targets::tar_target(plot_pcr, get_plot_pcr(pcr), format = "file"),
  targets::tar_target(plot_casos, get_plot_casos(casos), format = "file"),
  targets::tar_target(plot_r_p10_en, get_plot_r_p10_en(r), format = "file"),
  targets::tar_target(plot_r_p10_es, get_plot_r_p10_es(r), format = "file"),
  targets::tar_target(plot_r_p50_en, get_plot_r_p50_en(r), format = "file"),
  targets::tar_target(plot_r_p50_es, get_plot_r_p50_es(r), format = "file"),
  targets::tar_target(plot_r_p90_en, get_plot_r_p90_en(r), format = "file"),
  targets::tar_target(plot_r_p90_es, get_plot_r_p90_es(r), format = "file"),
  targets::tar_target(plot_r_ts_en, get_plot_r_ts_en(r, comunas), format = "file"),
  targets::tar_target(plot_r_ts_es, get_plot_r_ts_es(r, comunas), format = "file"),
  targets::tar_target(plot_r_bp_en, get_plot_r_bp_en(r, comunas), format = "file"),
  targets::tar_target(plot_r_bp_es, get_plot_r_bp_es(r, comunas), format = "file"),
  targets::tar_target(plot_r_hist_en, get_plot_r_hist_en(model_df), format = "file"),
  targets::tar_target(plot_r_hist_es, get_plot_r_hist_es(model_df), format = "file"),
  targets::tar_target(plot_r_vac_ts_en, get_plot_r_vac_ts(model_df, "en"), format = "file"),
  targets::tar_target(plot_r_vac_ts_es, get_plot_r_vac_ts(model_df, "es"), format = "file"),
  targets::tar_target(plot_fit_qqnorm, get_plot_fit_qqnorm(fit_nlme), format = "file"),
  targets::tar_target(plot_fit_acf, get_plot_fit_acf(fit_nlme), format = "file"),
  targets::tar_target(plot_fit_yh, get_plot_fit_yh(fit_nlme, model_df), format = "file"),
  targets::tar_target(plot_fit_boxcox_qqnorm, get_plot_fit_boxcox_qqnorm(fit_nlme_boxcox), format = "file"),
  targets::tar_target(plot_fit_boxcox_acf, get_plot_fit_boxcox_acf(fit_nlme_boxcox), format = "file"),
  targets::tar_target(plot_fit_boxcox_ar1_qqnorm, get_plot_fit_boxcox_ar1_qqnorm(fit_nlme_boxcox_ar1), format = "file"),
  targets::tar_target(plot_fit_boxcox_ar1_acf, get_plot_fit_boxcox_ar1_acf(fit_nlme_boxcox_ar1)),
  targets::tar_target(tbl_b, get_tbl_b(fit, "data/b.csv"), format = "file"),
  targets::tar_target(tbl_b_gamma, get_tbl_b(fit_gamma, "data/b_gamma.csv"), format = "file"),
  targets::tar_target(tbl_b_gamma_tex, get_tbl_b_tex(fit_gamma, "data/b_gamma.tex"), format = "file"),
  targets::tar_target(tbl_b_gamma_no_vaccine, get_tbl_b(fit_gamma_no_vaccine, "data/b_gamma_no_vaccine.csv"), format = "file"),
  targets::tar_target(tbl_b_gamma_no_vaccine_tex, get_tbl_b_tex(fit_gamma_no_vaccine, "data/b_gamma_no_vaccine.tex"), format = "file"),
  #AWS
  targets::tar_target(covariates_aws, get_covariates_aws(
    inmigrantes, pob_20_64, pp_vecinos_cuarentena, pcr, pp_vacunados_completo)
  ),
  targets::tar_target(model_df_aws, get_model_df(r_wallinga, covariates_aws)),
  targets::tar_target(fit_gamma_no_vaccine_aws, get_fit_oscar_gamma_no_vaccine(model_df_aws)),
  targets::tar_target(tbl_b_gamma_no_vaccine_aws, get_tbl_b(fit_gamma_no_vaccine_aws, "data/b_gamma_no_vaccine_aws.csv"), format = "file")
)
