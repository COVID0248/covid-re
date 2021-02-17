import::from(magrittr, "%>%")

est_re_Systrom <- function(covid_cases, state_selected, GAMMA = 1/7, R_T_MAX = 12) {
  r_t_range <- seq(0, R_T_MAX, length = 100 * R_T_MAX + 1)
  
  # Compute new cases and smooth them
  smooth_new_cases_1 <- function(cases){
    cases %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(new_cases = c(cases[1], diff(cases))) %>%
      dplyr::mutate(
        new_cases_smooth = round(
          smoother::smth.gaussian(new_cases, window = 7, alpha=7/4, tails = FALSE)
      )) %>%
      dplyr::select(state, date, new_cases, new_cases_smooth)
  }


  # Start using data first when smoothed value is above 25
  smooth_new_cases_25 <- function(x){
    start_index <- which(x$new_cases_smooth > 25)[1]
    length_vector <-dim(x)[1]
    return(x[start_index:length_vector,])
  }

  compute_likelihood <- function(cases){
    likelihood <- cases %>%
      dplyr::filter(new_cases_smooth > 0) %>%
      dplyr::mutate(
        r_t = list(r_t_range),
        lambda = map(lag(new_cases_smooth, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
        likelihood_r_t = map2(new_cases_smooth, lambda, dpois, log = TRUE)
      ) %>%
      dplyr::slice(-1) %>%
      dplyr::select(-lambda) %>%
      tidyr::unnest(c(likelihood_r_t, r_t))
  }
  
  compute_posterior <- function(likelihood){
    likelihood %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(r_t) %>%
      dplyr::mutate(posterior = exp(
        zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
      )) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>%
      # HACK: NaNs in the posterior create issues later on. So we remove them.
      dplyr::mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-likelihood_r_t)
  }
  
  # Estimate R_t and a 95% highest-density interval around it
  estimate_rt <- function(posteriors){
    posteriors %>%
      dplyr::group_by(state, date) %>%
      dplyr::summarize(
        r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
        r_t_most_likely = r_t_range[which.max(posterior)]
      ) %>%
      dplyr::mutate(
        r_t_lo = purrr::map_dbl(r_t_simulated, ~ hdi(.x)[1]),
        r_t_hi = purrr::map_dbl(r_t_simulated, ~ hdi(.x)[2])
      ) %>%
      dplyr::select(-r_t_simulated)
  }
  
  estimates <- covid_cases %>%
    dplyr::filter(state == state_selected) %>%
    smooth_new_cases_1() %>%
    smooth_new_cases_25()  %>%
    compute_likelihood() %>%
    compute_posterior() %>%
    estimate_rt()
}