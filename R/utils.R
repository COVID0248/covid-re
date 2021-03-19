R_from_r <- function (r, GT) {
  Tmax <- length(GT$GT)
  R <- 1/sum(GT$GT * (exp(-r * (0:(Tmax - 1)))))
}

est_re_exp <- function(ts, GT_obj, half_window_width = 3L) {
  res <- sapply(
    (half_window_width + 1):(length(ts) - half_window_width), 
    function(t) {
      idx <- (t - half_window_width):(t + half_window_width)
      data <- data.frame(Date = 1:length(ts), y = as.numeric(ts))
      m <- glm(y ~ 1 + Date, poisson, dplyr::slice(data, idx))
      r <- as.numeric(coef(m)["Date"])
      R <- R_from_r(r, GT_obj)
    }
  )
  names(res) <- 
    names(ts)[(half_window_width + 1):(length(ts) - half_window_width)]
  return(res)
}

date_to_sepi <- function(x) {
  diff <- difftime(
    strptime(x, format = "%Y-%m-%d"),
    strptime("2020-02-16", format = "%Y-%m-%d"),
    units="weeks"
  )
  return(as.integer(diff) + 8)
}

lags <- function(var, n = 10){
  var <- dplyr::enquo(var)
  indices <- seq_len(n)
  purrr::map(indices, ~dplyr::quo(dplyr::lag(!!var, !!.x)) ) %>% 
    purrr::set_names(sprintf("%s_lag%01d", rlang::quo_text(var), indices))
}

mystepwise <- function(yvar0, xvar0, preserve, max_pval, random, data) {
  xdel = " "
  while (xdel != "") {
    fmla <- paste0(yvar0, " ~ ", paste(xvar0, collapse = "+"), " + ", random)
    xdel <-
      lme4::lmer(as.formula(fmla), data = data) %>%
      drop1(test = "Chisq") %>%
      broom.mixed::tidy(effects = "fixed") %>%
      dplyr::filter(!(term %in% preserve)) %>%
      na.omit() %>%
      dplyr::filter(Pr.Chi. == max(Pr.Chi.)) %$%
      ifelse(Pr.Chi. > max_pval, term, "")
    xvar0 <- xvar0[xvar0 != xdel]
  }
  fmla <- paste0(yvar0, " ~ ", paste(xvar0, collapse = "+"), " + ", random)
  do.call(lme4::lmer, list(formula = as.formula(fmla), data = data))
}

long_boxplot <- function(data, varname) {
  data %>%
    dplyr::mutate(codigo_semana = as.factor(codigo_semana)) %>%
    ggplot2::ggplot(aes(codigo_semana, .data[[varname]])) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_classic()
}
