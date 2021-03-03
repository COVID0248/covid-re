
# Stepwise regression para modelos lineales mixtos
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