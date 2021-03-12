import::from(magrittr, "%>%")

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

date_to_sepi <- function(x) {
  diff <- difftime(
    strptime(x, format = "%Y-%m-%d"),
    strptime("2020-02-16", format = "%Y-%m-%d"),
    units="weeks"
  )
  return(as.integer(diff) + 8)
}

get_df_paso_a_paso <- function() {
  data <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
    paste0("/output/producto74/paso_a_paso_std.csv") %>%
    read.csv() %>%
    dplyr::rename_with(tolower) %>%
    dplyr::group_by(fecha, codigo_comuna) %>%
    dplyr::summarise(paso = min(paso), .groups = "drop") %>%
    dplyr::mutate(semana = date_to_sepi(fecha)) %>%
    dplyr::filter(codigo_comuna != 0) %>%
    dplyr::group_by(semana, codigo_comuna) %>%
    dplyr::summarise(paso = min(paso), .groups = "drop") %>%
    dplyr::mutate(paso = factor(paso)) %>%
    dplyr::arrange(semana, codigo_comuna) %>%
    tibble::as_tibble()
}
# saveRDS(get_df_paso_a_paso(), "data/df_paso_a_paso.rds")
