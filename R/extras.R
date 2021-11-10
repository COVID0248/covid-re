source("R/utils.R")
library(dplyr)

rnacional <-
  "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master" %>%
  paste0("/output/producto54/r.nacional.csv") %>%
  read.csv() %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(codigo_semana = date_to_sepi(fecha)) %>%
  dplyr::select(codigo_semana, r = r.estimado)

# weeks <- 
rnacional %>%
  dplyr::arrange(r) %>%
  dplyr::slice(floor((10, 50, 90) * dplyr::n() / 100))

r %>%
  dplyr::filter(codigo_semana %in% (T - 4 * (0:3))) %>%
  ggplot2::ggplot(aes(x = r)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_grid(cols = ggplot2::vars(codigo_semana), scales = "free") +
  ggplot2::theme_classic()# +
# ggplot2::labs(x = "epidemiological week", y = "effective R")
# 
ggsave("images/plot_r_hist_en.png", plot, width = 7, height = 7)
return("images/plot_r_hist_en.png")
}