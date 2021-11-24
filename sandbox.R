library(dplyr)
library(ggplot2)
library(magrittr)
source("R/utils.R")

# targets::tar_target(r_systrom_stanfile, "stan/systrom.stan", format = "file"),
# targets::tar_target(r_systrom_model, get_r_systrom_model(r_systrom_stanfile)),
# targets::tar_target(r_systrom, get_r_systrom(casos, r_systrom_model)),
# targets::tar_target(r_cislaghi, get_r_cislaghi(casos)),
# targets::tar_target(r_jrc, get_r_jrc(casos)),
# targets::tar_target(r_rki, get_r_rki(casos)),
# targets::tar_target(r_wallinga, get_r_wallinga(casos)),

tar_make("r_cislaghi_nac")
r0 <- tar_read("r_cislaghi_nac")
