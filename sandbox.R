# # Datos brutos
# data0 <-
#   "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/" %>%
#   paste0("master/output/producto1/Covid-19_std.csv") %>%
#   read.csv() %>%
#   dplyr::select(
#     i = Codigo.comuna,
#     t = Fecha,
#     n = Poblacion,
#     y = Casos.confirmados
#   ) %>%
#   dplyr::mutate(t = lubridate::as_date(t)) %>%
#   dplyr::filter(!is.na(i)) %>%
#   dplyr::arrange(i, t)
# 
# 
# 
# interpolated_data <- 
#   data0$i %>%
#   purrr::map(function(.x) {
#     
#   }
#     ~ data0 %>%
#       filter(i == .x) %>%
#       
#   ) %>%
#   
#   data0 %>%
#   dplyr::filter(i == 1101) %$%
#   approxfun(x = t, y = y)
# 
# 
# 
# # Rango de fechas
# dates <- 
#   data0 %>%
#   dplyr::group_by(i) %>%
#   dplyr::group_map(
#     ~ data.frame(t = as.Date(min(.x$t):max(.x$t), "1970-01-01"))  
#   )
#   
# 
# 
# # Datos interpolados
# data1 <-
#   data0 %>%
#   dplyr::group_by(i) %>%
#   dplyr::full_join(dates)
