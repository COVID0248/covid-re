import::from(magrittr, "%>%")

R.from.r <- function (r, GT) {
  Tmax = length(GT$GT)
  R = 1/sum(GT$GT * (exp(-r * (0:(Tmax - 1)))))
}

est_re_exp <- function(ts, GT_obj, half_window_width=3L) {
  # Loop over all time points where the sliding window fits in
  res <- sapply((half_window_width+1):(length(ts)-half_window_width), function(t) {
    # Define the sliding window
    idx <- (t-half_window_width):(t+half_window_width)
    data <- data.frame(Date=1:length(ts), y=as.numeric(ts))
    # Fit a Poisson GLM
    m <- glm( y ~ 1 + Date, family=poisson, data= data  %>% dplyr::slice(idx))
    # Extract the growth rate 
    r <- as.numeric(coef(m)["Date"])
    
    # Equation 2.9 from Wallinga & Lipsitch
    R <- R.from.r(r, GT_obj)
    return(R)
  })
  names(res) <- names(ts)[(half_window_width+1):(length(ts)-half_window_width)]
  return(res)
}