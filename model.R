library(minpack.lm)

function_model <- function(x, K, r, P) {
  return(K/(1 + ((K-P)/P)*exp(-r*x)))
}

fit_model <- function(new_data, country_name, n) {
  x <- new_data$day
  y <- new_data[, country_name]
  x_fit <- x[1:n]
  y_fit <- y[1:n]
  
  P = y[1]
  nlm1 <- nlsLM(y_fit ~  K/(1 + ((K-P)/P)*exp(-r*x_fit)), start=c(K=80000, r=0.3), lower = c(K=0, r=-Inf))
  
  return(nlm1)
}

plot_model <- function(new_data, nlm, country_name, n) {
  x <- new_data$day
  y <- new_data[,country_name]
  P = y[1]
  y_fitted <- function_model(x, coef(nlm)['K'],  coef(nlm)['r'], P)
  plot <- ggplot() + 
    geom_line(aes(x = x, y = y_fitted)) + 
    geom_line(aes(x = n, y = y, color = 'red')) + 
    geom_point(aes(x = x, y = y, color = 'red'))
  
  return(plot)
}

fit_and_plot_model <- function(new_data, country_name, n) {
  
  nlm1 <- fit_model(new_data, country_name, n)
  plot <- plot_model(new_data, nlm1, country_name, n)

  return(list(nlm1, plot))
}