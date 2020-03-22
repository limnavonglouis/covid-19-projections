#setwd(dir = "/Users/louislimnavong/Desktop/research/coronatimeseries")

source("utils.R")

create_data_from_timeserie <- function(count_start, data_country, time_serie, country_name) {
  
  day <- seq(length(data_country)-1)
  shifted_timeserie <- clean_timeserie(time_serie, count_start)
  shifted_timeserie <- c(shifted_timeserie, rep(NA, length(day) - length(shifted_timeserie)))
  new_data <- data.frame(day = day)
  new_data[, country_name] <- shifted_timeserie
  
  return(new_data)
}

create_data_compared_countries <- function(country_names, data_country, start) {
  
  day <- seq(length(data_country)-1)
  
  time_series_list <- list()
  for (i in seq_along(country_names)) {
    time_serie <- data_country[data_country$Country.Region == country_names[i],][,-1]
    time_serie <- clean_timeserie(time_serie, start)
    time_serie <- c(time_serie, rep(NA, length(day) - length(time_serie)))
    time_series_list[[i]] <- time_serie
  }
  
  new_data <- data.frame(day = day)
  for (i in seq_along(country_names)) {
    new_data[country_names[i]] <- as.numeric(time_series_list[[i]])
  }

  return(new_data)
}

create_fitted_data <- function(new_data, country_list) {
  
  n = length(new_data$day) - 1
  fitted_data <- data.frame(day = new_data$day)
  for (country in country_list) {
    nlm1 <- fit_model(new_data, country, n)
    P <- new_data[, country][1]
    x <- new_data$day
    fitted_data[,country] <- function_model(x, coef(nlm1)['K'],  coef(nlm1)['r'], P)
  }
  
  return(fitted_data)
}

