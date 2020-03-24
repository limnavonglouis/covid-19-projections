library(dplyr)
library(ggplot2)
library(reshape)

get_timeserie <- function(data, country_name) {
  timeserie <- data[data[, 'Country.Region'] == country_name, ][,-1]
  return(as.numeric(timeserie))
}

get_curve <- function(data_confirmed, data_deaths, data_recovered, country_name) {
  
  confirmed <- get_timeserie(data_confirmed, country_name)
  deaths <- get_timeserie(data_deaths, country_name)
  recovered <- get_timeserie(data_recovered, country_name)
  curve <- confirmed - deaths - recovered
  
  return(curve)
}

create_breakdown_data <- function(country_name, data_country_confirmed, data_country_deaths, data_country_recovered) {
  day <- seq(length(data_country_confirmed)-1)
  
  breakdown_data <- data.frame(day = day)
  breakdown_data$deaths <- get_timeserie(data_country_deaths, country_name) 
  breakdown_data$recovered <- get_timeserie(data_country_recovered, country_name)
  breakdown_data$current <- 
    get_timeserie(data_country_confirmed, country_name) - breakdown_data$deaths - breakdown_data$recovered
  
  return(breakdown_data)
}

plot_breakdown <- function(country_name, data_country_confirmed, data_country_deaths, data_country_recovered) {
  
  breakdown_data <- create_breakdown_data(country_name, data_country_confirmed, data_country_deaths, data_country_recovered)
  
  melted_breakdown_data <- melt(data = breakdown_data, id = 'day')
  plot <- ggplot(melted_breakdown_data, aes(x=day, y=value, fill = variable)) + 
    geom_area(alpha=0.6 , size=.5) + 
    labs(ylab('current'))
  
  return(plot)
}


plot_curve <- function(country_names, data_country_confirmed, data_country_deaths, data_country_recovered) {
  
  day <- seq(length(data_country_confirmed)-1)
  curve_data <- data.frame(day = day)
  
  for (country in country_names) {
    curve_data[, country] <- get_curve(data_confirmed = data_country_confirmed, data_deaths = data_country_deaths,
                                       data_recovered = data_country_recovered, country_name = country)
  }
  
  melted_data <- melt(data = curve_data, id = 'day')
  plot <- ggplot(melted_data) + geom_line(aes(x = day, y = value, color = variable)) + labs(ylab("current"))
  
  return(plot)
}