#setwd(dir = "/Users/louislimnavong/Desktop/research/coronatimeseries")

library(dplyr)
library(ggplot2)

source("model.R")
source('data_processing.R')

plot_model_compared <- function(data_type, country_list, data_country_confirmed, data_country_deaths, start) {
  
  if (data_type == 'deaths') {
    data_country <- data_country_deaths
  } else {
    data_country <- data_country_confirmed
  }
  
  new_data <- create_data_compared_countries(country_list, data_country, start)
  fitted_data <- create_fitted_data(new_data, country_list)
  melted_data <- melt(new_data, id = 'day')
  fitted_melted_data <- melt(fitted_data, id = 'day')
  data_temp <- merge(melted_data, fitted_melted_data, by = c("day", "variable"))
  
  plot <- ggplot(data_temp) + 
    geom_line(aes(x = day, y = value.y, color = variable), linetype="dashed") +
    geom_point(aes(x = day, y = value.x, colour = variable))
  
  return(plot)
  
}

path <- ("/Users/louislimnavong/Documents/GitHub/limnavonglouis/COVID-19/csse_covid_19_data/csse_covid_19_time_series/")
data <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
data_confirmed <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
data_deaths <- read.csv(paste(path, "time_series_19-covid-Deaths.csv", sep = ""))
data_country_confirmed <- data_confirmed[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))
data_country_deaths <- data_deaths[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

data_type = 'deaths'
country_list = c('Germany', 'France', 'Spain', 'Italy', 'China')
start = 50
plot_model_compared(data_type, country_list, data_country_confirmed, data_country_deaths, start)

