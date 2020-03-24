#setwd(dir = "/Users/louislimnavong/Desktop/research/coronatimeseries")

library(dplyr)
library(ggplot2)
library(reshape)

source('data_processing.R')

plot_compared_countries <- function(data_type, country_list, data_country_confirmed, data_country_deaths, start) {
  
  if (data_type == 'deaths') {
    data_country <- data_country_deaths
  } else {
    data_country <- data_country_confirmed
  }
  
  new_data <- create_data_compared_countries(country_list, data_country, start)
  melted_data <- melt(new_data, id = 'day')
  plot <- ggplot(melted_data, aes(x = day, y = value, colour = variable)) + 
    geom_line() 
  
  result <- list(plot, new_data)
  return(result)
}

# path <- ("/Users/louislimnavong/Documents/GitHub/limnavonglouis/COVID-19/csse_covid_19_data/csse_covid_19_time_series/")
# data <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
# data_confirmed <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
# data_deaths <- read.csv(paste(path, "time_series_19-covid-Deaths.csv", sep = ""))
# data_country_confirmed <- data_confirmed[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))
# data_country_deaths <- data_deaths[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))
# 
# data_type <- 'deaths'
# country_list <- c('Spain', 'France', 'Italy', 'US')
# start <- 0
# plot_compared_countries(data_type, country_list, data_country_confirmed, data_country_deaths, start)[[1]]

