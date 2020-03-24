#setwd(dir = "/Users/louislimnavong/Desktop/research/coronatimeseries")

library(dplyr)
library(ggplot2)

source("model.R")
source('data_processing.R')

plot_main <- function(data_type, data_country_confirmed, data_country_deaths, country_name, count_start, n) {
  
  if (data_type == 'deaths') {
    data_country <- data_country_deaths
  } else {
    data_country <- data_country_confirmed
  }
  
  time_serie <- data_country[data_country$Country.Region == country_name,][,-1]
  actual_data <- data.frame(day = seq(length(data_country)-1), country_name = as.numeric(time_serie))
  new_data <- create_data_from_timeserie(count_start, data_country, time_serie, country_name)
  result <- fit_and_plot_model(new_data, country_name, n)
  
  return(result)
} 

# path <- ("/Users/louislimnavong/Documents/GitHub/limnavonglouis/COVID-19/csse_covid_19_data/csse_covid_19_time_series/")
# 
# data_confirmed <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
# data_country <- data_confirmed[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))
# 
# data_deaths <- read.csv(paste(path, "time_series_19-covid-Deaths.csv", sep = ""))
# data_country_deaths <- data_deaths[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))
# 
# data_type = 'deaths'
# country_name = 'France'
# count_start = 50 
# n = 20
# 
# result <- plot_main(data_type, data_country_confirmed, data_country_deaths, 
#                     country_name, count_start, n)
# 
# nlm1 <- result[[1]]
# plot <- result[[2]]
# 
# nlm1
# plot 
