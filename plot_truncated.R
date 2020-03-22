#setwd(dir = "/Users/louislimnavong/Desktop/research/coronatimeseries")

library(dplyr)
library(ggplot2)
library(reshape)

source('data_processing.R')

plot_compared_countries <- function(country_list, data_country, start) {
  
  new_data <- create_data_compared_countries(country_list, data_country, start)
  melted_data <- melt(new_data, id = 'day')
  plot <- ggplot(melted_data, aes(x = day, y = value, colour = variable)) + 
    geom_line() 
  
  result <- list(plot, new_data)
  return(result)
}

path <- ("/Users/louislimnavong/Documents/GitHub/limnavonglouis/COVID-19/csse_covid_19_data/csse_covid_19_time_series/")
data <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
data_country <- data[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

country_list <- c('Spain', 'France', 'Italy', 'US')
start <- 500
plot_compared_countries(country_list, data_country, start)[[1]]

