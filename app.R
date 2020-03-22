
library(shiny)

library(dplyr)

source('plot_truncated.R')
source('plot_model.R')
source('plot_model_compared.R')

## SCRIPT ## 

path <- ("/Users/louislimnavong/Documents/GitHub/limnavonglouis/COVID-19/csse_covid_19_data/csse_covid_19_time_series/")
data <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
data_country <- data[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

## VARIABLES ## 

country_choices <- data_country[,'Country.Region']

## UI ##

ui <- fluidPage(
    
    titlePanel("Coronavirus Daily Cases Evolution"),
    
    tabsetPanel(
        
        tabPanel("Actual",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("country_list", "Select the list of countries",choices = country_choices, 
                                     selected = c('France', 'China'), multiple = TRUE, selectize = TRUE),
                         sliderInput("count_start", "Select the count start", 
                                     value = 0, min = 0, max = 600)
                     ),
                     mainPanel(
                         plotOutput('plot_compared_countries')
                     )
                 ),
                 fluidRow(
                     dataTableOutput('table_country')
                         )
        ),
        
        tabPanel("Projection",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("model_country_list", "Select the list of countries",choices = country_choices, 
                                     selected = c('France', 'China'), multiple = TRUE, selectize = TRUE),
                         sliderInput("model_count_start", "Select the count start", 
                                     value = 50, min = 0, max = 600)
                     ),
                     mainPanel(
                         plotOutput('plot_model_compared')
                     )
                 )
        ),
        
        
        tabPanel("Method",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("country_name", "Select the country", choices = country_choices, 
                                    selected = 'France', multiple = FALSE, selectize = TRUE),
                         sliderInput("country_count_start", "Select the count start", 
                                     value = 50, min = 0, max = 600),
                         numericInput("country_data", "Select the number of data point", value = 20)
                        ),
                     mainPanel(
                         plotOutput('plot_main_model')
                        )
                 )
         )
   ) # end of tabsetPanel 
    
    
) # end of fluidPage

## SERVER ##

server <- function(input, output) {
    
    output$table_country <- renderDataTable(plot_compared_countries(country_list = input$country_list,
                                                                data_country = data_country,
                                                                start = input$count_start)[[2]])
    
    output$plot_compared_countries <- renderPlot(plot_compared_countries(country_list = input$country_list,
                                                                         data_country = data_country,
                                                                         start = input$count_start)[[1]])
    
    output$plot_main_model <- renderPlot(plot_main(data_country = data_country,
                                                   country_name = input$country_name,
                                                   count_start = input$country_count_start,
                                                   n = input$country_data)[[2]])
    output$plot_model_compared <- renderPlot(plot_model_compared(country_list = input$model_country_list,
                                                                 data_country = data_country,
                                                                 start = input$model_count_start))
}


shinyApp(ui, server)