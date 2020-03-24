library(shiny)
library(shinythemes)

library(dplyr)
library(rvest)

source('plot_curve.R')
source('plot_model.R')
source('plot_model_compared.R')
source('plot_truncated.R')

## SCRIPT ## 

path <- ("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/")

data_confirmed <- read.csv(paste(path, "time_series_19-covid-Confirmed.csv", sep = ""))
data_country_confirmed <- data_confirmed[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

data_deaths <- read.csv(paste(path, "time_series_19-covid-Deaths.csv", sep = ""))
data_country_deaths <- data_deaths[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

data_recovered <- read.csv(paste(path, "time_series_19-covid-Recovered.csv", sep = ""))
data_country_recovered <- data_recovered[,-c(1,3,4)] %>% group_by(Country.Region) %>% summarise_all(funs(sum))

## VARIABLES ## 

country_choices <- data_country_confirmed[,'Country.Region']

## UI ##

ui <- fluidPage(theme = shinytheme("flatly"),
    
    titlePanel("Covid-19 Daily Cases Evolution"),
    
    selectInput("count_selection", "Select the data to plot.", choices = c('deaths', 'confirmed cases'), 
                selected = 'confirmed cases', multiple = FALSE, selectize = TRUE), 
    
    tabsetPanel(
        
        tabPanel("Actual",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("country_list", "Select the list of countries",choices = country_choices, 
                                     selected = c('France', 'China'), multiple = TRUE, selectize = TRUE),
                         sliderInput("count_start", "Select the count start", 
                                     value = 0, min = 0, max = 600),
                         p("The plot shows the daily evolution of the number of confirmed cases or deaths."),
                         p("The count start can be adpated depending on the compared countries."),
                         strong("count start:"),
                         p("Number of cases at which the plot starts."),
                         p("The data used in the plot can be found in the table below.")
                     ),
                     mainPanel(
                         plotOutput('plot_compared_countries')
                     )
                 ),
                 fluidRow(
                     column(1),
                     column(10,dataTableOutput('table_country'))
                         )
        ),
        
        tabPanel("Projection",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("model_country_list", "Select the list of countries",choices = country_choices, 
                                     selected = c('France', 'China'), multiple = TRUE, selectize = TRUE),
                         sliderInput("model_count_start", "Select the count start", 
                                     value = 50, min = 0, max = 600),
                         p("The evolution of the daily cases can be modeled using the data points from the first days of the epidemic."),
                         p("This enables us to project the future trajectory of the number of cases."),
                         p("More details can be found in the Methodology tab.")
                     ),
                     mainPanel(
                         plotOutput('plot_model_compared')
                     )
                 )
        ),
        
        
        tabPanel("Methodology",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("country_name", "Select the country", choices = country_choices, 
                                    selected = 'France', multiple = FALSE, selectize = TRUE),
                         sliderInput("country_count_start", "Select the count start", 
                                     value = 50, min = 0, max = 600),
                         numericInput("country_data", "Select the number of data point", value = 20),
                         p("Changing the number of data points used to make the estimation shows the day-to-day changes of the projection.")
                        ),
                     mainPanel(
                         plotOutput('plot_main_model')
                        )
                 ),
                 fluidRow(
                     br(),
                     column(1),
                     column(7, 
                            strong("Formulation of the model", style = "font-size:205%"),
                            br(), br(),
                            p("The evolution of the number of cases is modeled with a population growth model."),
                            withMathJax(),
                            helpText('$$\\frac{K}{1+\\left ( \\frac{K-P_{0}}{P_{0}} \\right )e^{-rt}}$$'),
                            p("\\(K\\) is the carrying capacity and is the theoterical maximum for the number of cases."),
                            p("\\(r\\) is a parameter proportional to the growth rate."),
                            p("\\(P_{0}\\) is the initial value which is related to the count start."),
                            p("\\(t\\) is the number of days after the start of the epidemic.")
                     ),
                     column(3, br(), br(), br(),
                            p("For this example:", style = "font-size:150%"),
                            h4("\\(K\\) ="), 
                            h4(textOutput('K'), align = "center"),
                            h4("\\(r\\) ="), 
                            h4(textOutput('r'), align = "center")
                    )
                 )
         ),
        
         tabPanel("Current",
                  
                  sidebarLayout(
                      sidebarPanel(
                          selectInput("curve_country_list", "Select the list of countries", choices = country_choices,
                                      selected = c('France', 'China'), multiple = TRUE, selectize = TRUE),
                          p("The current number of cases is an important indicator to follow for the capacity of health institutions such as hospitals."),
                          p("Current cases = "),
                          p("Confirmed cases - Recovered cases - Deaths"),
                          br(), br(),
                          selectInput("curve_country_name", "Select the country", choices = country_choices, 
                                      selected = 'China', multiple = FALSE, selectize = TRUE),
                          p("The breakdown shows the repartition for a given country.")
                      ),
                      mainPanel(
                          plotOutput('plot_curve_compared'),
                          plotOutput('plot_breakdown')
                          
                      )
                  )
         )
   ) # end of tabsetPanel 
    
    
) # end of fluidPage

## SERVER ##

server <- function(input, output) {
    
    output$table_country <- renderDataTable(plot_compared_countries(country_list = input$country_list,
                                                                    data_type = input$count_selection, 
                                                                    data_country_confirmed = data_country_confirmed,
                                                                    data_country_deaths = data_country_deaths,
                                                                    start = input$count_start)[[2]])
    
    output$plot_compared_countries <- renderPlot(plot_compared_countries(country_list = input$country_list,
                                                                         data_type = input$count_selection,
                                                                         data_country_confirmed = data_country_confirmed,
                                                                         data_country_deaths = data_country_deaths,
                                                                         start = input$count_start)[[1]])
    
    output$plot_main_model <- renderPlot(plot_main(data_country_confirmed = data_country_confirmed,
                                                   data_country_deaths = data_country_deaths,
                                                   data_type = input$count_selection,
                                                   country_name = input$country_name,
                                                   count_start = input$country_count_start,
                                                   n = input$country_data)[[2]])
    
    output$plot_model_compared <- renderPlot(plot_model_compared(country_list = input$model_country_list,
                                                                 data_country_confirmed = data_country_confirmed,
                                                                 data_country_deaths = data_country_deaths,
                                                                 data_type = input$count_selection,
                                                                 start = input$model_count_start))
    output$K <- renderText(coef(plot_main(data_country_confirmed = data_country_confirmed,
                                     data_country_deaths = data_country_deaths,
                                     data_type = input$count_selection,
                                     country_name = input$country_name,
                                     count_start = input$country_count_start,
                                     n = input$country_data)[[1]])['K'])
    output$r <- renderText(coef(plot_main(data_country_confirmed = data_country_confirmed,
                                          data_country_deaths = data_country_deaths,
                                          data_type = input$count_selection,
                                          country_name = input$country_name,
                                          count_start = input$country_count_start,
                                          n = input$country_data)[[1]])['r'])
    
    output$plot_curve_compared <- renderPlot(plot_curve(country_names = input$curve_country_list, 
                                                        data_country_confirmed = data_country_confirmed, 
                                                        data_country_deaths = data_country_deaths, 
                                                        data_country_recovered = data_country_recovered))
    output$plot_breakdown <- renderPlot(plot_breakdown(country_name = input$curve_country_name, 
                                                       data_country_confirmed = data_country_confirmed, 
                                                       data_country_deaths = data_country_deaths, 
                                                       data_country_recovered = data_country_recovered))
}


shinyApp(ui, server)