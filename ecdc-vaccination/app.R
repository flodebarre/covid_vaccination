#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## Todo
# Load data and format the data
# source("getData.R")
thedate <- "2021-08-24" # TimeStamp of the data

library(shiny)

# Global

# Load data
newdat <- read.csv(paste0("data/ecdc-vaccination_cumulated_", thedate, ".csv"))

# Load function to plot the figure
# (uses newdat as global variable)
source("plotFig.R")

# Get list of countries in the data
ctrs <- aggregate(newdat$Country, by = list(ReportingCountry = newdat$ReportingCountry), FUN = unique)
countries <- as.list(ctrs[, 1])
names(countries) <- ctrs[, 2]
rm(ctrs) # Clean memory

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ECDC vaccination data"),
    
    p(em("Disclaimer: Provided as-is. \n
The figure cannot be better than what is in the data.
")), 
    h4("Links"),
    p("The data come from the European Centre for Disease Prevention and Control (ECDC)"),
    a(href = "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", "[ECDC dataset]"), 
    a(href = "https://www.ecdc.europa.eu/en/publications-data/data-covid-19-vaccination-eu-eea", "[ECDC vaccination webpage]"), 
    a(href = "https://vaccinetracker.ecdc.europa.eu/public/extensions/COVID-19/vaccine-tracker.html", "[ECDC vaccine tracker]"),
    p("Code of this Shiny app:"),
    a(href = "")

    # Countries
    fluidRow(
        column(width = 4, selectInput("c1", "Country 1:",
                                                  countries, selected = "ES"), offset = 2), 
        column(width = 4, selectInput("c2", "Country 2:",
                                      countries, selected = "FR"))
    ),
    
    # Graduations
    fluidRow(
        column(width = 4, selectInput("byRec", "Population Graduations", c("  5000" = 5000, " 10000" = 10000, " 25000" = 25000, " 50000" = 50000, "100000" = 100000), selected = 10000, width = '200px'), offset = 4)
    ),
    
    # Scale
    fluidRow(
        column(width = 4, radioButtons("sameScale", "Scale", choices = c("same" = TRUE, "different" = FALSE), inline = TRUE), offset = 4)
    ),

    # Date
    fluidRow(
        column(width = 6, offset = 3, sliderInput(inputId = "week", label = "2021 Week number (click play to animate)", min = 1, max = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), value = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), step = 1, animate = animationOptions(interval = 750, playButton = "play"), width = '100%'))
    ),
    
    # Plot
    fluidRow(column(width = 8, offset = 2, plotOutput("agePyramid"))),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$agePyramid <- renderPlot({
        plotFig(as.character(input$c1), as.character(input$c2), as.character(input$week), as.logical(input$sameScale), as.numeric(input$byRec), thedate, newdat)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
