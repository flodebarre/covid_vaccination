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

library(shiny)

# Global

# Load data
newdat <- read.csv("data/ecdc-vaccination_cumulated_2021-08-24.csv")

# Define colors 
colPop <- gray(0.8) # Unvaccinated
colComplet1 <- "#9B2500" # 2 doses, left
col1D1 <- "#FF6939" # 1 dose, left
colComplet2 <- "#044063" # 2 doses, right
col1D2 <- "#4F92BA" # 1 dose, right

countries <- sort(unique(newdat$ReportingCountry))
aggregate(newdat$Country, by = list(ReportingCountry = newdat$ReportingCountry), FUN = unique)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ECDC vaccination data"),
    
    p("blabalbal"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        selectInput("c1", "Country 1", 
                    choices = list())

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = colComplet1, border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
