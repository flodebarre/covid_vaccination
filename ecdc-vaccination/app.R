## Todo before running the app
# Load data and format the data
# source("getData.R")

library(shiny)

# Global stuff ####

# LOAD DATA

# Name of the latest data file
latestDataFile <- max(system("ls data/ecdc-vaccination_cumulated_*", intern = TRUE))

# Extract date (timestamp)
thedate <- as.Date(substr(latestDataFile, 33, 42))

# Load the data
newdat <- read.csv(latestDataFile)


# Load function to plot the figure
source("plotFig.R")

# Get list of countries in the data (will be used in drop-down lists)
ctrs <- aggregate(newdat$Country, by = list(ReportingCountry = newdat$ReportingCountry), FUN = unique)
ctrs <- ctrs[order(ctrs[, 2]), ]
countries <- as.list(ctrs[, 1])
names(countries) <- ctrs[, 2]
rm(ctrs) # Clean memory

# UI ####
ui <- fluidPage(

    h1("EU vaccination: compare countries"), 
    p(em("Disclaimer: Provided as-is. \n
The figure cannot be better than what is in the data. Some countries are not reporting age-specific data to ECDC, and cannot therefore be shown here. 
There may be issues with denominators (estimations of population size of the different age bands). Obvious issues include a vaccination rate greater than 100%; this case is denoted by an asterisk next to the corresponding age band on the figure. 
")), 
    HTML("<p><i>I got the idea of drawing vaccination age-pyramids after seeing @VictimOfMaths' US-UK <a href = 'https://twitter.com/VictimOfMaths/status/1424788095485071367?s=20'>example on Twitter</a>. I interverted the positions of the different colors to put the emphasis on unvaccinated populations. </i></p>"), 
   HTML("<p>If you are viewing this app from a computer, you can change the width of the figure by changing the width of your browser's window.</p>"),
    
    fluidRow(
      column(width = 6, 
          h5("Data"),
          HTML("<p>The data come from the European Centre for Disease Prevention and Control (ECDC). The dataset is available on the <a href = 'https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv'>vaccination webpage</a>, and can be visualized on the <a href = 'https://vaccinetracker.ecdc.europa.eu/public/extensions/COVID-19/vaccine-tracker.html'>Vaccine tracker</a> page. Please check their 'Notes on the data', 'Country Disclaimers' for a list of potential issues. </p>"),
      ), 
      column(width = 6, 
             h5("Code"),
             HTML("<p>The source code of this Shiny app is available <a href = 'https://github.com/flodebarre/covid_vaccination/tree/main/ecdc-vaccination'>on Github</a>. You are welcome to check it and to <a href = 'mailto:florence.debarre@normalesup.org?subject=ShinyApp'>let me know</a> if you find mistakes or know better ways of doing it. </p>")
             )
    ),
    
    fluidRow(
        column(12, 
               h5("Notes"), 
               p("'1 dose' can include completed vaccination if the Janssen vaccine was used, or in countries (like France) that give only one dose to some of the people who have had Covid-19 previously."), 
               p(paste0("Last updated on ", thedate, "."))
        ) 
    ),
    
    hr(),

    # Parameters (grayed)
wellPanel(
    # Countries
    fluidRow(
        column(width = 4, selectInput("c1", "Country 1:",
                                                  countries, selected = "ES"), offset = 2), 
        column(width = 4, selectInput("c2", "Country 2:",
                                      countries, selected = "FR"))
    ),
    
    fluidRow(column(width = 6, radioButtons("densOrProp", "Type of plot", c("Population sizes" = "popsize", "Proportions" = "prop"), selected = "popsize", inline = TRUE), offset = 3)),
    
    conditionalPanel(condition = "input.densOrProp == 'popsize'",
    # Graduations and scale
    fluidRow(
        column(width = 3, p("\n\nChoose the precision of the population size graduations:")),
        column(width = 2, selectInput("byRec", label = "Graduations", choices = c("  5000" = 5000, " 10000" = 10000, " 25000" = 25000, " 50000" = 50000, "100000" = 100000), selected = 10000, width = '200px')),
        column(width = 4, p("Choose whether to use the same scale for both countries, or use different ones (preferable if large population size difference):")), 
        column(width = 3, radioButtons("sameScale", "Scale", choices = c("same" = TRUE, "different" = FALSE), inline = TRUE, selected = TRUE))
    ),
    ),
    
    # Date
    fluidRow(
        column(width = 6, offset = 3, sliderInput(inputId = "week", label = "2021 Week number (click play to animate)", min = 1, max = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), value = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), step = 1, animate = animationOptions(interval = 750, playButton = "play"), width = '100%'))
    ),
), 
    
    # Plot
    fluidRow(column(width = 10, offset = 1, plotOutput("agePyramid", height = 500))),
    
    # Trick to add white space at the bottom (not sure it is really working!)
    p("  "),
    fluidRow(column(1)), 
    fluidRow(column(width = 4,
                    downloadButton('downloadPlot', 'Download Plot'), 
                    offset = 4, align = "center")),
  p("  "),
  fluidRow(column(1)), 
)


# Server ####
server <- function(input, output) {
    
      output$agePyramid <- renderPlot({
        plotFig(c1 = as.character(input$c1), c2 = as.character(input$c2), 
                week = as.character(input$week), 
                densOrProp = as.character(input$densOrProp), 
                sameScale = as.logical(input$sameScale), byRec = as.numeric(input$byRec), 
                thedate = thedate, newdat = newdat)
      }, res = 80)
      
      
      output$downloadPlot <- downloadHandler(
        # Source: https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
        filename = function(){ paste('agePyramid_', input$c1, '-', input$c2, "_w-", input$week, '.pdf', sep='') },
        content = function(file){
          pdf(width = 7, height = 5.5, file = file)
          print(plotFig(c1 = as.character(input$c1), c2 = as.character(input$c2), 
                        week = as.character(input$week), 
                        densOrProp = as.character(input$densOrProp), 
                        sameScale = as.logical(input$sameScale), byRec = as.numeric(input$byRec), 
                        thedate = thedate, newdat = newdat))
          dev.off()
        }
      )
}

# Run the application ####
shinyApp(ui = ui, server = server)
