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

# Week values
wks <- sort(unique(newdat$YearWeekISO))
# Remove the first four
wks <- wks[5:length(wks)]

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
        column(6, align = "center", offset = 3, 
               p(paste0("Last updated on ", thedate, "."))
        ) 
    ),
    
    hr(),
   
   sidebarLayout(
     
     sidebarPanel(width = 5,
       #--------------------------------------------------------------------
       # PARAMETERS
       
       # Countries
       fluidRow(
         column(width = 6, selectInput("c1", "Country 1:",
                                       countries, selected = "ES"), offset = 0), 
         column(width = 6, selectInput("c2", "Country 2:",
                                       countries, selected = "FR"))
       ),
       
       fluidRow(column(width = 12, radioButtons("densOrProp", "Type of plot", c("Population sizes" = "popsize", "Proportions" = "prop"), selected = "popsize", inline = TRUE), offset = 0, align = "center")),
       
       # Display these options only if plotting population sizes
       conditionalPanel(condition = "input.densOrProp == 'popsize'",
                        # Graduations and scale
                        HTML("<p><b>Scale</b>  Choose whether to use the same scale for both countries, or use different ones (preferable if large population size difference):</p>"),
                        fluidRow(
                          column(width = 12, align = "center", radioButtons("sameScale", NULL, choices = c("same" = TRUE, "different" = FALSE), inline = TRUE, selected = TRUE))),
                        HTML("<p><b>Graduations</b>  Choose the precision of the population size graduations:</p>"),
                        fluidRow(
                          column(width = 6, offset = 3, align = "center", selectInput("byRec", label = NULL, choices = c("  5000" = 5000, " 10000" = 10000, " 25000" = 25000, " 50000" = 50000, "100000" = 100000), selected = 50000, width = '200px')),
                        ),
       ), # End of conditional panel
       
       # Date
       fluidRow(
#         column(width = 12, offset = 0, sliderInput(inputId = "week", label = "2021 Week number (click play to animate)", min = 1, max = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), value = as.numeric(substr(max(newdat$YearWeekISO), start = 7, stop = 8)), step = 1, animate = animationOptions(interval = 750, playButton = "play"), width = '100%'))
         column(width = 12, offset = 0, sliderInput(inputId = "week", label = "Week (click play to animate)", min = 1, max = length(wks), value = length(wks), step = 1, animate = animationOptions(interval = 250, playButton = "play"), width = '100%'))
       ),
       #--------------------------------------------------------------------
       
     ), # end of sidebarPanel
     
     mainPanel(width = 7,
       
       # Plot
       fluidRow(column(width = 10, offset = 1, plotOutput("agePyramid", height = 500, width = 600), align = "center")),
       
       # Trick to add white space at the bottom (not sure it is really working!)
       p("  "),
       fluidRow(column(1)), 
       fluidRow(column(width = 4,
                       downloadButton('downloadPlot', 'Download Plot'), 
                       offset = 4, align = "center")),
       
       
     ), # End of mainPanel
   ), # end of sidebarLayout

  

  p("  "),
  fluidRow(column(1)), 

  h3("Notes"), 
  p("'1 dose' can include completed vaccination if the Janssen vaccine was used, or in countries (like France) that give only one dose to some of the people who have had Covid-19 previously."), 
  
  h3("Frequently Asked Questions"),
  h4("Why cannot I find Germany?"),
  p("Some countries do not provide age-stratified data to ECDC. Germany is one of them."),
  h4("Why cannot I find the UK?"),
  p("The UK has left the UE and is not concerned by ECDC anymore."),
  h4("Why are some age classes missing?"),
  p("Some countries have not provided demographic data (population size of the age class) on all age classes."),
  
  
  
  h3("Credits"), 
  HTML("<p>I got the idea of drawing vaccination age-pyramids after seeing Colin Angus' (@VictimOfMaths) US-UK <a href = 'https://twitter.com/VictimOfMaths/status/1424788095485071367?s=20'>example on Twitter</a>. I interverted the positions of the different colors to put the emphasis on unvaccinated populations.<br/>
       Rich FitzJohn suggested a code modification to better order the countries in the drop-down list. </p>"), 

)

#-------------------------------------------------------------------------------------------------------------------

# Server ####
server <- function(input, output) {
    
      output$agePyramid <- renderPlot({
        plotFig(c1 = as.character(input$c1), c2 = as.character(input$c2),
                week = as.character(wks[input$week]),
                densOrProp = as.character(input$densOrProp),
                sameScale = as.logical(input$sameScale), byRec = as.numeric(input$byRec),
                thedate = thedate, newdat = newdat)
      }, res = 80)
      
      
      output$downloadPlot <- downloadHandler(
        # Source: https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
        filename = function(){ paste('agePyramid_', input$c1, '-', input$c2, "_w-", input$week, '.pdf', sep='') },
        content = function(file){
          pdf(width = 7, height = 6.5, file = file)
          print(plotFig(c1 = as.character(input$c1), c2 = as.character(input$c2),
                        week = as.character(wks[input$week]),
                        densOrProp = as.character(input$densOrProp),
                        sameScale = as.logical(input$sameScale), byRec = as.numeric(input$byRec),
                        thedate = thedate, newdat = newdat))
          dev.off()
        }
      )
}

# Run the application ####
shinyApp(ui = ui, server = server)
