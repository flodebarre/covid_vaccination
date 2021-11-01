library(shiny)

#### Initializations ####

# Load data

## Couvertures calculées
out <- read.csv("data/calc_vaccDep_alldeps.csv")

## Departement information
departements <- read.csv("data/departement2020.csv")
# Sort departement information
depsTable <- departements[is.element(departements$dep, sort(unique(out$dep))), ]
# List of departement names for input
depsList <- as.list(depsTable$dep)
names(depsList) <- paste(depsTable$dep, depsTable$libelle, sep = " - ")

lastDate <- max(out$date)

# Plotting parameters
trp <- 0.8
colEPCI <- adjustcolor(col = "#1f78b4", alpha.f = trp)
colDep <- adjustcolor(col = "#F1B200", alpha.f = trp)
colSPF <- adjustcolor(col = "#FF581F", alpha.f = trp)


colLine <- gray(0.9) # Color of the graduation lines
lwdCurve <- 2.5 # Width of the curves
thetype <- "o" # Type of curve
thecex <- 0.4 # Size of the points


#***************************************************************

#### UI ####

ui <- fluidPage(

    # Application title
    titlePanel("Couvertures vaccinales en France"),

    HTML("<p>Notes explicatives en bas de page. Données de vaccination jusqu'au 24 octobre 2021, mise à jour du code le 1er novembre 2021. <br/>
         Une question, une suggestion, une correction ? <a href = 'mailto:florence.debarre@normalesup.org?subject=ShinyApp_CouvVaccinale'>Envoyez-moi un email</a>.<p>"), 
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            # Input type of injection
            radioButtons("input_inj", "Type d'injection", 
                         choiceValues = list("1", "complet"), 
                         choiceNames = list("Premières injections", "Vaccination complète (hors 3e doses)"),
                         selected = "1"),
            
            # Input age class
            radioButtons("input_agcl", "Classe d'âge", 
                         choiceValues = list("Tous âges", "00-39", "40-64", "65-74", "75+"), 
                         choiceNames = list("Tous âges", "0-39 ans", "40-64 ans", "65-74 ans", "75 ans et plus"), 
                         selected = "Tous âges"), 
            
            
            # Input departement
            selectInput("input_dep", label = "Département", 
                        choices = depsList, 
                        selected = "93"
                        )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(plotOutput("cumuPlot")),
          fluidRow(plotOutput("weeklyPlot"))
  
           
        )
    ),
    
    h2("Sources et caractéristiques des jeux de données"), 
    h3("SPF, denom. INSEE 2020"),
    HTML("<p>Les données de Santé Publique France (SPF) sont disponibles sur le site data.gouv.fr (<a href = 'https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/'>lien</a>), et proviennent de l'exploitation par SPF du système d’information Vaccin Covid (VAC-SI). Le département indiqué correspond au lieu de vaccination, et non au lieu de résidence. Le dénominateur utilisé pour le calcul des taux de vaccination est l'estimation de taille de population 2020 de l'INSEE.</p>
         <p>Les limites de ce jeu de données sont les suivantes : 
         <ol><li>Les personnes ayant été vaccinées hors de leur département de résidence ne sont pas comptées dans leur département de résidence, mais dans le département dans lequel elles ont été vaccinées. </li>
         <li>Les données de vaccination peuvent inclure les vaccinations de personnes ne résidant pas en France, et donc non comptées dans les estimations démographiques.</li>
         <li>La démographie de la France a changé depuis 2020, en particulier pour les tranches d'âges les plus élevées (vieillissement de la population), donnant lieu à une potentielle surestimation des taux de vaccination quand les valeurs de 2020 sont utilisées. </li></ol></p>"),
    h3("Ameli, denom. INSEE 2020"), 
    HTML("<p>Ce jeu de données est fourni par l'Assurance Maladie, sur le site datavaccin-covid.ameli.fr (<a href = 'https://datavaccin-covid.ameli.fr/explore/dataset/donnees-vaccination-par-tranche-dage-type-de-vaccin-et-departement/information/'>lien</a>). Il s'agit de données par département. Les données du système d’information Vaccin Covid (VAC-SI) sont appariées au système national des données de santé (SNDS), ce qui permet d'identifier le département de résidence des patients. Les dénominateurs sont, comme pour le jeu de données SPF, les estimations démographiques 2020 de l'INSEE.</p>
         <p>La limite principale de ce jeu de données est l'estimation de la taille de la population de chaque département (point 3. des limites SPF, plus haut). </p>"),
    h3("Ameli, assurés sociaux"),
    HTML("<p>Deux jeux de données fournis par l'Assurance Maladie sont combinés ici : les données de vaccination par EPCI (intercommunalités ; <a href = 'https://datavaccin-covid.ameli.fr/explore/dataset/donnees-de-vaccination-par-epci/'>lien</a>) et les données de vaccination par commune pour Paris, Lyon, Marseille (<a href = 'https://datavaccin-covid.ameli.fr/explore/dataset/donnees-de-vaccination-par-commune/'>lien</a>). Comme le jeu de données précédent, les données proviennent de Vaccin Covid, sont appariées au SNDS pour identifier l'EPCI ou la commune de résidence. Contrairement aux jeux de données précédents cependant, seuls les patients référencées par l'Assurance Maladie sont pris en compte dans les effectifs. Il s'agit de l'ensemble des bénéficiaires de l'Assurance Maladie obligatoire (tous régimes) :
    <ul>
<li>ayant bénéficié d'au moins une prestation dans l'année ;</li>
<li>et/ou ayant séjourné au moins une fois dans un établissement de santé public ou privé dans l'année.</li></ul></p>
         <p>Ce jeu de données a deux avantages : <ol>
         <li>Le lieu indiqué est le lieu de résidence de la personne vaccinée -- comme pour l'autre jeu Ameli</li>
         <li>Dans le calcul des taux de vaccination, le numérateur et le dénominateur correspondent à la même population dont la taille est connue -- avantage spécifique à ce jeu de données</li>
         </ol></p>"),
    
    h2("Calculs"), 
    HTML("<p>Les trois jeux de données utilisent des subdivisions différentes pour les classes d'âges. Pour pouvoir comparer les jeux de données entre eux, les classes d'âges sont harmonisées, et sont donc moins détaillées à la fin qu'elles ne le sont dans chacun des jeux de données. </br>
         Pour la classe d'âge 00-39, les résultats d'Ameli, denom. INSEE 2020 ne sont pas affichées, parce que les données 00-12 ne sont pas fournies. (Peut-être serait-il possible de les reconstituer à partir des données 'Tous âges' mais je n'ai pas essayé). </p>
         <p>Pour les données par EPCI, un EPCI à cheval sur plusieurs départements est compté dans chacun de ces départements (à l'exception des EPCI de Paris, Lyon, Marseille ou les données par commune sont disponibles et permettent de distinguer les différents départements). </p>"),
    h2("Code"), 
    HTML("<p><ul><li>Le code pour générer cette application Shiny et tracer les courbes est disponible <a href = 'https://github.com/flodebarre/covid_vaccination/blob/main/couverture_vaccinale/app.R'>ici</a>.</li>
         <li>Le code pour calculer les couvertures vaccinales par département et classe d'âge est disponible <a href = 'https://github.com/flodebarre/covid_vaccination/blob/main/couverture_vaccinale.Rmd'>ici</a> (section '# Comparisons, departements').</li></ul></p>"),
    fluidRow(HTML("&nbsp;"))
    

)

#***************************************************************


#### Server ####

server <- function(input, output) {
  
  inj <- reactive(input$input_inj)
  dep <- reactive(input$input_dep)
  agcl <- reactive(input$input_agcl)
  
  # Select data
  mres1 <- reactive(out[out$inj == inj() & out$dep == dep() & out$agcl == agcl(), ])

#  output$tb <- renderTable({as.data.frame(mend())})
  
  tit1 <- reactive(ifelse(input$input_inj == "1", "Premières injections", "Vaccinations complètes"))
  tit2 <- reactive(ifelse(input$input_agcl == "Tous âges", "", "ans"))
  
  depname <- reactive(departements[departements$dep == input$input_dep, "libelle"])
  
  observeEvent(input$input_agcl, {
    agg <- agcl()
    if(agg == "Tous âges") agg <- "tousages"
    if(agg == "75+") agg <- "75-etplus"
    
  })
  
  
  
  plotCumu <- reactive({
    mm <- mres1()
    
    mend <- mm[mm$date == lastDate, ]
    
    mm$date <- as.Date(mm$date)

    par(las = 1, mgp = c(2.25, 0.5, 0), tck = -0.015, xpd = TRUE, 
        mar = rep(4, 4))
    
    # Initialize plot
    plot(mm$date, mm$txEPCI, col = colEPCI, ylim = c(0, 1), 
         xaxs = "i", yaxs = "i", 
         xlab = "Date", ylab = "Proportion estimée de la population", 
         main = paste(tit1(), "cumulées, ", agcl(), tit2(), "\n", dep(), "-", depname()), 
         type = "n", axes = FALSE)
    
    # Add graduations    
    par(xpd = FALSE)
    for(i in seq(0, 1, by = 0.1)) abline(h = i, col = gray(0.9), lwd = 1)
    for(i in seq(0.05, 0.95, by = 0.1)) abline(h = i, col = colLine, lwd = 0.5)
    par(xpd = TRUE)
    
    
    # Add legend
    par(family = "mono")
    legend("topleft",
           col = c(colSPF, colDep, colEPCI), lty = c(1, 1, 1),
           legend = c(paste0("SPF,   denom. INSEE 2020, couverture finale : ", round(100*mend$txSPF, 1), "%"),
                      paste0("Ameli, denom. INSEE 2020, couverture finale : ", round(100*mend$txRes, 1), "%"),
                      paste0("Ameli, assurés sociaux,   couverture finale : ", round(100*mend$txEPCI, 1), "%")), lwd = lwdCurve, bty = "o", cex = 0.9, box.col = "white", box.lwd = -1)
    
    par(family = "sans")
    
    # Add axes
    axis(2, at = seq(0, 1, by = 0.1))
    axis(4, at = seq(0, 1, by = 0.1))
    axis.Date(1, at = seq(as.Date("2021-01-01"), max(mm$date), by="months"))
    
    # Add points
    points(mm$date, mm$txEPCI, col = colEPCI, type = thetype, cex = thecex, lwd = lwdCurve)
    if(!is.element(agcl(), c("00-39"))){
      points(mm$date, mm$txRes, col = colDep, 
             type = thetype, cex = thecex, lwd = lwdCurve)
    }
    points(mm$date, mm$txSPF, col = colSPF, 
           type = thetype, cex = thecex, lwd = lwdCurve)
    
  })

  
  
  plotWeekly <- reactive({
    mm <- mres1()
    
    mend <- mm[mm$date == lastDate, ]
    
    mm$date <- as.Date(mm$date)
    
    par(las = 1, mgp = c(2.25, 0.5, 0), tck = -0.015, xpd = TRUE, 
        mar = rep(4, 4))
    
    # Initialize plot
    plot(mm$date, mm$txEPCIw, col = colEPCI, ylim = c(0, 0.1), type = "n", 
         xaxs = "i", yaxs = "i", 
         xlab = "Date", ylab = "Proportion estimée de la population", 
         cex = thecex, 
         main = paste(tit1(), "de la semaine, ", agcl(), "\n Dep", dep(), "-", depname()), 
         axes = FALSE)
    
    # Add graduations
    par(xpd = FALSE)
    for(i in seq(0, 0.1, by = 0.01)) abline(h = i, col = colLine)
    par(xpd = TRUE)
    
    # Add legend
    par(family = "mono")
    legend("topleft",
           col = c(colSPF, colDep, colEPCI), lty = c(1, 1, 1),
           legend = c(paste0("SPF,   denom. INSEE 2020"),
                      paste0("Ameli, denom. INSEE 2020"),
                      paste0("Ameli, assurés sociaux")), lwd = lwdCurve, bty = "o", cex = 0.9, box.col = "white", box.lwd = -1)
    par(family = "sans")
    
    # Add axes
    axis.Date(1, at = seq(as.Date("2021-01-01"), max(mm$date), by="months"))
    axis(2, at = seq(0, 0.1, by = 0.01))
    axis(4, at = seq(0, 0.1, by = 0.01))
    
    # Add points
    points(mm$date, mm$txEPCIw, col = colEPCI, type = thetype, cex = thecex, lwd = lwdCurve)
    if(!is.element(agcl(), c("00-39"))){
      points(mm$date, mm$txResw, col = colDep, type = thetype, cex = thecex, lwd = lwdCurve)
    }
    points(mm$date, mm$txSPFw, col = colSPF, type = thetype, cex = thecex, lwd = lwdCurve)
    
  })
  
  
  
  #************************************************************
  # OUTPUTS
  
  output$cumuPlot <- renderPlot({
    plotCumu()
  })

  output$weeklyPlot <- renderPlot({
    plotWeekly()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
