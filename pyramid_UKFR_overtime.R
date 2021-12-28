##
## Age pyramid and vaccination
## Comparison France and UK
##
## Plot inspired by 
## https://twitter.com/VictimOfMaths/status/1424788101382160388?s=20
## The code behind it is in pyramid_victimOfMaths.R (modified)
## and was downloaded from https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDVaxxAgeUKUSA.R
##
## FD August 2021
##

# Load data

# England data
# Comes from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
# I downloaded Excel sheets for specific dates (see `data/nhs/`)
# and manually reorganized them into a single file (`combined`)

rm(list = ls()) # I am doing this knowingly

dat.England.all <- read.csv("data/nhs/combined.csv")
head(dat.England.all)
tail(dat.England.all)

# France data from https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/
# vacsi-a-fra-2021-08-10-19h05.csv
URL <- "https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd"
download.file(URL, "data/vacsi-a-fra.csv")
dat.France.all <- read.csv("data/vacsi-a-fra.csv", sep = ";")

head(dat.France.all)
tail(dat.France.all)

# Dico for age classes in France
agcl <- sort(unique(dat.France.all$clage_vacsi))
ages <- c("All", "0_4", "5_9", "10_11", "12_17", "18_24", "25_29", "30_39", "40_49", "50_59", "60_64", "65_69", "70_74", "75_79", "80_120")
names(ages) <- agcl
ages
# Rewrite age classes for France
dat.France.all$age <- ages[as.character(dat.France.all$clage_vacsi)]

# Remove age class "all"
dat.France.all <- dat.France.all[dat.France.all$age != "All", ]

# Data population France (2021)
# https://www.insee.fr/fr/statistiques/2381472
popFrance <- read.csv("data/popFrance_insee.csv")
# Turn into dictionnary
popFr <- popFrance$pop
names(popFr) <- popFrance$ageClass

# Add pop information to France data
dat.France.all$pop <- popFr[dat.France.all$age]

# Select the columns that we want
dat.France.all <- dat.France.all[, c("jour", "age", "pop", "n_cum_complet", "n_cum_dose1", "n_cum_rappel")]
names(dat.France.all) <- c("date", "ageClass", "pop", "cumComplet", "cumDose1", "cumRappel")

dat.England.all <- dat.England.all[, c("date", "age", "pop", "dose2", "dose1", "dose3")]
names(dat.England.all) <- c("date", "ageClass", "pop", "cumComplet", "cumDose1", "cumRappel")

# Add country information
dat.England.all$country <- "Angleterre"
dat.France.all$country <- "France"

# All dates in the England dataset
dates <- sort(unique(dat.England.all$date))
dates
# Subselect lines for these dates in the France dataset
dat.France.all <- dat.France.all[is.element(dat.France.all$date, dates), ]

dat.England.all[1,]
dat.France.all[1,]
# Bind the datasets into a single one
dat.all <- rbind(dat.England.all, dat.France.all)


# Get min and max ages of the age classes
ag <- matrix(unlist(strsplit(as.character(dat.all$ageClass), split = "_")), byrow = TRUE, ncol = 2)
dat.all$agemin <- as.numeric(ag[, 1])
dat.all$agemax <- as.numeric(ag[, 2])

# Make numerical values numerical again
for(col in c("pop", "cumComplet", "cumDose1", "agemin", "agemax")){
  dat.all[, col] <- as.numeric(dat.all[, col])
}

# Re-define last age class (very few people so old!)
dat.all[dat.all$agemax == 120, c("agemax")] <- 105

# Compute width of the age classes
dat.all$width <- dat.all$agemax - dat.all$agemin + 1

# Values by year of age
dat.all$pop.byY <- dat.all$pop / dat.all$width
dat.all$cumComplet.byY <- dat.all$cumComplet / dat.all$width
dat.all$cumDose1.byY   <- dat.all$cumDose1 / dat.all$width
dat.all$cumRappel.byY   <- dat.all$cumRappel / dat.all$width


# Colors
colPop <- gray(0.925) # Unvaccinated
colRappelEN <- "#006833"  #"#922300"
colCompletEN <- "#2FB16F"#"#ED4009"
col1DEN <- "#ABEDCB" #"#FF8B66"
colRappelFR <- "#043D5F"
colCompletFR <- "#3177A1"
col1DFR <- "#ABD2E9"

factorEN <- 1 # Means that England on the right-hand side




#for(thedate in dates){
  thedate <- dates[length(dates)]
  
  # Select values at this date
  dat <- dat.all[dat.all$date == thedate, ]
  
  
  
  #------------------------------------------------------------
  
  # Plot 
  
  
  for(version in 1:2){
    # Two versions of the figure, 
    # 1 focus on vaccinated completely
    # 2 focus on unvaccinated
    
    # file name of the output figure  
    fname <- paste0("pics/pyramid_UK-FR_", thedate, "_", version, ".pdf")
    
    pdf(fname, width = 7.5, height = 7)
    
    par(xpd = FALSE, family = "sans", mgp = c(1.5, 0.0, 0), tck = -0.02)
    par(mar = c(6, 2.5, 4, 2.5))
    
    # Initialize plot
    plot(c(-10^6, 10^6), c(0, max(dat$agemax)), type = "n", 
         axes = FALSE, xlab = "", ylab = "", 
         xaxs = "i")
    
    # Write Credits
    par(family = "mono")
    mtext(side = 1, line = 4.75, text = paste0("@flodebarre, d'après @VictimOfMaths ; ", thedate, ",  
  Données NHS : https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
  France : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/, 
           Population INSEE 2021 https://www.insee.fr/fr/statistiques/2381472
  Code : https://github.com/flodebarre/covid_vaccination/blob/main/pyramid_UKFR_overtime.R"), adj = 0, cex = 0.55, col = gray(0.4))
    par(family = "sans")
    
    for(ctr in c("Angleterre", "France")){
      # For each country / side of the plot
    
      if(ctr == "Angleterre"){
        factor <- factorEN
        colComplet <- colCompletEN
        col1D <- col1DEN
        colRappel <- colRappelEN
      }else{
        factor <- -1 * factorEN
        colComplet <- colCompletFR
        col1D <- col1DFR
        colRappel <- colRappelFR
      }
      
      tmp <- dat[dat$country == ctr, ]
      
      for(ag in unique(tmp$ageClass)){
        # For each age class, 
        # Subset of the data
        tmp <- dat[dat$country == ctr & dat$ageClass == ag, ]
        
        # Check if issues with pop size estimations
        popSizePb <- FALSE
        
        # If there is an issue: 
        if(is.na(tmp$cumDose1) | tmp$cumDose1 > tmp$pop){
          popSizePb <- TRUE # Update the indicator
          
          # Change pop size to the number of first doses
          tmp$pop <- tmp$cumDose1
          # Recompute by year version
          tmp$pop.byY <- tmp$pop / tmp$width
        }
        
        # Plot Total population
        rect(xleft = factor * tmp$pop.byY, ybottom = tmp$agemin, 
             xright = 0, ytop = tmp$agemax + 1, 
             col = colPop, border = gray(0, 0))
        
        
        if(version == 1){
          # Vaccinated, 1 dose
          rect(xleft = factor * (tmp$cumDose1.byY), ybottom = tmp$agemin, 
               xright = 0, ytop = tmp$agemax + 1, 
               col = col1D, border = gray(0, 0))
          
          # Vaccinated, complete
          rect(xleft = factor * tmp$cumComplet.byY, ybottom = tmp$agemin, 
               xright = 0, ytop = tmp$agemax + 1, 
               col = colComplet, border = gray(0, 0))
          
          # Vaccinated, booster
          rect(xleft = factor * tmp$cumRappel.byY, ybottom = tmp$agemin, 
               xright = 0, ytop = tmp$agemax + 1, 
               col = colRappel, border = gray(0, 0))
        
          }else{
          ## Full vaccination in the end
          # Vaccinated, 1 dose
          rect(xleft = factor * (tmp$pop.byY - tmp$cumDose1.byY), ybottom = tmp$agemin, 
               xright = factor * tmp$pop.byY, ytop = tmp$agemax + 1, 
               col = col1D, border = gray(0, 0))
          
          # Vaccinated, complete
          rect(xleft = factor * (tmp$pop.byY - tmp$cumComplet.byY), ybottom = tmp$agemin, 
               xright = factor * tmp$pop.byY, ytop = tmp$agemax + 1, 
               col = colComplet, border = gray(0, 0))
          
          # Vaccinated, booster
          rect(xleft = factor * (tmp$pop.byY - tmp$cumRappel.byY), ybottom = tmp$agemin, 
               xright = factor * tmp$pop.byY, ytop = tmp$agemax + 1, 
               col = colRappel, border = gray(0, 0))
        
        }
        
        # Graduations for age class
        lines(c(factor*10^6, 0), rep(tmp$agemin, 2), col = "white", lwd = 1.5)
        # Age values
        par(xpd = TRUE)
        text(x = factor*10^6, y = tmp$agemin, labels = tmp$agemin, col = gray(0), adj = c(-factor, 0.25), cex = 0.9)
        par(xpd = FALSE)
        
        # If pb, add star to notify it at mid 
        if(popSizePb){
          text(x = factor * tmp$pop.byY, y = (tmp$agemin + tmp$agemax + 1)/2, labels = "*", cex = 2, font = 2, adj = -factor)
        }
        
        
      }
      # Add country legend
      par(xpd = TRUE)
      if(factor == -1){adjj <- 0}else{adjj <- 1}
      text(x = factor * 10^6, y = 110, labels = ctr, adj = c(adjj, 0), cex = 1.3, font = 2)
      par(xpd = FALSE)
      
    }
    
    # Old version of the axis
    #xx <- c(0, 250000, 500000, 750000, 1000000)
    #axis(1, at = c(-xx, xx), labels = format(abs(c(-xx, xx)), trim = TRUE), cex.axis = 0.8)
    
    
    par(xpd = FALSE)
    # Graduations
    wfine <- 0.3
    # Horizontal by year
    for(i in 0:110){
      abline(h = i, col = "white", lwd = wfine)
    }
    # Vertical by 100000
    for(i in seq(0, 10^6, by = 100000)){
      abline(v = i, col = "white", lwd = wfine)
      abline(v = -i, col = "white", lwd = wfine)
    }
    # Vertical separation of the two countries
    #abline(v = 0, col = gray(0), lwd = 1.5)
    lines(x = c(0, 0), y = c(0, max(dat$agemax) + 1), col = 1, lwd = 1.5, lend = "butt")
    
    # Horizontal axis
    axis(1, at = seq(-10^6, 10^6, by = 100000), labels = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), cex.axis = 0.8, pos = -1)
    axis(1, at = c(-500000, 500000, 0, 10^6, -10^6), labels = c("500 000", "500 000", "0", "1Mio", "1Mio"), lwd.ticks = -1, lwd = -1)
    cexl <- 0.9 # Text size of legend of axes
    mtext("Population par année d'âge", side = 1, line = 1, cex = cexl)
    mtext("Une étoile * signifie un potentiel problème d'estimation de la taille de la classe d'âge dans les données démographiques utilisées", side = 1, line = 1.75, cex = 0.6*cexl)
    
    # Legend ages
    par(xpd = TRUE)
    
    yl <- 100   # y position of "Age" 
    text("Age", x = 10^6, y = yl, cex = cexl, adj = c(0.5, 1))
    text("Age", x = -10^6, y = yl, cex = cexl, adj = c(0.5, 1))
    
    
    # Plot legend (manually centered)
    par(family = "mono")
    legend(x = 0, y = 105, pch = 15, col = c(colRappelFR, colCompletFR, col1DFR, colPop, colRappelEN, colCompletEN, col1DEN, colPop), ncol = 2, legend = c("   Dose de rappel", "Vaccination complète", " Vaccination 1 dose", "   Non vacciné·e", "", "", "", ""), xjust = 0.29, yjust = 0, box.lwd = -1, text.font = 1, pt.cex = 2, cex = 0.8)
    
    # Add title 
    par(family = "sans")
    
    mtext(thedate, side = 3, line = 2.75, cex = 1.2, font = 2)
    dev.off()
    
    system(paste0("open ", fname))
  } # end version
#} # end thedate

  
  
# ## Evaluate this in console to convert pdfs to png
# # for p in pics/pyramid_UK-FR_*.pdf; do; pdftoppm "$p" "${p%.*}" -png; done
# system("./pyramid_UK-FR_gifScript.sh")
# 
# # Build gif
# system("convert -delay 150 pics/pyramid_UK-FR_*_2*.png pics/pyramid_UK-FR_2.gif")
# system("convert -delay 150 pics/pyramid_UK-FR_*_1*.png pics/pyramid_UK-FR_1.gif")

# Final prop
datEN <- dat[dat$country == "Angleterre",]
datFR <- dat[dat$country == "France",]
c(sum(datEN$cumDose1, na.rm = TRUE)/sum(datEN$pop, na.rm = TRUE), 
  sum(datEN$cumComplet, na.rm = TRUE)/sum(datEN$pop, na.rm = TRUE), 
  sum(datFR$cumDose1, na.rm = TRUE)/sum(datFR$pop, na.rm = TRUE), 
  sum(datFR$cumComplet, na.rm = TRUE)/sum(datFR$pop, na.rm = TRUE))
