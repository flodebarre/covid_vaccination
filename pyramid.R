##
## Age pyramid and vaccination
## Comparison France and UK
##
## Plot inspired by 
## https://twitter.com/VictimOfMaths/status/1424788101382160388?s=20
## The code behind it is in pyramid_victimOfMaths.R (modified)
## and was downloaded from https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDVaxxAgeUKUSA.R
##
## FD 2021-08-11
##

# Load data

# England data from https://coronavirus.data.gov.uk/details/download
# vaccinationsAgeDemographics, England
dat.England.all <- read.csv("data/nation_E92000001_2021-08-10.csv")
head(dat.England.all)

# Quality check of population values
plot(dat.England.all$cumPeopleVaccinatedCompleteByVaccinationDate/dat.England.all$VaccineRegisterPopulationByVaccinationDate - dat.England.all$cumVaccinationCompleteCoverageByVaccinationDatePercentage/100, col = "red", ylim = c(-0.1, 0.1))

# France data from https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/
# vacsi-a-fra-2021-08-10-19h05.csv
dat.France.all <- read.csv("data/vacsi-a-fra-2021-08-10-19h05.csv", sep = ";")
head(dat.France.all)

# Alternative dataset for England
# This is exported from @VictimofMaths's code, comes from NHS data
# https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/08/COVID-19-weekly-announced-vaccinations-05-August-2021.xlsx
dat.NHS <- read.csv("data/England_2021-08-05.csv")

# Dico for age classes in France
agcl <- sort(unique(dat.France.all$clage_vacsi))
ages <- c("All", "0_4", "5_9", "10_11", "12_17", "18_24", "25_29", "30_39", "40_49", "50_59", "60_64", "65_69", "70_74", "75_79", "80_120")
names(ages) <- agcl
ages

# Dico for age classes in England
agesNHS <- c("18_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_120", "0_17")
names(agesNHS) <- dat.NHS$age

# Get populations 
# England data: present in "VaccineRegisterPopulationByVaccinationDate"
dat.England.all$pop <- dat.England.all$VaccineRegisterPopulationByVaccinationDate

# Extract max date in England dataset
maxDateEngland <- max(dat.England.all$date)

# France data: get it via comparison coverage and numbers
# cov = 100*n/ntot
# ntot = 100*n/cov
dat.France.all$pop1 <- dat.France.all$n_cum_dose1 * 100/dat.France.all$couv_dose1
dat.France.all$pop2 <- dat.France.all$n_cum_complet * 100/dat.France.all$couv_complet

# Extract max date in France dataset
maxDateFrance <- max(dat.France.all$jour)

# Check that it does stabilize
plot(dat.France.all$pop1)
points(dat.France.all$pop2, col = 2)

# Compare dates (they are the same)
maxDateEngland
maxDateFrance
# Choose latest date
thedate <- "2021-08-09"

#-- Compare NHS and gov.uk --
dat.gov <- dat.England.all[dat.England.all$date == "2021-08-05", ]
unique(dat.gov$age)
dat.gov$agemin <- substr(dat.gov$age, 1, 2)
unique(dat.NHS$age)
par(las = 1)
plot(dat.gov$agemin, dat.gov$VaccineRegisterPopulationByVaccinationDate, ylim = c(0, max(dat.gov$VaccineRegisterPopulationByVaccinationDate)), 
     xlab  = "age min of the age class", 
     ylab = "number of people", pch = 15)

points(dat.gov$agemin, dat.gov$cumPeopleVaccinatedCompleteByVaccinationDate)

points(dat.NHS$agemin, dat.NHS$dose2, col = 2)
points(dat.NHS$agemin, dat.NHS$pop, col = 2, pch = 15)

legend("topright", col = c(1, 2, 1, 2), pch = c(15, 15, 1, 1), legend = c("gov.uk, total pop", "nhs, total pop", "gov.uk", "cum vaxx", "nhs, cum vaxx"))

# After discussion with Meaghan Kall and Colin Angus, 
# I will rather use NHS data
#-----------------------------------------

# Select values at this date
dat.England <- dat.NHS
dat.France <- dat.France.all[dat.France.all$jour == thedate & dat.France.all$clage_vacsi != 0, ] # remove "all" age class

# Select the columns that we need and rename them
dat.England <- dat.England[, c("age", "pop", "dose2", "dose1", "agemin", "agemax")]
names(dat.England) <- c("ageClass", "pop", "cumComplet", "cumDose1", "agemin", "agemax")

# Reformulate age classes
dat.England$ageClass <- agesNHS[dat.England$ageClass]

# Get min and max ages of the age classes
ag <- matrix(unlist(strsplit(as.character(dat.England$ageClass), split = "_")), byrow = TRUE, ncol = 2)
dat.England$agemin <- as.numeric(ag[, 1])
dat.England$agemax <- as.numeric(ag[, 2])

# Add country information
dat.England$country <- "Angleterre"

# France: rewrite age class
dat.France$ageClass <- ages[as.character(dat.France$clage_vacsi)]
names(dat.France)

# France: Get population
dat.France$pop <- round((dat.France$pop1 + dat.France$pop2)/2)
dat.France[dat.France$pop == Inf, "pop"] <- NA

# Select the columns that we want
dat.France <- dat.France[, c("clage_vacsi", "pop", "n_cum_complet", "n_cum_dose1")]
names(dat.France) <- c("ageClass", "pop", "cumComplet", "cumDose1")

# Convert the age classes to homogenize the naming system
dat.France$ageClass <- ages[as.character(dat.France$ageClass)]

# Add country information
dat.France$country <- "France"

# France: Get min and max ages of the age classes
ag <- matrix(unlist(strsplit(as.character(dat.France$ageClass), split = "_")), byrow = TRUE, ncol = 2)
dat.France$agemin <- as.numeric(ag[, 1])
dat.France$agemax <- as.numeric(ag[, 2])

harmonize <- FALSE # Whether to harmonize age classes or not

if(harmonize){
  # Harmonize age classes
  sort(unique(dat.France$ageClass))
  sort(unique(dat.England$ageClass))
  
  # For France, we need: 
  # 0_17
  #
  # For England, we need:
  # 30_39, 40_49, 50_59, 80_120
  
  # Get the corresponding lines
  i3039 <- which(is.element(dat.England$ageClass, c("30_34", "35_39")))
  i4049 <- which(is.element(dat.England$ageClass, c("40_44", "45_49")))
  i5059 <- which(is.element(dat.England$ageClass, c("50_54", "55_59")))
  i80120 <- which(is.element(dat.England$ageClass, c("80_84", "85_89", "90_120")))
  
  
  # Compute numbers for the combined age classes
  dat.England <- rbind(dat.England, 
                       c("30_39", sum(as.numeric(dat.England[i3039, "pop"])), sum(as.numeric(dat.England[i3039, "cumComplet"])), sum(as.numeric(dat.England[i3039, "cumDose1"])), "Angleterre", 30, 39), 
                       c("40_49", sum(as.numeric(dat.England[i4049, "pop"])), sum(as.numeric(dat.England[i4049, "cumComplet"])), sum(as.numeric(dat.England[i4049, "cumDose1"])), "Angleterre", 40, 49), 
                       c("50_59", sum(as.numeric(dat.England[i5059, "pop"])), sum(as.numeric(dat.England[i5059, "cumComplet"])), sum(as.numeric(dat.England[i5059, "cumDose1"])), "Angleterre", 50, 59), 
                       c("80_120", sum(as.numeric(dat.England[i80120, "pop"])), sum(as.numeric(dat.England[i80120, "cumComplet"])), sum(as.numeric(dat.England[i80120, "cumDose1"])), "Angleterre", 80, 120)) 
  
  # Get the corresponding lines, France
  i018 <- which(is.element(dat.France$ageClass, c("0_4", "5_9", "10_11", "12_17")))
  
  dat.France <- rbind(dat.France, 
                      c("0_17", sum(as.numeric(dat.France[i018, "pop"]), na.rm = TRUE), sum(as.numeric(dat.France[i018, "cumComplet"])), sum(as.numeric(dat.France[i018, "cumDose1"])), "France", 0, 17))
  
  # Hard code population below 18, as could not be retrieved from the data
  dat.France[dat.France$ageClass == "0_17", "pop"] <- 14540168            
  
  # Intersection of the age classes between the datasets
  agcls <- intersect(dat.England$ageClass, dat.France$ageClass)
  sort(agcls)
  
  # Combine the datasets
  dat <- rbind(dat.England[is.element(dat.England$ageClass, agcls), ], 
               dat.France[is.element(dat.France$ageClass, agcls), ])
}else{
  # France: group 0-11
  # Get the corresponding lines
  i011 <- which(is.element(dat.France$ageClass, c("0_4", "5_9", "10_11")))
  
  # Hard coded population
  pop017 <- 14540168
  pop011 <- pop017 - sum(as.numeric(dat.France[is.element(dat.France$ageClass, c("10_11", "12_17")), "pop"]))
  dat.France <- rbind(dat.France, 
                      c("0_11", pop011, sum(as.numeric(dat.France[i011, "cumComplet"])), sum(as.numeric(dat.France[i011, "cumDose1"])), "France", 0, 11))
  
  # Remove the lines
  dat.France <- dat.France[-i011, ]
  
  # Join the datasets
  dat <- rbind(dat.England, dat.France)
}

# Make numerical values numerical again
for(col in c("pop", "cumComplet", "cumDose1", "agemin", "agemax")){
  dat[, col] <- as.numeric(dat[, col])
}

# Re-define last age class (very few people so old!)
dat[dat$agemax == 120, c("agemax")] <- 105
dat[dat$agemax == 100, c("agemax")] <- 105

# Compute width of the age classes
dat$width <- dat$agemax - dat$agemin + 1

# Values by year of age
dat$pop.byY <- dat$pop / dat$width
dat$cumComplet.byY <- dat$cumComplet / dat$width
dat$cumDose1.byY   <- dat$cumDose1 / dat$width


#------------------------------------------------------------

# Plot 

# Colors
colPop <- gray(0.8)
colCompletEN <- "#9B2500"
col1DEN <- "#FF6939"
colCompletFR <- "#044063"
col1DFR <- "#4F92BA"

factorEN <- 1

for(version in 1:2){
  # Two versions of the figure, 
  # 1 focus on vaccinated completely
  # 2 focus on unvaccinated
  
  # file name of the output figure  
  fname <- paste0("pics/pyramid_UK-FR_", version, "_", 1*harmonize, ".pdf")
  
  pdf(fname, width = 7.5, height = 7)
  
  par(xpd = FALSE, family = "sans", mgp = c(2, 0.15, 0), tck = -0.02)
  par(mar = c(6, 2.5, 4, 2.5))
  
  # Initialize plot
  plot(c(-10^6, 10^6), c(0, max(dat$agemax)), type = "n", 
       axes = FALSE, xlab = "", ylab = "", 
       xaxs = "i")
  
  # Write Credits
  par(family = "mono")
  mtext(side = 1, line = 4.5, text = paste0("@flodebarre, d'après @VictimOfMaths, données du ", thedate, " (France) et 2021-08-05 (England),  
Données UK : https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
France : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/
Code : https://github.com/flodebarre/covid_vaccination/blob/main/pyramid.R"), adj = 0, cex = 0.55, col = gray(0.5))
  par(family = "sans")
  
  for(ctr in c("Angleterre", "France")){
    # For each country / side of the plot
  
    if(ctr == "Angleterre"){
      factor <- factorEN
      colComplet <- colCompletEN
      col1D <- col1DEN
    }else{
      factor <- -1 * factorEN
      colComplet <- colCompletFR
      col1D <- col1DFR
    }
    
    tmp <- dat[dat$country == ctr, ]
    
    for(ag in unique(tmp$ageClass)){
      # For each age class, 
      # Subset of the data
      tmp <- dat[dat$country == ctr & dat$ageClass == ag, ]
      
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
        
      }
      
      # Graduations for age class
      lines(c(factor*10^6, 0), rep(tmp$agemin, 2), col = "white", lwd = 1.5)
      # Age values
      par(xpd = TRUE)
      text(x = factor*10^6, y = tmp$agemin, labels = tmp$agemin, col = gray(0), adj = c(0.5, 0.25), cex = 0.9)
      par(xpd = FALSE)
      
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
  
  # Horizontal axis
  axis(1, at = seq(-10^6, 10^6, by = 100000), labels = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), cex.axis = 0.8, pos = -1)
  axis(1, at = c(-500000, 500000, 0, 10^6, -10^6), labels = c("500 000", "500 000", "0", "1Mio", "1Mio"), lwd.ticks = -1, lwd = -1)
  cexl <- 0.9 # Text size of legend of axes
  mtext("Population par année d'âge", side = 1, line = 1.25, cex = cexl)
  
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
  
  # Legend ages
  par(xpd = TRUE)
  
  yl <- 100   # y position of "Age" 
  text("Age", x = 10^6, y = yl, cex = cexl, adj = c(0.5, 1))
  text("Age", x = -10^6, y = yl, cex = cexl, adj = c(0.5, 1))
  
  
  # Plot legend (manually centered)
  par(family = "mono")
  legend(x = 0, y = 105, pch = 15, col = c(colCompletFR, col1DFR, colPop, colCompletEN, col1DEN, colPop), ncol = 2, legend = c("Vaccination complète", " Vaccination 1 dose", "   Non vacciné·e", "", "", ""), xjust = 0.29, yjust = 0, box.lwd = -1, text.font = 1, pt.cex = 2, cex = 0.8)
  
  par(family = "sans")
  mtext("La France a moins vacciné les plus âgé·e·s que l'Angleterre", side = 3, line = 2.75, cex = 1.2, font = 2)
  dev.off()
  
  system(paste0("open ", fname))
} # end version
