# Initializations

library("MetBrewer")


# Load the data ####

# Date for timestamp
today <- Sys.Date()

# URL of the data file
URL <- "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv"

dataFile <- paste0("data/ecdc-vaccination_", today, ".csv") # Name of the output file
# Download the data
download.file(URL, dataFile)

# Load the downloaded dataset
dat.ECDC <- read.csv(dataFile, stringsAsFactors = FALSE)

# Clean and reformat the data ####

# Remove regional data, keep only country data
dat.ECDC <- dat.ECDC[is.element(dat.ECDC$Region, dat.ECDC$ReportingCountry), ]

# Load country codes
# Source: https://gist.github.com/radcliff/f09c0f88344a7fcef373
# Modified for Greece (was GR, changed into EL)
cc <- read.csv("data/countryCodes.csv")
# Turn into dictionary
dic.cc <- cc$English.short.name.lower.case
names(dic.cc) <- cc$Alpha.2.code

# Add full name of the countries
dat.ECDC$Country <- dic.cc[dat.ECDC$ReportingCountry]

# Clean memory
rm(dic.cc, cc)

# Check age groups for all countries
table(dat.ECDC$Country, dat.ECDC$TargetGroup)
# Germany, The Netherlands and Liechtenstein do not share age data

# Sub-select data ####
# Only keep the Age<18 Target group
dat.ECDC <- dat.ECDC[dat.ECDC$TargetGroup == "Age<18", ]

# Initialize the new dataset
newdat <- data.frame("TargetGroup" = character(0), 
                     "FirstDose" = numeric(0), 
                     "SecondDose" = numeric(0), 
                     "DoseAdditional1" = numeric(0), # Add booster shots
                     "Population" = numeric(0), 
                     "Denominator" = numeric(0), 
                     "Country" = character(0), 
                     "ReportingCountry" = character(0), 
                     "minAge" = numeric(0), 
                     "maxAge" = numeric(0),
                     "ageWidth" = numeric(0), 
                     "YearWeekISO" = character(0))

# Loop on all dates and all countries
for(thedate in sort(unique(dat.ECDC$YearWeekISO))){
  for(ctr in unique(dat.ECDC$ReportingCountry)){
    
    # Subselect the data until this date and for this country
    tmp <- dat.ECDC[dat.ECDC$ReportingCountry == ctr & dat.ECDC$YearWeekISO <= thedate, ]
    
    if(nrow(tmp) > 1){
      # If there are data at this date!
      
      # Compute cumulated values
      agg <- aggregate(x = tmp[, c("FirstDose", "SecondDose", "DoseAdditional1")], by = list(TargetGroup = tmp[, "TargetGroup"]), FUN = sum)
      # Get the unique values (e.g. population size should not be summed!)
      agg2 <- aggregate(x = tmp[, c("Population", "Denominator", "Country", "ReportingCountry")], by = list(TargetGroup = tmp[, "TargetGroup"]), FUN = unique)
      # Merge the two aggregated datasets
      agg <- merge(agg, agg2, by = "TargetGroup")
      
      # Add the date information
      agg$YearWeekISO <- thedate
      
      # Add these data to the new dataset, only if more than one line, i.e. if detail on age classes is available
        newdat <- rbind(newdat, agg)
    }
  }
}

# Made numerical values numeric again
for(col in c("FirstDose", "SecondDose", "DoseAdditional1", "Population", "Denominator", "minAge", "maxAge")){
  newdat[, col] <- as.numeric(newdat[, col])
}


# Tranform date to have proper date and not just week
# format(as.Date(paste0("2020-W52", "-1"), format = "%Y-W%W-%w"), "%Y-%m-%d") # Check
newdat$fullDate <- format(as.Date(paste0(newdat$YearWeekISO, "-1"), format = "%Y-W%W-%w"), "%Y-%m-%d")
# Error for the end of 2020 because of problematic week format, but we are ignoring it anyway

# Rough plot to select countries
plot(as.Date(newdat$fullDate), newdat$FirstDose/newdat$Denominator)

maxDate <- max(newdat$fullDate, na.rm = TRUE)
tmp <- newdat[which(newdat$fullDate == maxDate), ]
par(xpd = TRUE)
text(x = as.Date(maxDate), tmp$FirstDose/tmp$Denominator, labels = tmp$Country, adj = 0)


#..................................................................
# PLOT!
# Countries
ctrs <- unique(newdat$Country)
cols <- met.brewer("Archambault", n = length(ctrs), type = "continuous")
names(cols) <- ctrs

# Western countries
westCtrs <- c("Spain", "Italy", "Finland", "Belgium", "Ireland", "France", "Norway", "Netherlands")
colsW <- met.brewer("Signac", n = length(westCtrs), type = "discrete")
names(colsW) <- westCtrs
# French versions of the names, as dictionnary
dic.WC <- c("Espagne", "Italie", "Finlande", "Belgique", "Irlande", "France", "Norvège", "Pays-Bas")
names(dic.WC) <- westCtrs


fname <- "pics/vacc18.png" # output name

png(fname, width = 7, height = 7, res = 300, units = "in")
par(mar = c(5, 4, 3, 5))
par(las = 1, mgp = c(2, 0.5, 0))

# Initialize plot
plot(as.Date(newdat$fullDate), 100*newdat$FirstDose/newdat$Denominator, type = "n", axes = FALSE, 
     xlab = "", ylab = "Taux de vaccination (%), au moins une dose, moins de 18 ans", 
     xlim = c(as.Date("2021-05-01"), max(newdat$fullDate, na.rm = TRUE)))

# Graduations
par(xpd = FALSE)
for (i in 100*seq(0, 1, by = 0.01)){
  abline(h = i, col = gray(0.9), lwd = 0.5)
}
for (i in 100*seq(0, 1, by = 0.1)){
  abline(h = i, col = gray(0.9), lwd = 1.75)
  abline(h = i + 5, col = gray(0.9), lwd = 1.25)
}

# Show country lines
for(ctr in westCtrs){
  lwdd <- ifelse(ctr == "France", 4, 1.5)
  tmp <- newdat[newdat$Country == ctr, ]
  lines(as.Date(tmp$fullDate), 100 * tmp$FirstDose / tmp$Denominator, col = colsW[ctr], lwd = lwdd)
}

# Axes
#   Vertical
axis(2, las = 1, lwd = 0, mgp = c(0, 0.2, 0))
#   Horizontal
mths <- seq(as.Date("2020-01-01"), Sys.Date(), by = "month")
axis(1, at = mths, labels = format(mths, "%b\n%Y"), mgp = c(0, 0.75, 0), pos = 0, padj = 0.5, lwd = 0, lwd.ticks = 1, cex = 0.8)

# Add country names at the end of the curves
maxDate <- max(newdat$fullDate, na.rm = TRUE)
tmp <- newdat[which(newdat$fullDate == maxDate & is.element(newdat$Country, westCtrs)), ]
par(xpd = TRUE)
text(x = as.Date(maxDate) + 7, 100 * tmp$FirstDose/tmp$Denominator, labels = dic.WC[tmp$Country], adj = 0, col = colsW[tmp$Country], cex = 0.8)

title(main = "Taux de vaccination (%) au moins une dose
des moins de 18 ans en Europe de l'Ouest")

# Credits
mtext(side = 1, line = 3, text = " Data: https://www.ecdc.europa.eu/en/publications-data/data-covid-19-vaccination-eu-eea\nCode: https://github.com/flodebarre/covid_vaccination/blob/main/childrenVaccination.R", family = "mono", col = gray(0.5), cex = 0.7)
dev.off()
system(paste0("open ", fname))

