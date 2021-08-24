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
cc <- read.csv("../data/countryCodes.csv")
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

# Define age classes
ac1 <- data.frame(ageClass = c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "Age18_24", "Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+"))

ac2 <- data.frame(ageClass = c("Age<18", "Age18_24", "Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+"))

ac1$minAge <- c(0, 5, 10, 15, 18, 25, 50, 60, 70, 80)
ac1$maxAge <- c(4, 9, 14, 17, 24, 49, 59, 69, 79, 100)
ac1$ageWidth <- ac1$maxAge - ac1$minAge + 1

ac2$minAge <- c(0, 18, 25, 50, 60, 70, 80)
ac2$maxAge <- c(17, 24, 49, 59, 69, 79, 100)
ac2$ageWidth <- ac2$maxAge - ac2$minAge + 1



# Initialize the new dataset
newdat <- data.frame("TargetGroup" = character(0), 
                     "FirstDose" = numeric(0), 
                     "SecondDose" = numeric(0), 
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
      agg <- aggregate(x = tmp[, c("FirstDose", "SecondDose")], by = list(TargetGroup = tmp[, "TargetGroup"]), FUN = sum)
      # Get the unique values (e.g. population size should not be summed!)
      agg2 <- aggregate(x = tmp[, c("Population", "Denominator", "Country", "ReportingCountry")], by = list(TargetGroup = tmp[, "TargetGroup"]), FUN = unique)
      
      agg <- merge(agg, agg2, by = "TargetGroup")
      
      # If there is no line for Age<18, add it
      if(all(agg$TargetGroup != "Age<18")){
        agg <- rbind(agg, c("Age<18", 0, 0, agg[agg$TargetGroup == "ALL", "Population"], NA, agg[agg$TargetGroup == "ALL", "Country"], agg[agg$TargetGroup == "ALL", "ReportingCountry"]))
      }
      
      # Add Denominator to Age<18 if it is missing
      if(is.na(agg[agg$TargetGroup == "Age<18", "Denominator"])){
        agg[agg$TargetGroup == "Age<18", "Denominator"] <- as.numeric(agg[agg$TargetGroup == "ALL", "Population"]) - as.numeric(agg[agg$TargetGroup == "ALL", "Denominator"])
      }
      
      # Subselect the age class data that we need
      # Detailed age classes for children
      ac.children <- setdiff(ac1$ageClass, ac2$ageClass)
      # Subset of the data for these age classes
      tagg <- agg[is.element(agg$TargetGroup, ac.children), ]
      # NOTE: consider using "any" instead of "all" here (actually all is OK)
      if(all(is.na(tagg$Denominator))){
        # If we are missing all the population sizes, use the other age classes (ac2)
        agg <- agg[is.element(agg$TargetGroup, ac2$ageClass), ]
        # Add age class information
        agg <- merge(agg, ac2, by.x = "TargetGroup", by.y = "ageClass", all.x = TRUE)
      }else{
        # But if we have some values, use them
        agg <- agg[is.element(agg$TargetGroup, ac1$ageClass), ]
        # Add age class information
        agg <- merge(agg, ac1, by.x = "TargetGroup", by.y = "ageClass", all.x = TRUE)
      }
      # Add the date information
      agg$YearWeekISO <- thedate
      
      # Add these data to the new dataset, only if more than one line, i.e. if detail on age classes is available
      if(nrow(agg) > 1){
        newdat <- rbind(newdat, agg)
      }
    }
    
  }
}

# Made numerical values numeric again
for(col in c("FirstDose", "SecondDose", "Population", "Denominator", "minAge", "maxAge")){
  newdat[, col] <- as.numeric(newdat[, col])
}


# Save the output
write.csv(newdat, file = paste0("data/ecdc-vaccination_cumulated_", today, ".csv"), row.names = FALSE)
