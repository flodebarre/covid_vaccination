#### Download data ####
dlData <- FALSE

if(dlData){
  # Amelia data
  # source("0_loadAmeliData.R")
  
  # SPF data
  URL.SPF <- "https://www.data.gouv.fr/en/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd"
  system(paste0("wget -O data/vaccSPF.csv ", URL.SPF))
  
  URL.SPF2 <- "https://www.data.gouv.fr/en/datasets/r/dc103057-d933-4e4b-bdbf-36d312af9ca9"
  system(paste0("wget -O data/vaccSPF_tot.csv ", URL.SPF2))
}

#### Load data ####

# Ameli
vaccEPCI <- read.csv("data/vaccEPCI.csv", sep = ";")
head(vaccEPCI)
# Find final time
finalDate <- max(vaccEPCI$date)
finalDate
# Subset the data to only keep the most recent
vaccEPCI <- vaccEPCI[which(vaccEPCI$date == finalDate), ]

sum(vaccEPCI[vaccEPCI$classe_age == "TOUT_AGE", "population_carto"])
names(vaccEPCI)

# SPF
vaccSPF <- read.csv("data/vaccSPF.csv", sep = ";")
head(vaccSPF)
# Select values of the same date as the Ameli data
vaccSPF <- vaccSPF[vaccSPF$jour == finalDate, ]

sort(unique(vaccSPF$clage_vacsi))

# SPF, final, which contains population values
tmp <- read.csv("data/vaccSPF_tot.csv", sep = ";")

# Add this info to the SPF dataset
vaccSPF <- merge(vaccSPF, tmp[, c("clage_vacsi", "pop")], by = "clage_vacsi", all = TRUE)

# Load demographic data
demoAmeli <- read.csv("data/demographic/insee_2021_Ameli.csv")
demoAmeli
demoSPF <- read.csv("data/demographic/insee_2021_SPF.csv")
demoSPF

# Add new population data
vaccSPF <- merge(vaccSPF, demoSPF, "clage_vacsi")
vaccSPF
vaccSPF$couv_tot_dose1_2021 <- 100 * vaccSPF$n_cum_dose1 / vaccSPF$pop2021
vaccSPF$couv_tot_complet_2021 <- 100 * vaccSPF$n_cum_complet / vaccSPF$pop2021

# Names of the age classes of SPF
ageClasses <- sort(unique(vaccSPF$clage_vacsi))
names(ageClasses) <- c("Tous âges", "0-4 ans", "5-9 ans", "10-11 ans", "12-17 ans", "18-24 ans", "25-29 ans", "30-39 ans", "40-49 ans", "50-59 ans", "60-64 ans", "65-69 ans", "70-74 ans", "75-79 ans", "80 ans et +")
ageClasses
# Reverse dictionary
ac2 <- names(ageClasses)
names(ac2) <- ageClasses

vaccSPF$clage_name <- ac2[as.character(vaccSPF$clage_vacsi)]

plot(seq_len(nrow(vaccSPF)), vaccSPF$couv_dose1, 
     ylim = c(0, 100))
points(seq_len(nrow(vaccSPF)), vaccSPF$couv_tot_dose1_2021, col = 2)
axis(1, at = seq_len(nrow(vaccSPF)), labels = names(ageClasses), las = 3, cex.lab = 0.6)

ac <- sort(unique(vaccEPCI$classe_age))
ac

#### Compute Ameli age classes on SPF data ####
vaccSPF2 <- data.frame(classe_age = c("65-74", "75 et +", "TOUT_AGE"))
inds <- list(c(69, 74), c(79, 80), c(0))

out <- matrix(0, ncol = 4, nrow = 3)
for(i in 1:3){
  out[i, ] <- colSums(vaccSPF[is.element(vaccSPF$clage_vacsi, inds[[i]]), c("n_cum_dose1", "n_cum_complet", "pop", "pop2021")])
}
out
vaccSPF2$n_tot_dose1 <- out[, 1]
vaccSPF2$n_tot_complet <- out[, 2]
vaccSPF2$pop <- out[, 3]
vaccSPF2$pop2021 <- out[, 4]
vaccSPF2$couv_dose1 <- 100 * vaccSPF2$n_tot_dose1 / vaccSPF2$pop
vaccSPF2$couv_complet <- 100 * vaccSPF2$n_tot_complet / vaccSPF2$pop
vaccSPF2$couv_dose1_2021 <- 100 * vaccSPF2$n_tot_dose1 / vaccSPF2$pop2021
vaccSPF2$couv_complet_2021 <- 100 * vaccSPF2$n_tot_complet / vaccSPF2$pop2021

vaccSPF2

# Compute national values for Ameli data
vaccAmeli <- data.frame(classe_age = sort(unique(vaccEPCI$classe_age)))
vaccAmeli$n_tot_dose1 <- NA
vaccAmeli$n_tot_complet <- NA
vaccAmeli$pop <- NA
for(ag in vaccAmeli$classe_age){
  subdat <- vaccEPCI[vaccEPCI$classe_age == ag,]
  tmp <- colSums(subdat[, c("effectif_cumu_1_inj", "effectif_cumu_termine", "population_carto")], na.rm = TRUE)
  vaccAmeli[vaccAmeli$classe_age == ag, 2:4] <- tmp
}
vaccAmeli

vaccAmeli$couv_dose1 <- 100 * vaccAmeli$n_tot_dose1 / vaccAmeli$pop
vaccAmeli$couv_complet <- 100 * vaccAmeli$n_tot_complet / vaccAmeli$pop

plot(seq_len(nrow(vaccAmeli)), vaccAmeli$couv_dose1, 
     ylim = c(0, 100))
axis(1, at = seq_len(nrow(vaccAmeli)), labels = vaccAmeli$classe_age, las = 3, cex.lab = 0.6)

points(5:7, vaccSPF2$couv_dose1, col = 2)
points(5:7, vaccSPF2$couv_dose1_2021, col = 3)

plot(seq_len(nrow(vaccAmeli)), vaccAmeli$n_tot_dose1, ylim= c(0, max(vaccSPF2$n_tot_dose1)))
axis(1, at = seq_len(nrow(vaccAmeli)), labels = vaccAmeli$classe_age, las = 3, cex.lab = 0.6)

### TODO: SPF as function of time and take same date

points(5:7, vaccSPF2$n_tot_dose1, col = 2)


