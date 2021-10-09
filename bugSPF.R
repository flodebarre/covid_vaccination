#### Download the data ####
# No need to do this again, but kept for legacy
dlData <- FALSE

# History of the dataset can be found on the covidtracker repo
# (thank you for using GitHub!)

if(dlData){
  ## Previous datasets
  URL1 <- "https://raw.githubusercontent.com/rozierguillaume/covid-19/6a58b1865ca863ce27699dd7dbcd849708e5e5a5/data/france/donnees-vacsi-a-fra.csv"
  system(paste0("wget -O data/SPF/vacc_2021-09-27.csv ", URL1))

  URL2 <- "https://raw.githubusercontent.com/rozierguillaume/covid-19/0297c752b2d49e25986b996c359da6b1bb0089b2/data/france/donnees-vacsi-a-fra.csv"
  system(paste0("wget -O data/SPF/vacc_2021-09-24.csv ", URL2))
  
  URL3 <- "https://raw.githubusercontent.com/rozierguillaume/covid-19/9271d9a6e255b19be43cfd0ecb8fd54926449abe/data/france/donnees-vacsi-a-fra.csv"
  system(paste0("wget -O data/SPF/vacc_2021-09-16.csv ", URL3))
  
  URL4 <- "https://raw.githubusercontent.com/rozierguillaume/covid-19/f26497830826192410d3da5ccf15f86e9fcc075b/data/france/donnees-vacsi-a-fra.csv"
  system(paste0("wget -O data/SPF/vacc_2021-09-01.csv ", URL4))
  
  ## Today's dataset
  URL.SPF <- "https://www.data.gouv.fr/en/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd"
  system(paste0("wget -O data/SPF/vacc_2021-10-09.csv ", URL.SPF))
}

#### Load data #### 

data0 <- read.csv("data/SPF/vacc_2021-10-09.csv", sep = ";")
head(data0)

data1 <- read.csv("data/SPF/vacc_2021-09-27.csv", sep = ";")
head(data1)
max(data1$jour)

data2 <- read.csv("data/SPF/vacc_2021-09-24.csv", sep = ";")
head(data2)
max(data2$jour)

data3 <- read.csv("data/SPF/vacc_2021-09-16.csv", sep = ";")
head(data3)
max(data3$jour)

data4 <- read.csv("data/SPF/vacc_2021-09-01.csv", sep = ";")
head(data4)


# Define colors for the age classes
length(unique(data3$clage_vacsi))
library("colorspace")
pal <- colorspace::qualitative_hcl(length(unique(data3$clage_vacsi)), palette = "Dark3")
plot(seq_along(pal), col = pal, pch = 16)
pal <- c(gray(0), pal[-length(pal)])
plot(seq_along(pal), col = pal, pch = 16)
names(pal) <- as.character(sort(unique(data3$clage_vacsi)))
pal
length(pal)
library(grDevices)

# Define and name age classes
ageClasses <- sort(unique(data3$clage_vacsi))
#ageClasses <- c(74, 79, 80)
names(ageClasses) <- c("Tous âges", "0-4 ans", "5-9 ans", "10-11 ans", "12-17 ans", "18-24 ans", "25-29 ans", "30-39 ans", "40-49 ans", "50-59 ans", "60-64 ans", "65-69 ans", "70-74 ans", "75-79 ans", "80 ans et +")
ageClasses

# Plotting specifications for the datasets
ltys <- c(1, 1, 2, 2, 4)
lwds <- 2*c(1, 1, 1.5, 1.25, 2)
cols <- c(gray(0), "orchid3", "seagreen3", "lightskyblue3", "orange2", "khaki2")

#### PLOT ####

par(las = 1)

# layout(matrix(1:16, ncol = 4, byrow = TRUE))
layout(1)
sc <- 0.75 # scale of pdf
for(i in seq_along(ageClasses)){
  pdf(width = sc*7, height = sc*8, file = paste0("pics/bugSPF_", ageClasses[i], ".pdf"))
  par(mar = rep(2.5, 4) + c(2.5, 0, 0, -2.25))
  par(mgp = c(1.25, 0.25, 0), tck = -.005)
  par(las = 1)
  # Initialize the plot
plot(as.Date(data0$jour), data0$couv_complet, type = "n", ylim = c(0, 100), 
     xlab = "Jour", ylab = "Taux vaccination complète", frame.plot = FALSE, 
     xaxs = "i", yaxs = "i")

  ag <- ageClasses[i]
  subdat <- data0[data0$clage_vacsi == ag, ]
  lines(as.Date(subdat$jour), subdat$couv_complet, col = cols[1], lwd = lwds[1], lty = ltys[1])
  par(xpd = TRUE)
  #text(as.Date(max(subdat$jour)), max(subdat$couv_complet), names(ageClasses)[i], cex = 0.7, adj = c(0.5, -0.5))
  par(xpd = FALSE)
  
  subdat <- data1[data1$clage_vacsi == ag, ]
  lines(as.Date(subdat$jour), subdat$couv_complet, col = cols[2], lwd = lwds[2], lty = ltys[2])

  subdat <- data2[data2$clage_vacsi == ag, ]
  lines(as.Date(subdat$jour), subdat$couv_complet, col = cols[3], lwd = lwds[3], lty = ltys[3])
  
  subdat <- data3[data3$clage_vacsi == ag, ]
  lines(as.Date(subdat$jour), subdat$couv_complet, col = cols[4], lwd = lwds[4], lty = ltys[4])
  
  subdat <- data4[data4$clage_vacsi == ag, ]
  lines(as.Date(subdat$jour), subdat$couv_complet, col = cols[5], lwd = lwds[5], lty = ltys[5])
  
  title(main = names(ageClasses)[i])
  
  legend("topleft", col = cols, lty = ltys, lwd = lwds, legend = c("2021-10-09", "2021-09-27", "2021-09-24", "2021-09-16", "2021-09-01"), bty = "n", title = "Date du jeu de données")
  
  mtext("@flodebarre | Code : https://github.com/flodebarre/covid_vaccination/blob/main/bugSPF.R
Données SPF, téléchargées sur 
https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/ 
pour 2021-10-09, et pour les autres sur l'historique de 
https://github.com/rozierguillaume/covid-19/blob/master/data/france/donnees-vacsi-a-fra.csv
", side = 1, line = 4.35, col = gray(0.6), cex = 0.45, font = 1, family = "mono", adj = 0)
  
  dev.off()
}

for(ag in ageClasses){
  system(paste0("convert -density 192 pics/bugSPF_", ag, ".pdf -quality 100 -alpha remove pics/bugSPF_", ag, ".png"))
}

