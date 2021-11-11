# Compute population sizes from UN population estimates
# (used in FT_2021-11-11.R)

# Source: https://population.un.org/wpp/Download/Standard/CSV/
# File: Medium variant, annual projections from 2020 to 2100 (CSV, 277.91 MB)
# (direct link https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv)
# Then: grep "France" and grep "2020", saved as `UN_France2020.csv` in `data/` folder

# Load data
pops <- read.csv("data/UN_France2020.csv")

# Subsets: 
# Mainland France
pops_FR <- pops[pops$Location == "France", ]

# France and dependencies
pops_FRandDep <- pops[pops$Location == "France (and dependencies)", ]

# Demography function
demog <- function(pop){
  # Total population size
  tot <- sum(pop$PopTotal)
  
  # Subset of 12+ yo
  pop12 <- pop[pop$AgeGrp >=12, ]
  tot12 <- sum(pop12$PopTotal)
  
  c("tot" = tot*1000, "tot12" = tot12*1000)
}

# Mainland France
demog(pops_FR)

# France and dependencies
demog(pops_FRandDep)
