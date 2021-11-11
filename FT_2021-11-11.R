# Context: Wondering about the 8.4% unvaxxed estimate given in https://twitter.com/vincentglad/status/1458745794769608707?s=20 

#### LOAD DATA #### 

# SPF DATA
# File source: 
# https://static.data.gouv.fr/resources/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/20211110-190602/vacsi-tot-a-reg-2021-11-10-19h06.csv
# (available at https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#description)
a <- read.csv("data/vacsi-tot-a-reg-2021-11-10-19h06.csv", sep = ";")
dim(a)

# UN population estimate given by JBM
# https://twitter.com/jburnmurdoch/status/1458752303939428354?s=20
popUN <- 56146413

# UN population estimates - compute them again
source("FT_UNpopEstimates.R")

popUN2 <- demog(pops_FR)[2] # Estimate for "France" data (mainland France)
# Compare to John's estimate (0 = same)
popUN - popUN2

# -> The source of this UN estimate is 
# https://population.un.org/wpp/Download/Standard/CSV/
# Medium variant, annual projections from 2020 to 2100 (CSV, 277.91 MB)
# year 2020, "France"
# and NOT "France (and dependencies)"

# Here is the UN estimate for 12+ with dependencies
popUN_withdep <- 58486854

# Just check I typed it correctly (0 = OK)
popUN_withdep - demog(pops_FRandDep)[2]


#### 1) MAINLAND FRANCE ONLY #####

# Subset of 
# - 12yo and older (clage_vacsi > 11)
# - Mainland France (region code > 10)
suba <- a[a$clage_vacsi > 11 & a$reg > 10, ]
dim(suba)

tot1 <- sum(suba$n_tot_dose1) # Total number at least one dose
totpop <- sum(suba$pop) # Total population size

# % not vaccinated
# With SPF data (OK)
(totpop - tot1) / totpop * 100
# With UN pop estimate for mainland France (OK)
(popUN - tot1) / popUN * 100
# With UN pop estimate with dependencies (Wrong)
(popUN_withdep - tot1) / popUN_withdep * 100


#### 2) WHOLE FRANCE  ####

# Subset of 12yo and older
sub2 <- a[a$clage_vacsi > 11, ]

totvac2 <- sum(sub2$n_tot_dose1) # Total number at least one dose
totpop2 <- sum(sub2$pop) # Total population size

# % Not vaccinated
# SPF data (OK)
(totpop2 - totvac2) / totpop2 * 100
# With UN pop estimate (Wrong)
(popUN - totvac2) / popUN * 100
# With UN pop estimate with dependencies (OK)
(popUN_withdep - totvac2) / popUN_withdep * 100
