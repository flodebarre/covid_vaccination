# Update datasets #

# Load vaccination data

# Source: https://datavaccin-covid.ameli.fr/explore/?exclude.theme=Datavisualisation&sort=modified

URL.AmeliCom <- "https://datavaccin-covid.ameli.fr/explore/dataset/donnees-de-vaccination-par-commune/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"

system(paste0("wget -O data/vaccCom.csv ", URL.AmeliCom))


URL.AmeliEPCI <- "https://datavaccin-covid.ameli.fr/explore/dataset/donnees-de-vaccination-par-epci/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"

system(paste0("wget -O data/vaccEPCI.csv ", URL.AmeliEPCI))


URL.AmeliDep <- "https://datavaccin-covid.ameli.fr/explore/dataset/donnees-vaccination-par-tranche-dage-type-de-vaccin-et-departement/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
# La démographie par département (source Insee) au 1er janvier 2020

system(paste0("wget -O data/vaccDep.csv ", URL.AmeliDep))

