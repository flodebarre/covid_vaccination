# Source map data
# https://gadm.org/download_country_v3.html
fra <- readRDS("data/gadm36_FRA_2_sf.rds")

##
# Install packages...
##
#install.packages("rgdal", configure.args = c("--with-proj-lib=/usr/local/lib/", "--with-proj-include=/usr/local/include/"))

#install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")

# install.packages("sf")

# brew update
# brew upgrade
# brew install gdal --HEAD

# WORKED!
# https://github.com/r-spatial/sf/issues/1536#issuecomment-727342736
# Install brew
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
# Install the packages:
#  brew install pkg-config
# brew install gdal proj geos

# 2.1) Not sure it's necessary ... I included it on terminal.

# export LDFLAGS="-L/usr/local/opt/libpq/lib"
# export CPPFLAGS="-I/usr/local/opt/libpq/include"

# #install.packages("devtools") # Worked
# install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source") # worked
# install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source") # worked
# library(devtools) # Worked
# install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/") # Worked

###

library("ggplot2")
library("sf")

#install.packages("viridis")
library(viridis)

?chloroLayer

fra.merged <- merge(fra, final.both, by.x = "CC_2", by.y = "departement_residence")

# Bin data
fra.merged$tmp <- as.factor(1*(fra.merged$diffInjRes.1D < -100000) + 1*(fra.merged$diffInjRes.1D < -50000) + 1*(fra.merged$diffInjRes.1D < -20000) + 1*(fra.merged$diffInjRes.1D < 20000) + 1*(fra.merged$diffInjRes.1D < 50000) + 1*(fra.merged$diffInjRes.1D < 100000) + 1*(fra.merged$diffInjRes.1D < 200000))

fra.merged$tmp 

library(RColorBrewer)
for(i in dev.list()) dev.off()
fra_map <- ggplot(fra.merged) + 
  geom_sf(aes(fill = tmp), show.legend = FALSE, color = "white", size = 0.2) + 
  # enlever l'affichage des coordonnés et de la grille
  coord_sf(datum = NA, expand = FALSE) +
#  scale_fill_viridis(discrete = TRUE, option = "D") +
#  ggtitle("Carte des départements français") +
  scale_fill_brewer(palette = "RdBu") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))+ theme(legend.position = "bottom")
fra_map
 