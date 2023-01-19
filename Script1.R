library(sf)
library(dplyr)
library(tidyverse)
communes <- st_read("fond/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
str(communes)

View(communes[1:10,])

# Afficher le système de projection
st_crs(communes)

# Bretagne 22, 29, 35, 56
communes_Bretagne <- communes[which(communes$dep == 22 | communes$dep == 29| 
                                      communes$dep == 35| communes$dep == 56),]
communes_Bretagne <- communes_Bretagne %>% select(code,libelle,dep,epc,surf)
# Vérification de la nature de la table
str(communes_Bretagne)

# Affichage des contenus
plot(communes_Bretagne,lwd=0.2,)

#Affichage de la géométrie
plot(st_geometry(communes_Bretagne),lwd=0.2,)
