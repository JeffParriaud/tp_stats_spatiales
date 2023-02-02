# Charger les librairies
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)

##################################
# Exercice 1
##################################

# Question 1

pop19 <- readxl::read_xlsx("Donnees/Pop_legales_2019.xlsx") %>%
  # Arrondissements de Paris
  mutate(
    COM = ifelse(substr(COM,1,2) == 75, "75056", COM)
  ) %>%
  group_by(COM) %>%
  summarise(PMUN19 = sum(PMUN19), .groups = 'drop')
str(pop19)

metro_sf <- st_read("Fonds_carte/France_metro/commune_francemetro_2021.shp", 
                    options = "ENCODING=WINDOWS-1252"
                    ) %>%
  # Opérer la jointure avec les codes commune
  left_join(
    pop19, by = c("code"="COM")
  ) %>%
  # Remplacer la variable densité par ce calcul
  mutate(
    DENSITE = PMUN19/surf
    )

# Question 2
# Distribution de la variable densité
hist(metro_sf$DENSITE)
summary(metro_sf$DENSITE)
quantile(metro_sf$DENSITE,probs=seq(0,1,0.1))

# Question 3
plot(metro_sf["DENSITE"],border=FALSE)

# Question 4
plot(metro_sf["DENSITE"],border=FALSE,breaks="sd",main="standard method")
plot(metro_sf["DENSITE"],border=FALSE,breaks="jenks",main="jenks method")
plot(metro_sf["DENSITE"],border=FALSE,breaks="quantile",main="quantile method")
plot(metro_sf["DENSITE"],border=FALSE,breaks="pretty",main="pretty method") 
# La plus intéressante est celle des quantiles, mais il faudrait modifier l'affichage de la légende

# Question 5a
densite_decoupe_quantile <- classIntervals(
  metro_sf$DENSITE,
  style="quantile",
  n=5
)
summary(densite_decoupe_quantile$var)
summary(densite_decoupe_quantile$brks)

# Question 5b
pal1 <- RColorBrewer::brewer.pal(n=5,name="YlOrRd")
plot(densite_decoupe_quantile$brks,pal=pal1,main="decoupage quantile")
table(
  cut(
    metro_sf$DENSITE,
    breaks = densite_decoupe_quantile$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)

# Question 5c
# Avec méthode standard
densite_decoupe_sd <- classIntervals(
  metro_sf$DENSITE,
  style="sd",
  n=5
)
plot(densite_decoupe_sd$brks,pal=pal1,main="decoupage standard")
table(
  cut(
    metro_sf$DENSITE,
    breaks = densite_decoupe_sd$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)
# Avec méthode jenks
densite_decoupe_jenks <- classIntervals(
  metro_sf$DENSITE,
  style="jenks",
  n=5
)
plot(densite_decoupe_jenks$brks,pal=pal1,main="decoupage jenks")
table(
  cut(
    metro_sf$DENSITE,
    breaks = densite_decoupe_jenks$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)
# Avec méthode pretty
densite_decoupe_pretty <- classIntervals(
  metro_sf$DENSITE,
  style="pretty",
  n=5
)
plot(densite_decoupe_jenks$brks,pal=pal1,main="decoupage pretty")
table(
  cut(
    metro_sf$DENSITE,
    breaks = densite_decoupe_pretty$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)

# Question 5d
metro_sf <- metro_sf %>%
  mutate(
    DENSITE_cat=cut(
      DENSITE,
      breaks = c(0,40,162,1000,8000,27310),
      include.lowest=TRUE,
      right=FALSE,
      ordered_result=TRUE
    )
  )
table(metro_sf$DENSITE_cat,useNA="always")
metro_sf %>%
  ggplot() +
  geom_bar(aes(x=DENSITE_cat))
plot(metro_sf["DENSITE_cat"],border=FALSE,pal=pal1)




############################
# Exercice 2
############################

# Question 1
tx_pauvrete <- readxl::read_xlsx("Donnees/Taux_pauvrete_2018.xlsx")
tx_pauvrete <-tx_pauvrete[1:96,]
str(tx_pauvrete)

dep_sf <- st_read("Fonds_carte/France_metro/dep_francemetro_2021.shp", 
                    options = "ENCODING=WINDOWS-1252"
) 

mer <- st_read("Fonds_carte/merf_2021/merf_2021.shp")

# Jointure pour rajouter le taux de pauvreté à notre fond départemental
dep_sf <- dep_sf %>%
  left_join(
    tx_pauvrete %>% select (-Dept),
    by=c("code"="Code")
  )
str(dep_sf)
summary(dep_sf$Tx_pauvrete)
boxplot(dep_sf$Tx_pauvrete)

# Méthode de Fisher
mf_map(
  x=dep_sf,
  var = "Tx_pauvrete",
  type = "choro",
  nbreaks=4,
  breaks="fisher"
)

# Méthode des classes de même amplitude
mf_map(
  x=dep_sf,
  var = "Tx_pauvrete",
  type = "choro",
  nbreaks=4,
  breaks="equal"
)

# Méthode des classes basées sur les quantiles
mf_map(
  x=dep_sf,
  var = "Tx_pauvrete",
  type = "choro",
  nbreaks=4,
  breaks="quantile"
)



# Question 2
# Filtrage de la petite couronne parisienne
dep_idf <-dep_sf %>%
  filter(code %in% c("75","92","93","94"))

# Création d'une sortie pdf
pdf(file="macarte.pdf",width=9,height=11)
# Affichage de la carte principale avec découpage manuel des classes
mf_map(
  x=dep_sf,
  var = "Tx_pauvrete",
  type = "choro",
  nbreaks=4,
  breaks=c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
  pal="Mint",
  leg_pos=NA
)

# Ouverture d'un encadré
mf_inset_on(
  x=dep_sf,
  pos="topright",
  cex = .2
)
mf_init(dep_idf)
# Affichage de la petite couronne en insert
mf_map(
  x=dep_idf,
  var = "Tx_pauvrete",
  type = "choro",
  nbreaks=4,
  breaks=c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
  pal="Mint",
  leg_pos=NA,
  add=TRUE
)
# Labels
mf_label(
  dep_idf,
  var="code",
  col="black"
)
# Fermeture de l'encadré
mf_inset_off()
# On rajoute la mer
mf_map(mer,col="steelblue",add=TRUE)
# Legende
mf_legend(
  type="choro",
  title = "Taux de pauvreté",
  val =c("","Moins de 13","De 13 à moins de 17","De 17 à moins de 25","25 ou plus"),
  pal="Mint",
  pos="left"
)
mf_layout(
  title="Taux de pauvreté par département en 2018",
  credits="Source : Insee - © IGN - Insee _ 2021"
)
# Fin de la sortie pdf
dev.off()
dev.off() # Répéter jusqu'à affichage d'une erreur

# Question 3
st_write(dep_sf,"dept_tx_pauvrete_2018.gpkg")
