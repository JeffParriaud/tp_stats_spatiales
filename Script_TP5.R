#######################################################################
# Exercice 2
#######################################################################

# Question 1:
# Connexion au serveur et vérification des tables présentes
library(DBI)
source(file = "connexion_db.R")
conn<-connecter()
DBI::dbListTables(conn)

# Question 2:
# Affichage des variables présentes dans une table
DBI::dbListFields(conn,"popnaiss_com")

# Question 3:
# Utilisation de dbSendQuery
rs <-dbSendQuery(conn,'SELECT * FROM popnaiss_com')
# Hélas rs n'est pas un dataframe
str(rs)

# Question 4:
# Créer une copie d'une table
rs<-dbGetQuery(conn,'SELECT * FROM popnaiss_com')
# Effectivement rs2 est bien un dataframe
str(rs)

# Question 5:
# Utiliser une sélection avec WHERE
test <-dbGetQuery(conn,"SELECT * FROM popnaiss_com WHERE codgeo = '35238';")

# Question 6:
# Récupérer une information avec une jointure
test <-dbGetQuery(conn,"SELECT * FROM popnaiss_com AS t1
                  INNER JOIN bpe21_metro AS t2 ON t1.codgeo = t2.depcom
                  WHERE t1.codgeo = '35047';")

# Question7a :
# Utilisation du deeplayer pour effectuer des requêtes
library(dplyr)
library(dbplyr)

# Connexion à la table popnaiss
popnaiss<-tbl(conn,"popnaiss_com")
str(popnaiss) # ! ce n'est pas un data.frame

# Reprise de la question 5
popnaiss %>% 
  filter(codgeo=="35047") %>% 
  show_query()
# Show-query() affiche la requête effectuée

pop_bruz <- popnaiss %>% 
  filter(codgeo=="35047") %>% 
  collect()
# collect() récupère l'information demandée et la place dans un dataframe
str(pop_bruz)

# Question 7b:
# Reprendre la question 6 avec le deeplayer

popnaiss<-tbl(conn,"popnaiss_com")
bpe21_metro<-tbl(conn,"bpe21_metro")

equip_bruz <- inner_join(x=popnaiss,y=bpe21_metro,by=c("codgeo"="depcom")) %>%
  filter(codgeo=="35047") %>%
  collect()





#######################################################################
# Exercice 3
#######################################################################

# Question 1:
# Creation d'une table en ne conservant que certaines variables
bpe_dep50 <-dbGetQuery(conn,"SELECT ID, DEPCOM, DOM, SDOM, TYPEQU, GEOMETRY FROM bpe21_metro
                  WHERE dep = '50';")
# Mais le résultat est un dataframe

# Question 2:
# Utilisation de st_read
bpe_dep50b <- st_read(conn,query="SELECT ID, DEPCOM, DOM, SDOM, TYPEQU, GEOMETRY FROM bpe21_metro
                  WHERE dep = '50';")
# Ici on a bien un sf

# Question 3:
# Récupérer un système de projection
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_metro;")
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_04;")
# ou alors
proj_metro<- st_crs(st_read(conn,query="SELECT GEOMETRY FROM bpe21_metro
                  WHERE dep = '50';"))$input
proj_reunion<- st_crs(st_read(conn,query="SELECT GEOMETRY FROM bpe21_04
                  ;"))$input

# Question 4
# Dénombrer les maternités
DBI::dbListFields(conn,"bpe21_metro")
mater <- dbGetQuery(conn,"SELECT reg,count(*) as nbmat FROM bpe21_metro
                  WHERE typequ='D107'
                  GROUP BY reg
                  ORDER BY nbmat desc
                  ;")

# Question 5a:
# Créer la table des cinémas
cinema_bpe <- st_read(conn,query="SELECT * FROM bpe21_metro
                  WHERE typequ='F303';")

# On construit un buffer de 1km (une zone tampon) autour de la sorbonne
# df des coordonnées
sorbonne_buffer <- data.frame(x=2.34297,y=48.84864) %>% 
  #qu'on transforme en objet sf (systeme de proj WGS84 => crs=4326)
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  # on reprojette en LAMBERT-93 (crs=2154)
  st_transform(2154) %>% 
  # on crée la zone tampon autour du point (l'unité est le mètre ici)
  st_buffer(1000) 

str(sorbonne_buffer) # le buffer est constitué d'un unique polygône
plot(sorbonne_buffer %>% st_geometry()) # qui s'avère être un cercle
# Récupérer les cinémas proches de la Sorbonne dans une liste
cinema_1km_sorbonne_list <- st_within(x=cinema_bpe,y=sorbonne_buffer)

cinema_1km_sorbonne <- cinema_bpe %>%
  filter(lengths(cinema_1km_sorbonne_list)>0)
# On pouvait aussi utiliser which(cinema_1km_sorbonne_list[]==1)
cinema_1km_sorbonne %>% nrow()


library(leaflet)
# Optionnel :
# On récupère une icone spécifique sur https://ionic.io/ionicons (mot clé film)
cinemaIcons <- makeIcon(iconUrl = "images/film-sharp.png", 18,18)

leaflet() %>% 
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>% 
  addTiles() %>% 
  addMarkers(lat = 48.84864, lng = 2.34297) %>% 
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>% 
  addMarkers(data = cinema_1km_sorbonne %>% st_transform(4326))


# Remarque : 1000m en LAMBERT-93 ce n'est pas exactement 1000m en WGS84 (zoomez sur la carte suivante)
leaflet() %>%
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>%
  addTiles() %>%
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>%
  addPolygons(data=sorbonne_buffer %>% st_transform(4326), col = "red")    
# Certains cinémas n'ont pas été filtrés!!!!!
