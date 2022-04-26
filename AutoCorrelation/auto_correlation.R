# Analyse spatiale : travail sur la bdd justice


# Installation/chargement des lirairies nÈcessaires

#install.packages("sf")
#install.packages("spdep")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("mapsf")

library(spdep)
library(readr)
library(dplyr)
library(ggplot2)
library(mapsf)
library(sf)

# Importation des jeux de donnÈes :

donnees = st_read("AnalyseSpatialeENSG/DONNEES/TJ/stat_tj_v2.shp")
plot(donnees)

# importation du jeu de donn√©es tribunal :
centroide = st_read("AnalyseSpatialeENSG/DONNEES/anaspa_tribunal/anaspa_tribunal.shp")

# VÈrification du systeme de coordonnees
st_crs(centroide)
centroide = st_transform(centroide, "EPSG:2154")

distance_centroide = st_distance(centroide)
distance_centroide
colnames(centroide)


# Calcul de l'autocorr√©lation spatiale pour les crimes/d√©lits


'''
# A la main :

weight_matrix <- function(decay, layer){
  d = st_distance(layer)
  w = exp(-units::drop_units(d)/decay)
  diag(w)<-0
  return(w)
}

spAutocorr <- function(x,w,m){
  n = length(x)
  cx = x - mean(x)
  cxvec = matrix(cx,nrow=n,ncol=1)
  normalization = n/(sum(w)*sum(cxvec*cxvec))
  return(n*mean(((matrix(data = rep(cx,n), ncol = n, nrow = n, byrow = FALSE)*w)%*%cxvec)/normalization))
}

n = nrow(centroide)
m = matrix(rep(1, n*n), nrow = n, ncol = n);diag(m)<-0
decay = 10000
w = weight_matrix(decay, centroide)

spAutocorr(centroide$total_crim, w, m)

is.na(centroide$total_crim)
'''

# avec le package spdep

donneesnb = poly2nb(donnees)
w = nb2listw(donneesnb)
donneesnb
w

donneesnb$total
moran.test(donnees$total, w, na.action = na.omit)
geary.test(donnees$total, w)

loctrib = localmoran(donnees$total, w)           
donnees$loctrib = loctrib[,1]

mf_map(x = donnees, var = "loctrib", type = "choro")
mf_map(x = donnees, var = "total", type = "choro")


# On va faire la meme chose avec les crimes par habitants (delit_hab)

colnames(donnees)
moran.test(donnees$delit_hab, w, na.action = na.omit)
geary.test(donnees$delit_hab, w)

loccrime_hab = localmoran(donnees$delit_hab, w)           
donnees$loccrime_hab = loccrime_hab[,1]

mf_map(x = donnees, var = "loccrime_hab", type = "choro")
mf_map(x = donnees, var = "delit_hab", type = "choro")

# idem avec les d√©lits par magistrats : delit_mag


colnames(donnees)
moran.test(donnees$delit_mag, w, na.action = na.omit)
geary.test(donnees$delit_mag, w)

locdelit_mag = localmoran(donnees$delit_mag, w)           
donnees$locdelit_mag = locdelit_mag[,1]

mf_map(x = donnees, var = "locdelit_mag", type = "choro")
mf_map(x = donnees, var = "delit_mag", type = "choro")

