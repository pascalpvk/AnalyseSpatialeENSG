################################################################################
################################################################################
########################                             ###########################
########################     library cartography     ###########################
########################                             ###########################
################################################################################
################################################################################

                                      ####

################################################################################
###                 installation packages - si besoin                        ###
################################################################################

install.packages("cartography")
install.packages("sf")
install.packages("cartogram")

                                       ##

###                            appel des packages                            ###

library(cartography)
library(sf)
library(cartogram)

                                      ####

################################################################################
###                            gestion des couleurs                          ###
################################################################################

# Creation d'une palette

display.carto.all(20)                             # vue des couleurs disponibles
display.carto.pal("turquoise.pal")                          # zoom sur turquoise
mypal <- carto.pal(pal1 = "wine.pal", n1 = 5, pal2 = "green.pal", n2 = 4)
                                           # creation de la palette � 2 couleurs
image(1:9, 1, as.matrix(1:9), col=mypal, xlab = "", ylab = "", xaxt = "n", 
      yaxt = "n",bty = "n")                        # visualisation de la palette

# Creation d'une couleur transparente

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END


################################################################################
###                             import des donn�es                           ###
################################################################################

                                          ##

###                      un set d'entrainement : La Martinique               ###

# donn�es Martinique

mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), 
               quiet = TRUE)

                                          ##

###             un set de travail (adapter la racine du fichier)             ###

# donn�es France / criminalit�

# par d�partement (surfacique)

depts <- st_read("D:/5-projets/donn�es_travail/anaspa_dep/depts_enrichis.gpkg")

# par commissariat/gendarmerie

serv <- st_read("D:/5-projets/donn�es_travail/anaspa_service/donnees_services.gpkg")

################################################################################
###                               premiers pas                               ###
################################################################################


### Carte des communes de la Martinique ###

plot(
  st_geometry(mtq), 
  col = "lightblue4", 
  border = "lightblue3", 
  bg = "lightblue1"
)

# Ajout de la population de chaque commune (symbole proportionn�)

propSymbolsLayer(
  x = mtq, 
  var = "POP", 
  legend.title.txt = "Population totale\n(2015)"
)

# Ajout du titre

title(main = "Population en Martinique")

### avec data depts ###

# Carte des departements FR

plot(
  st_geometry(depts), 
  col = "lightblue4", 
  border = "lightblue3", 
  bg = "lightblue1"
)

# Ajout crimes par d�partements

propSymbolsLayer(
  x = depts, 
  var = "inf_2017_e", 
  symbols = "circle",
  col = "blue",
  legend.title.txt = "D�lits constat�s\n(2018)"
)

# Ajout du titre

title(main = "Carte des d�lits constat�s en France par d�partement")


################################################################################
###                         carte avec 2 variables                           ###
################################################################################

# Fond de carte

plot(
  st_geometry(depts), 
  col="darkseagreen3", 
  border="darkseagreen4",  
  bg = "lightblue1"
)

# Ajout couche representant deux variables

propSymbolsChoroLayer(
  x = depts, 
  var = "inf_2017_e", 
  border = "grey50",
  lwd = 1,
  legend.var.pos = "topright", 
  legend.var.title.txt = "Nombre d�lits",
  var2 = "revenu_med",
  method = "equal", 
  nclass = 4, 
  col = carto.pal(pal1 = "sand.pal", n1 = 4),
  legend.var2.values.rnd = -2,
  legend.var2.pos = "left", 
  legend.var2.title.txt = "Revenu\nm�dian\n(en euros)"
) 

# Ajout du titre

title("D�lits et revenu m�dian par d�partement en France, 2018")


################################################################################
###                              carte choroplete                            ###
################################################################################

# Carte choroplete

choroLayer(
  x = depts, 
  var = "revenu_med", 
  breaks = c(15000, 19000, 22000, 25000, 31057),
  col = c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33", "#7E1512"),
  legend.pos = "left",
  legend.title.txt = "Revenu m�dian"
)

# Ajout d'une couche supplementaire avec transparence

propSymbolsLayer(
  x = depts, 
  var = "inf_2017_e", 
  symbols = "circle",
  col = t_col("blue", perc = 50),
  legend.pos = "right", 
  legend.title.txt = "D�lits constat�s\n(2018)"
)

# Ajout du titre et d'elements de legende

layoutLayer(
  title = "D�lits et revenu m�dian par d�partement en France, 2018", 
  sources = "ENSG", 
  author = "Delord & Cie, 2019",
  north = TRUE
)

################################################################################
###                      plusieurs cartes accolees                           ###
################################################################################

# Definition du nombre de cartes (ici 4 : 2x2)

par(mfrow=c(2,2), mar = c(0,.2,1.2,.2))

# Insertion carte 1

choroLayer(
  x = depts, 
  var = "revenu_med", 
  breaks = c(0, 7500, 15000, 22500, 31057),
  col = c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33", "#7E1512"),
  legend.pos = "right",
  legend.title.txt = "Revenu m�dian"
)
propSymbolsLayer(
  x = depts, 
  var = "inf_2017_e",
  inches = 0.2,
  symbols = "circle",
  col = t_col("blue", perc = 50),
  legend.pos = "topleft", 
  legend.title.txt = "D�lits constat�s\n(2018)"
)
layoutLayer(
  title = "D�lits et revenu m�dian par d�partement en France, 2018", 
  sources = "ENSG", 
  author = "Delord & Cie, 2019",
  north = TRUE
)

# Insertion carte 2

choroLayer(
  x = depts, 
  var = "ss_dipl", 
  breaks = c(0, 100000, 200000, 300000, 600000),
  col = c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33", "#7E1512"),
  legend.pos = "left",
  legend.title.txt = "Non diplom�s"
)
layoutLayer(
  title = "Non diplom�s par d�partement en France, 2018", 
  sources = "ENSG", 
  author = "Delord & Cie, 2019",
  north = TRUE
)

# Insertion carte 3

choroLayer(
  x = depts, 
  var = "POPULATION", 
  breaks = c(0, 200000, 400000, 600000, 1000000),
  col = c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33", "#7E1512"),
  legend.pos = "left",
  legend.title.txt = "Population"
)
layoutLayer(
  title = "Population par d�partement en France, 2018", 
  sources = "ENSG", 
  author = "Delord & Cie, 2019",
  north = TRUE
)

# Insertion carte 4

choroLayer(
  x = depts, 
  var = "dipl_sup", 
  breaks = c(0, 100000, 200000, 300000, 600000),
  col = c("#F1B1B4", "#E3898E", "#D35E63", "#BD2D33", "#7E1512"),
  legend.pos = "left",
  legend.title.txt = "Diplom�s sup�rieurs"
)
layoutLayer(
  title = "Diplom�s sup�rieurs par d�partement en France, 2018", 
  sources = "ENSG", 
  author = "Delord & Cie, 2019",
  north = TRUE
)

################################################################################
###                         Cartogramme de Dorling                           ###
################################################################################

depts_dorling <- cartogram_dorling(x = depts, weight = "POPULATION", k = 12) 

plot(st_geometry(depts_dorling), col = "#940000", border= "white", bg = "lightblue")

labelLayer(x = depts_dorling, txt = "CODE_DEPT", overlap = FALSE, show.lines = FALSE, 
           halo = TRUE, r = .15)
layoutLayer("D�lits en France par d�partement - Cartogramme de Dorling",tabtitle=TRUE, 
            author= "INSEE 2016", sources="", frame=FALSE, scale = NULL)

################################################################################
###                         Cartogramme de Dougenik                           ###
################################################################################

depts_cont <- cartogram_cont(x = depts, weight = "inf_2017_e", prepare = "none") 
plot(st_geometry(depts_cont),
     col = c("#7E1512", "#BD2D33", "#D35E63", "#E3898E", "#F1B1B4"),
     border= "white", bg = "lightblue")
layoutLayer("D�lits en France - Cartogramme de Dougenik",tabtitle=TRUE, 
            author= "INSEE 2018", sources="", frame=TRUE, scale = FALSE)

depts_pop <- cartogram_cont(x = depts, weight = "POPULATION", prepare = "none")
plot(st_geometry(depts_pop),
     col = c("#7E1512", "#BD2D33", "#D35E63", "#E3898E", "#F1B1B4"),
     border= "white", bg = "lightblue")
layoutLayer("Population des d�partements en France - Cartogramme de Dougenik",
            tabtitle=TRUE, 
            author= "INSEE 2018", sources="", frame=TRUE, scale = FALSE)
