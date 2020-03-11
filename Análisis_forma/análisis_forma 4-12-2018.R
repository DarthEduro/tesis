####análisis TPS 03/12/2018

#Paquetes necesarios para relizar la impiortación de coordenaras TPS y 
# los análisis de morfometrías

install.packages("geomorph")

library(geomorph)
library(scales)
options(width=70)


####


morph <- readland.tps("TPS_TESIS 4-12-2018.TPS",
                   specID = "imageID")

#estructura de los datos
str(morph)
dim(morph) # 54 landmarks, dos ejes, 65 sujetos

##### MArcar Semilandmarks

curvas <- read.table("semilandmarks.csv", sep=",",
                     header=T)
curvas

# análisis generalizado de Procrustes

proc_morph <- gpagen(morph, print.progress=F,curves = curvas)

summary(proc_morph)

plot(proc_morph)

###Detección de errores de digitalización en imágenes mediante
###el análisis de outliers

options(width=70)
out <- plotOutliers(proc_morph$coords)
out
names(out)
#####

#### Construyendo base de datos para realizar los analisis alometricos y poner a prueba
# si las pendientes difieren entre individuos de distinto grupo

# Importando la base de datos referentes a grupo
data <- read.table("Base de datos Testo y CSS_Ciudad_Montaña_2018.csv", sep=",", header=T)
names(data) <- c("code","nombre","edad","sexo","altura",
                 "peso","grasa","musculo","gra_vic","cintura",
                 "cadera","hombro","_hom", "cin_cad","hom_cin",
                 "imc","etnia","comunidad","lugar","test", 
                 "comida", "periodon", "enferm","med","cuest",
                 "q1", "q2", "q3","q4","hora")

str(data)
summary(data)

data

## Etiquetas y orden de los individuos por lugar
data$lugar
table(data$lugar)
# Extrayendo datos y combinandolos con la morfometria de morfometria geometrica
# Tamaño de centroide tambien se encuentra guardado en el objeto proc_antder


## Base de datos con la funcion geomorph.data.frame, es conveniente usarla cuando
## tenemos modelos mas complejos

gdf <- geomorph.data.frame(shape=proc_morph$coords, cs= proc_morph$Csize,
                           grupo=data$lugar)

#gdf es una lista
gdf

str(gdf)

## Difieren la forma de la forma de los 2 grupos ¿tipo de analisis?

m1 <- procD.lm(shape ~grupo, data=gdf)
m1

## Comparando las formas consenso entre grupos
table(data$lugar)

which(data$lugar == "urbano")
which(data$lugar == "rural")

## Formas consenso
urbano <-mshape((gdf$shape)[,,1:31])
rural <-mshape((gdf$shape)[,,32:65])

# urbano y rural, forma de urbano como referencia

## En forma de rejilla de deformacion, #mag indica la magnificacion de la deformacion
plotRefToTarget(urbano,rural, method="TPS", mag=3)

## En forma de rejilla de vectores
plotRefToTarget(urbano,rural, method="vector", mag=6)

## Analisis alometricos
                           
## Alometrias de los dos grupos usando la funcion procD.allometry
alogrupo <- procD.allometry(shape~cs, ~grupo, data=gdf)
alogrupo


# a.c) Analisis de covarianza
plot(alogrupo) #Common allometric component of shape: CAC"

plot(alogrupo, method="PredLine") #Primer componente principal vs tamaño

#### B) Analisis de la forma empezando con la funcion.
#    Variable de respuesta: forma
#    Variables predictoras: grupo, parasitos, posiblemente sitio, tamanho [Usamos <procD.lm>]

gdf2<- geomorph.data.frame(proc_morph,
                           grupo=data$lugar)
alogrupo2 <- procD.lm(coords ~ log(Csize) + grupo + grupo:log(Csize), data=gdf2)
summary(alogrupo2)

alogrupo; alogrupo2
