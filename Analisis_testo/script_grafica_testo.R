###Análisis testosterona 

##CArgar base de datos TEsis 

bd <- read.csv("Base de datos Testo y CSS_Ciudad_Montaña_2018.csv")

##generar un subset con los datos de comunidad y testosterona

datos <- data.frame(bd$Lugar,bd$Test..pg.ml.)

plot(datos$bd.Test..pg.ml.~datos$bd.Lugar,
     frame=F,
     ylim = c(0,300),
     xlab = "Población",
     ylab = "Testosterona (pg/ml)",
     main ="Niveles de Testosterona (pg/ml) en cada población",
     names= c("Ciudad de México", "Población Indigena"))

mis_niveles<-levels(datos$bd.Lugar)

proporcion_nivel <-summary(datos$bd.Lugar)/nrow(datos)

for(i in 1:length(mis_niveles)){
  
  cada_nivel<-mis_niveles[i]
  cada_valor<-datos[datos$bd.Lugar==cada_nivel, "bd.Test..pg.ml."]
  
  
  mis_puntos<-jitter(rep(i, length(cada_valor)), amount=proporcion_nivel[i]/2)
  points(mis_puntos, cada_valor, pch=20, col=rgb(0,0,0,.2)) 
  
}

