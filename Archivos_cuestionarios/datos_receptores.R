### Informacion de los evaluadores de los questionarios

###Cargar las cuatro bases de datos

setwd("~/GitHub/tesis/Archivos_cuestionarios")

##base de datos de todo el cuestionario A
base_a <- read.csv("Cuestionario A.1 (Respuestas) - Respuestas de formulario 1.csv") 


## revisar si no hay personas que contestaron dos veces
summary(base_a$Crear.ID..CURP.)
str(base_a$Crear.ID..CURP.)
which(summary(base_a$Crear.ID..CURP.)!=1)


subset(base_a,base_a$Crear.ID..CURP.=="MAEA25121995")

subset(base_a,base_a$Crear.ID..CURP.=="ROSC940128HDFDNS05")

base_a.2 <- base_a [-c(4,44),]

which(summary(base_a.2$Crear.ID..CURP.)!=1)

#escoger solo las columnas que interesan
base_a.3 <- base_a.2 [,-c(1,2,9:137)]
names(base_a.3)
##base de datos de todo el cuestionario B
base_b <- read.csv("Cuestionario B.1 (Respuestas) - Respuestas de formulario 1.csv") 

##cargar info del orden de las fotos
orden_fotos <- read.csv("orden_fotos.csv")

## revisar si no hay personas que contestaron dos veces

which(summary(base_b$Crear.ID..CURP.)!=1)

##ninguno se repite

#escoger solo las columnas que interesan
base_b.2 <- base_b [,-c(1,2,9:137)]

##base de datos de todo el cuestionario C
base_c <- read.csv("Cuestionario C.1 (Respuestas) - Respuestas de formulario 1.csv") 

## revisar si no hay personas que contestaron dos veces

which(summary(base_c$Crear.ID..CURP.)!=1)

subset(base_c,base_c$Crear.ID..CURP.=="LUCJ950727")

subset(base_c,base_c$Crear.ID..CURP.=="VIRL980614")

base_c.2 <- base_c [-c(46),]

#escoger solo las columnas que interesan
base_c.3 <- base_c.2 [,-c(1,2,9:137)]

##base de datos de todo el cuestionario D
base_d <- read.csv("Cuestionario D (Respuestas) - Respuestas de formulario 1.csv") 

## revisar si no hay personas que contestaron dos veces

which(summary(base_d$Crear.ID..CURP.)!=1)

subset(base_d,base_d$Crear.ID..CURP.=="MSTA810302")

subset(base_d,base_d$Crear.ID..CURP.=="PATJ890312")

base_d.2 <- base_d [-c(115,56),]

#escoger solo las columnas que interesan
base_d.3 <- base_d.2 [1:275,-c(1,2,9:145)]

##juntar toa la informacion e los cuestionarios

base_completa <- data.frame(edad = c(base_a.3$Edad,
                                     base_b.2$Edad,
                                     base_c.3$Edad,
                                     base_d.3$Edad), 
                            sexo = c(base_a.3$Sexo,
                                     base_b.2$Sexo,
                                     base_c.3$Sexo,
                                     base_d.3$Sexo)
                            , 
                            orie = c(base_a.3$OrientaciÃ³n.sexual.atracciÃ³n,
                                     base_b.2$OrientaciÃ³n.sexual.atracciÃ³n,
                                     base_c.3$OrientaciÃ³n.sexual.atracciÃ³n,
                                     base_d.3$OrientaciÃ³n.sexual),
                            emba = c(base_a.3$Â.EstÃ.s.embarazada.,
                                     base_b.2$Â.EstÃ.s.embarazada.,
                                     base_c.3$Â.EstÃ.s.embarazada.,
                                     base_d.3$Â.EstÃ.s.embarazada.),
                            lact = c(base_a.3$Â.EstÃ.s.lactando.,
                                     base_b.2$Â.EstÃ.s.lactando.,
                                     base_c.3$Â.EstÃ.s.lactando.,
                                     base_d.3$Â.EstÃ.s.lactando.),
                            anti = c(base_a.3$Â.EstÃ.s.usando.anticonceptivos.hormonales.,
                                     base_b.2$Â.EstÃ.s.usando.anticonceptivos.hormonales.,
                                     base_c.3$Â.EstÃ.s.usando.anticonceptivos.hormonales.,
                                     base_d.3$Â.EstÃ.s.usando.anticonceptivos.hormonales.))

datos <- transform(datos,sexo=factor(sexo,labels=c("Mujer","Hombre")))

base_completa <- transform(base_completa,sexo=factor(sexo,labels=c("h","m","o","o")))

base_completa <- transform(base_completa,sexo=factor(sexo,labels=c("H","M","MyH","")))

base_completa <- rbind(base_a.3, base_b.2, base_c.3, base_d.3, make.row.names = FALSE)
nombres <- c("edad","sexo","ori_sex","emb", "lact", "anticon" )

summary(base_a.3)
h <- 6 +
m <- 3+
MyH <- 1+
H <- 17+
M <- 16+