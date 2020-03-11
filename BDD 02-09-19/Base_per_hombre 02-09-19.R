########### base de datos de respuestas de percepcion discriminado por sexo ###############

##### Sexo Hombre########



getwd()
setwd("C:/Users/jscar/Documents/Tesis/BDD 30-08-19")

##########base de datos de todo el cuestionario A
base_a <- read.csv("Cuestionario A.1 (Respuestas) - Respuestas de formulario 1.csv") 

##cargar info del orden de las fotos
orden_fotos <- read.csv("orden_fotos.csv")

## revisar si no hay personas que contestaron dos veces
summary(base_a$Crear.ID..CURP.)
str(base_a$Crear.ID..CURP.)
which(summary(base_a$Crear.ID..CURP.)!=1)


subset(base_a,base_a$Crear.ID..CURP.=="MAEA25121995")

subset(base_a,base_a$Crear.ID..CURP.=="ROSC940128HDFDNS05")

base_a.2 <- base_a [-c(4,44),]

which(summary(base_a.2$Crear.ID..CURP.)!=1)

###### filtrar por sexo Hombre

base_a.2 <-subset(base_a.2,base_a.2$Sexo=="Hombre")

### generar el vector con los nombres de la foto

id_fotos_a<- NULL
id_fotos_b<- NULL
id_fotos_c<- NULL
id_fotos_d <- NULL

of_qa <- orden_fotos$q_a[1:16]
of_qb <- orden_fotos$q_b[1:16]
of_qc <- orden_fotos$q_c[1:16]
of_qd <- orden_fotos$q_d[1:17]

for (gg in of_qa){
  id_fotos_a <- paste0("id_",of_qa)
}

for (gg in of_qb){
  id_fotos_b <- paste0("id_",of_qb)
}

for (gg in of_qc){
  id_fotos_c <- paste0("id_",of_qc)
}

for (gg in of_qd){
  id_fotos_d <- paste0("id_",of_qd)
}
orden <- c(of_qa, of_qb, of_qc, of_qd)

id_fotos <- c(id_fotos_a, id_fotos_b, id_fotos_c, id_fotos_d)



##solo las preguntas sobre la percepcion del Q A
pp_qa <- base_a.2[,10:137]

names(pp_qa)

###Seleccionar los valores de atractivo del primer cuestionario

valor <- NULL
base_atra_a <- pp_qa[1]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.atractiva.te.parece.esta.persona..",ii))
  base_atra_a<-cbind(base_atra_a,pp_qa[valor])
  print(valor)
}

names(base_atra_a)<- id_fotos_a

###Seleccionar los valores de dominancia del primer cuestionario

valor <- NULL
base_dom_a <- pp_qa[2]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.dominante.te.parece.esta.persona..",ii))
  base_dom_a<-cbind(base_dom_a,pp_qa[valor])
  print(valor)
}

names(base_dom_a)<- id_fotos_a

###Seleccionar los valores de masculinidad del primer cuestionario

valor <- NULL
base_masc_a <- pp_qa[3]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.masculina.te.parece.esta.persona..",ii))
  base_masc_a<-cbind(base_masc_a,pp_qa[valor])
  print(valor)
}

names(base_masc_a)<- id_fotos_a

###Seleccionar los valores de confiabilidad del primer cuestionario

valor <- NULL
base_conf_a <- pp_qa[4]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.confiable.te.parece.esta.persona..",ii))
  base_conf_a<-cbind(base_conf_a,pp_qa[valor])
  print(valor)
}

names(base_conf_a)<- id_fotos_a

###Seleccionar los valores de agresividad del primer cuestionario

valor <- NULL
base_agre_a <- pp_qa[5]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.agresiva.te.parece.esta.persona..",ii))
  base_agre_a<-cbind(base_agre_a,pp_qa[valor])
  print(valor)
}

names(base_agre_a)<- id_fotos_a

###Seleccionar los valores de agresividad del primer cuestionario

valor <- NULL
base_fuert_a <- pp_qa[6]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.fuerte.te.parece.esta.persona..",ii))
  base_fuert_a<-cbind(base_fuert_a,pp_qa[valor])
  print(valor)
}

names(base_fuert_a)<- id_fotos_a

###Seleccionar los valores de percepcion salud del primer cuestionario

valor <- NULL
base_salud_a <- pp_qa[7]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tan.saludable.te.parece.esta.persona..",ii))
  base_salud_a<-cbind(base_salud_a,pp_qa[valor])
  print(valor)
}

names(base_salud_a)<- id_fotos_a

###Seleccionar los valores de percepcion de adiposidad facial del primer cuestionario

valor <- NULL
base_adipo_a <- pp_qa[8]

for (ii in 1:length(of_qa)){
  
  valor <- which(names(pp_qa)==paste0("Â.QuÃ..tanta.adiposidad.consideras.que.tiene.este.rostro..",ii))
  base_adipo_a<-cbind(base_adipo_a,pp_qa[valor])
  print(valor)
}

names(base_adipo_a)<- id_fotos_a

mean(base_adipo_a[1])

##### Obtener la media y la sd de atractivo

atra_a <- NULL
atra_sd_a<- NULL

for (hh in 1:length(base_atra_a)){
  valor<-mean(base_atra_a[,hh])
  valor_sd<-sd(base_atra_a[,hh])
  atra_a[hh]<- valor
  atra_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de dominancia

dom_a <- NULL
dom_sd_a<- NULL

for (hh in 1:length(base_dom_a)){
  valor<-mean(base_dom_a[,hh])
  valor_sd<-sd(base_dom_a[,hh])
  dom_a[hh]<- valor
  dom_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de masculinidad

masc_a <- NULL
masc_sd_a<- NULL

for (hh in 1:length(base_masc_a)){
  valor<-mean(base_masc_a[,hh])
  valor_sd<-sd(base_masc_a[,hh])
  masc_a[hh]<- valor
  masc_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de confiabilidad

conf_a <- NULL
conf_sd_a<- NULL

for (hh in 1:length(base_conf_a)){
  valor<-mean(base_conf_a[,hh])
  valor_sd<-sd(base_conf_a[,hh])
  conf_a[hh]<- valor
  conf_sd_a[hh]<-valor_sd
}


##### Obtener la media y la sd de agresividad

agre_a <- NULL
agre_sd_a<- NULL

for (hh in 1:length(base_agre_a)){
  valor<-mean(base_agre_a[,hh])
  valor_sd<-sd(base_agre_a[,hh])
  agre_a[hh]<- valor
  agre_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de fuerza

fuert_a <- NULL
fuert_sd_a<- NULL

for (hh in 1:length(base_fuert_a)){
  valor<-mean(base_fuert_a[,hh])
  valor_sd<-sd(base_fuert_a[,hh])
  fuert_a[hh]<- valor
  fuert_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de psalud

salud_a <- NULL
salud_sd_a<- NULL

for (hh in 1:length(base_salud_a)){
  valor<-mean(base_salud_a[,hh])
  valor_sd<-sd(base_salud_a[,hh])
  salud_a[hh]<- valor
  salud_sd_a[hh]<-valor_sd
}

##### Obtener la media y la sd de adiposidad facial

adipo_a <- NULL
adipo_sd_a<- NULL

for (hh in 1:length(base_adipo_a)){
  valor<-mean(base_adipo_a[,hh])
  valor_sd<-sd(base_adipo_a[,hh])
  adipo_a[hh]<- valor
  adipo_sd_a[hh]<-valor_sd
}

base_p_a <- data.frame(of_qa,id_fotos_a,
                       atra_a,atra_sd_a,
                       dom_a,dom_sd_a,
                       masc_a,masc_sd_a,
                       conf_a,conf_sd_a,
                       agre_a,agre_sd_a,
                       fuert_a,fuert_sd_a,
                       salud_a,salud_sd_a,
                       adipo_a,adipo_sd_a)

##########################################################################
####Cuestionario B

##base de datos de todo el cuestionario B
base_b <- read.csv("Cuestionario B.1 (Respuestas) - Respuestas de formulario 1.csv") 

##cargar info del orden de las fotos
orden_fotos <- read.csv("orden_fotos.csv")

## revisar si no hay personas que contestaron dos veces

which(summary(base_b$Crear.ID..CURP.)!=1)

##ninguno se repite

#### filtrar solo a las Hombre

base_b <-subset(base_b,base_b$Sexo=="Hombre")


##solo las preguntas sobre la percepcion del Q B
pp_qb <- base_b[,10:137]

names(pp_qb)

###Seleccionar los valores de atractivo del segundo cuestionario

valor <- NULL
base_atra_b <- pp_qb[1]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.atractiva.te.parece.esta.persona..",ii))
  base_atra_b<-cbind(base_atra_b,pp_qb[valor])
  print(valor)
}

names(base_atra_b)<- id_fotos_b

###Seleccionar los valores de dominancia del segundo cuestionario

valor <- NULL
base_dom_b <- pp_qb[2]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.dominante.te.parece.esta.persona..",ii))
  base_dom_b<-cbind(base_dom_b,pp_qb[valor])
  print(valor)
}

names(base_dom_b)<- id_fotos_b

###Seleccionar los valores de masculinidad del segundo cuestionario

valor <- NULL
base_masc_b <- pp_qb[3]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.masculina.te.parece.esta.persona..",ii))
  base_masc_b<-cbind(base_masc_b,pp_qb[valor])
  print(valor)
}

names(base_masc_b)<- id_fotos_b

###Seleccionar los valores de confiabilidad del segundo cuestionario

valor <- NULL
base_conf_b <- pp_qb[4]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.confiable.te.parece.esta.persona..",ii))
  base_conf_b<-cbind(base_conf_b,pp_qb[valor])
  print(valor)
}

names(base_conf_b)<- id_fotos_b

###Seleccionar los valores de agresividad del segundo cuestionario

valor <- NULL
base_agre_b <- pp_qb[5]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.agresiva.te.parece.esta.persona..",ii))
  base_agre_b<-cbind(base_agre_b,pp_qb[valor])
  print(valor)
}

names(base_agre_b)<- id_fotos_b

###Seleccionar los valores de agresividad del segundo cuestionario

valor <- NULL
base_fuert_b <- pp_qb[6]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.fuerte.te.parece.esta.persona..",ii))
  base_fuert_b<-cbind(base_fuert_b,pp_qb[valor])
  print(valor)
}

names(base_fuert_b)<- id_fotos_b

###Seleccionar los valores de percepcion salud del segundo cuestionario

valor <- NULL
base_salud_b <- pp_qb[7]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tan.saludable.te.parece.esta.persona..",ii))
  base_salud_b<-cbind(base_salud_b,pp_qb[valor])
  print(valor)
}

names(base_salud_b)<- id_fotos_b

###Seleccionar los valores de percepcion de adiposidad facial del segundo cuestionario

valor <- NULL
base_adipo_b <- pp_qb[8]

for (ii in 1:length(of_qb)){
  
  valor <- which(names(pp_qb)==paste0("Â.QuÃ..tanta.adiposidad.consideras.que.tiene.este.rostro..",ii))
  base_adipo_b<-cbind(base_adipo_b,pp_qb[valor])
  print(valor)
}

names(base_adipo_b)<- id_fotos_b

##### Obtener la media y la sd de atractivo

atra_b <- NULL
atra_sd_b<- NULL

for (hh in 1:length(base_atra_b)){
  valor<-mean(base_atra_b[,hh])
  valor_sd<-sd(base_atra_b[,hh])
  atra_b[hh]<- valor
  atra_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de dominancia

dom_b <- NULL
dom_sd_b<- NULL

for (hh in 1:length(base_dom_b)){
  valor<-mean(base_dom_b[,hh])
  valor_sd<-sd(base_dom_b[,hh])
  dom_b[hh]<- valor
  dom_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de masculinidad

masc_b <- NULL
masc_sd_b<- NULL

for (hh in 1:length(base_masc_b)){
  valor<-mean(base_masc_b[,hh])
  valor_sd<-sd(base_masc_b[,hh])
  masc_b[hh]<- valor
  masc_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de confiabilidad

conf_b <- NULL
conf_sd_b<- NULL

for (hh in 1:length(base_conf_b)){
  valor<-mean(base_conf_b[,hh])
  valor_sd<-sd(base_conf_b[,hh])
  conf_b[hh]<- valor
  conf_sd_b[hh]<-valor_sd
}


##### Obtener la media y la sd de agresividad

agre_b <- NULL
agre_sd_b<- NULL

for (hh in 1:length(base_agre_b)){
  valor<-mean(base_agre_b[,hh])
  valor_sd<-sd(base_agre_b[,hh])
  agre_b[hh]<- valor
  agre_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de fuerza

fuert_b <- NULL
fuert_sd_b<- NULL

for (hh in 1:length(base_fuert_b)){
  valor<-mean(base_fuert_b[,hh])
  valor_sd<-sd(base_fuert_b[,hh])
  fuert_b[hh]<- valor
  fuert_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de psalud

salud_b <- NULL
salud_sd_b<- NULL

for (hh in 1:length(base_salud_b)){
  valor<-mean(base_salud_b[,hh])
  valor_sd<-sd(base_salud_b[,hh])
  salud_b[hh]<- valor
  salud_sd_b[hh]<-valor_sd
}

##### Obtener la media y la sd de adiposidad facial

adipo_b <- NULL
adipo_sd_b<- NULL

for (hh in 1:length(base_adipo_b)){
  valor<-mean(base_adipo_b[,hh])
  valor_sd<-sd(base_adipo_b[,hh])
  adipo_b[hh]<- valor
  adipo_sd_b[hh]<-valor_sd
}

base_p_b <- data.frame(of_qb,id_fotos_b,
                       atra_b,atra_sd_b,
                       dom_b,dom_sd_b,
                       masc_b,masc_sd_b,
                       conf_b,conf_sd_b,
                       agre_b,agre_sd_b,
                       fuert_b,fuert_sd_b,
                       salud_b,salud_sd_b,
                       adipo_b,adipo_sd_b)

##########################################################################
####Cuestionario C

##base de datos de todo el cuestionario C
base_c <- read.csv("Cuestionario C.1 (Respuestas) - Respuestas de formulario 1.csv") 

##cargar info del orden de las fotos
orden_fotos <- read.csv("orden_fotos.csv")

## revisar si no hay personas que contestaron dos veces

which(summary(base_c$Crear.ID..CURP.)!=1)

subset(base_c,base_c$Crear.ID..CURP.=="LUCJ950727")

subset(base_c,base_c$Crear.ID..CURP.=="VIRL980614")

base_c.2 <- base_c [-c(46),]

#######filtrar por sexo Hombre

base_c.2 <-subset(base_c.2,base_c.2$Sexo=="Hombre")

##solo las preguntas sobre la percepcion del Q C
pp_qc <- base_c.2[,10:137]

names(pp_qc)

###Seleccionar los valores de atractivo del tercer cuestionario

valor <- NULL
base_atra_c <- pp_qc[1]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.atractiva.te.parece.esta.persona..",ii))
  base_atra_c<-cbind(base_atra_c,pp_qc[valor])
  print(valor)
}

names(base_atra_c)<- id_fotos_c

###Seleccionar los valores de dominancia del tercer cuestionario

valor <- NULL
base_dom_c <- pp_qc[2]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.dominante.te.parece.esta.persona..",ii))
  base_dom_c<-cbind(base_dom_c,pp_qc[valor])
  print(valor)
}

names(base_dom_c)<- id_fotos_c

###Seleccionar los valores de masculinidad del tercer cuestionario

valor <- NULL
base_masc_c <- pp_qc[3]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.masculina.te.parece.esta.persona..",ii))
  base_masc_c<-cbind(base_masc_c,pp_qc[valor])
  print(valor)
}

names(base_masc_c)<- id_fotos_c

###Seleccionar los valores de confiabilidad del tercer cuestionario

valor <- NULL
base_conf_c <- pp_qc[4]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.confiable.te.parece.esta.persona..",ii))
  base_conf_c<-cbind(base_conf_c,pp_qc[valor])
  print(valor)
}

names(base_conf_c)<- id_fotos_c

###Seleccionar los valores de agresividad del tercer cuestionario

valor <- NULL
base_agre_c <- pp_qc[5]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.agresiva.te.parece.esta.persona..",ii))
  base_agre_c<-cbind(base_agre_c,pp_qc[valor])
  print(valor)
}

names(base_agre_c)<- id_fotos_c

###Seleccionar los valores de agresividad del tercer cuestionario

valor <- NULL
base_fuert_c <- pp_qc[6]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.fuerte.te.parece.esta.persona..",ii))
  base_fuert_c<-cbind(base_fuert_c,pp_qc[valor])
  print(valor)
}

names(base_fuert_c)<- id_fotos_c

###Seleccionar los valores de percepcion salud del tercer cuestionario

valor <- NULL
base_salud_c <- pp_qc[7]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tan.saludable.te.parece.esta.persona..",ii))
  base_salud_c<-cbind(base_salud_c,pp_qc[valor])
  print(valor)
}

names(base_salud_c)<- id_fotos_c

###Seleccionar los valores de percepcion de adiposidad facial del tercer cuestionario

valor <- NULL
base_adipo_c <- pp_qc[8]

for (ii in 1:length(of_qc)){
  
  valor <- which(names(pp_qc)==paste0("Â.QuÃ..tanta.adiposidad.consideras.que.tiene.este.rostro..",ii))
  base_adipo_c<-cbind(base_adipo_c,pp_qc[valor])
  print(valor)
}

names(base_adipo_c)<- id_fotos_c

##### Obtener la media y la sd de atractivo

atra_c <- NULL
atra_sd_c<- NULL

for (hh in 1:length(base_atra_c)){
  valor<-mean(base_atra_c[,hh])
  valor_sd<-sd(base_atra_c[,hh])
  atra_c[hh]<- valor
  atra_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de dominancia

dom_c <- NULL
dom_sd_c<- NULL

for (hh in 1:length(base_dom_c)){
  valor<-mean(base_dom_c[,hh])
  valor_sd<-sd(base_dom_c[,hh])
  dom_c[hh]<- valor
  dom_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de masculinidad

masc_c <- NULL
masc_sd_c<- NULL

for (hh in 1:length(base_masc_c)){
  valor<-mean(base_masc_c[,hh])
  valor_sd<-sd(base_masc_c[,hh])
  masc_c[hh]<- valor
  masc_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de confiabilidad

conf_c <- NULL
conf_sd_c<- NULL

for (hh in 1:length(base_conf_c)){
  valor<-mean(base_conf_c[,hh])
  valor_sd<-sd(base_conf_c[,hh])
  conf_c[hh]<- valor
  conf_sd_c[hh]<-valor_sd
}


##### Obtener la media y la sd de agresividad

agre_c <- NULL
agre_sd_c<- NULL

for (hh in 1:length(base_agre_c)){
  valor<-mean(base_agre_c[,hh])
  valor_sd<-sd(base_agre_c[,hh])
  agre_c[hh]<- valor
  agre_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de fuerza

fuert_c <- NULL
fuert_sd_c<- NULL

for (hh in 1:length(base_fuert_c)){
  valor<-mean(base_fuert_c[,hh])
  valor_sd<-sd(base_fuert_c[,hh])
  fuert_c[hh]<- valor
  fuert_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de psalud

salud_c <- NULL
salud_sd_c<- NULL

for (hh in 1:length(base_salud_c)){
  valor<-mean(base_salud_c[,hh])
  valor_sd<-sd(base_salud_c[,hh])
  salud_c[hh]<- valor
  salud_sd_c[hh]<-valor_sd
}

##### Obtener la media y la sd de adiposidad facial

adipo_c <- NULL
adipo_sd_c<- NULL

for (hh in 1:length(base_adipo_c)){
  valor<-mean(base_adipo_c[,hh])
  valor_sd<-sd(base_adipo_c[,hh])
  adipo_c[hh]<- valor
  adipo_sd_c[hh]<-valor_sd
}

base_p_c <- data.frame(of_qc,id_fotos_c,
                       atra_c,atra_sd_c,
                       dom_c,dom_sd_c,
                       masc_c,masc_sd_c,
                       conf_c,conf_sd_c,
                       agre_c,agre_sd_c,
                       fuert_c,fuert_sd_c,
                       salud_c,salud_sd_c,
                       adipo_c,adipo_sd_c)
##########################################################################
####Cuestionario D

##base de datos de todo el cuestionario D
base_d <- read.csv("Cuestionario D (Respuestas) - Respuestas de formulario 1.csv") 

##cargar info del orden de las fotos
orden_fotos <- read.csv("orden_fotos.csv")

## revisar si no hay personas que contestaron dos veces

which(summary(base_d$Crear.ID..CURP.)!=1)

subset(base_d,base_d$Crear.ID..CURP.=="MSTA810302")

subset(base_d,base_d$Crear.ID..CURP.=="PATJ890312")

base_d.2 <- base_d [-c(115,56),]

############Filtrar por sexo Hombre

base_d.2 <-subset(base_d.2,base_d.2$Sexo=="Hombre")

##solo las preguntas sobre la percepcion del Q D
pp_qd <- base_d.2[,10:145]

names(pp_qd)

###Seleccionar los valores de atractivo del cuarto cuestionario

valor <- NULL
base_atra_d <- pp_qd[1]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.atractiva.te.parece.esta.persona..",ii))
  base_atra_d<-cbind(base_atra_d,pp_qd[valor])
  print(valor)
}

names(base_atra_d)<- id_fotos_d

###Seleccionar los valores de dominancia del cuarto cuestionario

valor <- NULL
base_dom_d <- pp_qd[2]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.dominante.te.parece.esta.persona..",ii))
  base_dom_d<-cbind(base_dom_d,pp_qd[valor])
  print(valor)
}

names(base_dom_d)<- id_fotos_d

###Seleccionar los valores de masculinidad del cuarto cuestionario

valor <- NULL
base_masc_d <- pp_qd[3]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.masculina.te.parece.esta.persona..",ii))
  base_masc_d<-cbind(base_masc_d,pp_qd[valor])
  print(valor)
}

names(base_masc_d)<- id_fotos_d

###Seleccionar los valores de confiabilidad del cuarto cuestionario

valor <- NULL
base_conf_d <- pp_qd[4]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.confiable.te.parece.esta.persona..",ii))
  base_conf_d<-cbind(base_conf_d,pp_qd[valor])
  print(valor)
}

names(base_conf_d)<- id_fotos_d

###Seleccionar los valores de agresividad del cuarto cuestionario

valor <- NULL
base_agre_d <- pp_qd[5]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.agresiva.te.parece.esta.persona..",ii))
  base_agre_d<-cbind(base_agre_d,pp_qd[valor])
  print(valor)
}

names(base_agre_d)<- id_fotos_d

###Seleccionar los valores de agresividad del cuarto cuestionario

valor <- NULL
base_fuert_d <- pp_qd[6]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.fuerte.te.parece.esta.persona..",ii))
  base_fuert_d<-cbind(base_fuert_d,pp_qd[valor])
  print(valor)
}

names(base_fuert_d)<- id_fotos_d

###Seleccionar los valores de percepcion salud del cuarto cuestionario

valor <- NULL
base_salud_d <- pp_qd[7]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tan.saludable.te.parece.esta.persona..",ii))
  base_salud_d<-cbind(base_salud_d,pp_qd[valor])
  print(valor)
}

names(base_salud_d)<- id_fotos_d

###Seleccionar los valores de percepcion de adiposidad facial del cuarto cuestionario

valor <- NULL
base_adipo_d <- pp_qd[8]

for (ii in 1:length(of_qd)){
  
  valor <- which(names(pp_qd)==paste0("Â.QuÃ..tanta.adiposidad.consideras.que.tiene.este.rostro..",ii))
  base_adipo_d<-cbind(base_adipo_d,pp_qd[valor])
  print(valor)
}

names(base_adipo_d)<- id_fotos_d

##### Obtener la media y la sd de atractivo

atra_d <- NULL
atra_sd_d<- NULL

for (hh in 1:length(base_atra_d)){
  valor<-mean(base_atra_d[,hh])
  valor_sd<-sd(base_atra_d[,hh])
  atra_d[hh]<- valor
  atra_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de dominancia

dom_d <- NULL
dom_sd_d<- NULL

for (hh in 1:length(base_dom_d)){
  valor<-mean(base_dom_d[,hh])
  valor_sd<-sd(base_dom_d[,hh])
  dom_d[hh]<- valor
  dom_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de masculinidad

masc_d <- NULL
masc_sd_d<- NULL

for (hh in 1:length(base_masc_d)){
  valor<-mean(base_masc_d[,hh])
  valor_sd<-sd(base_masc_d[,hh])
  masc_d[hh]<- valor
  masc_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de confiabilidad

conf_d <- NULL
conf_sd_d<- NULL

for (hh in 1:length(base_conf_d)){
  valor<-mean(base_conf_d[,hh])
  valor_sd<-sd(base_conf_d[,hh])
  conf_d[hh]<- valor
  conf_sd_d[hh]<-valor_sd
}


##### Obtener la media y la sd de agresividad

agre_d <- NULL
agre_sd_d<- NULL

for (hh in 1:length(base_agre_d)){
  valor<-mean(base_agre_d[,hh])
  valor_sd<-sd(base_agre_d[,hh])
  agre_d[hh]<- valor
  agre_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de fuerza

fuert_d <- NULL
fuert_sd_d<- NULL

for (hh in 1:length(base_fuert_d)){
  valor<-mean(base_fuert_d[,hh])
  valor_sd<-sd(base_fuert_d[,hh])
  fuert_d[hh]<- valor
  fuert_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de psalud

salud_d <- NULL
salud_sd_d<- NULL

for (hh in 1:length(base_salud_d)){
  valor<-mean(base_salud_d[,hh])
  valor_sd<-sd(base_salud_d[,hh])
  salud_d[hh]<- valor
  salud_sd_d[hh]<-valor_sd
}

##### Obtener la media y la sd de adiposidad facial

adipo_d <- NULL
adipo_sd_d<- NULL

for (hh in 1:length(base_adipo_d)){
  valor<-mean(base_adipo_d[,hh])
  valor_sd<-sd(base_adipo_d[,hh])
  adipo_d[hh]<- valor
  adipo_sd_d[hh]<-valor_sd
}

base_p_d <- data.frame(of_qd,id_fotos_d,
                       atra_d,atra_sd_d,
                       dom_d,dom_sd_d,
                       masc_d,masc_sd_d,
                       conf_d,conf_sd_d,
                       agre_d,agre_sd_d,
                       fuert_d,fuert_sd_d,
                       salud_d,salud_sd_d,
                       adipo_d,adipo_sd_d)
## Crear una base de datos para vaciar los valores de percepción

atra<-NA
dom<-NA
masc<-NA
conf<-NA
agre<-NA
fuert<-NA
salud<-NA
adipo<-NA

atra_sd<-NA
dom_sd<-NA
masc_sd<-NA
conf_sd<-NA
agre_sd<-NA
fuert_sd<-NA
salud_sd<-NA
adipo_sd<-NA

atra[1:65]<- c(base_p_a[,3],base_p_b[,3],base_p_c[,3],base_p_d[,3])
dom[1:65]<- c(base_p_a[,5],base_p_b[,5],base_p_c[,5],base_p_d[,5])
masc[1:65]<- c(base_p_a[,7],base_p_b[,7],base_p_c[,7],base_p_d[,7])
conf[1:65]<- c(base_p_a[,9],base_p_b[,9],base_p_c[,9],base_p_d[,9])
agre[1:65]<- c(base_p_a[,11],base_p_b[,11],base_p_c[,11],base_p_d[,11])
fuert[1:65]<- c(base_p_a[,13],base_p_b[,13],base_p_c[,13],base_p_d[,13])
salud[1:65]<- c(base_p_a[,15],base_p_b[,15],base_p_c[,15],base_p_d[,15])
adipo[1:65]<- c(base_p_a[,17],base_p_b[,17],base_p_c[,17],base_p_d[,17])

atra_sd[1:65]<- c(base_p_a[,4],base_p_b[,4],base_p_c[,4],base_p_d[,4])
dom_sd[1:65]<- c(base_p_a[,6],base_p_b[,6],base_p_c[,6],base_p_d[,6])
masc_sd[1:65]<- c(base_p_a[,8],base_p_b[,8],base_p_c[,8],base_p_d[,8])
conf_sd[1:65]<- c(base_p_a[,10],base_p_b[,10],base_p_c[,10],base_p_d[,10])
agre_sd[1:65]<- c(base_p_a[,12],base_p_b[,12],base_p_c[,12],base_p_d[,12])
fuert_sd[1:65]<- c(base_p_a[,14],base_p_b[,14],base_p_c[,14],base_p_d[,14])
salud_sd[1:65]<- c(base_p_a[,16],base_p_b[,16],base_p_c[,16],base_p_d[,16])
adipo_sd[1:65]<- c(base_p_a[,18],base_p_b[,18],base_p_c[,18],base_p_d[,18])

datos_percep <- data.frame(orden,id_fotos,
                           atra,atra_sd,
                           dom,dom_sd,
                           masc,masc_sd,
                           conf,conf_sd,
                           agre,agre_sd,
                           fuert,fuert_sd,
                           salud,salud_sd,
                           adipo,adipo_sd)
########## acomodar los valores en orden de sujetios

datos_percep <-datos_percep[order(datos_percep$orden),]

base_ciudad_montana <- read.csv("Base de datos Testo y CSS_Ciudad_Montaña_2018.csv")

base_completa_h <- cbind(base_ciudad_montana,datos_percep)


