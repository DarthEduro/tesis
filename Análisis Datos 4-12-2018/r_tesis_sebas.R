###Primeros analisis de base de datos Hormonas- 16 marzo 18

data <- read.table("Base de datos Testo y CSS_Ciudad_Montaña_2018.csv", sep=",", header=T)
names(data) <- c("code","nombre","edad","sexo","altura",
                 "peso","grasa","musculo","gra_vic","cintura",
                 "cadera","hombro","_hom", "cin_cad","hom_cin",
                 "imc","etnia","comunidad","lugar","test", 
                 "comida", "periodon", "enferm","med","cuest",
                 "q1", "q2", "q3","q4","hora")

data <- transform(data,med=factor(med,labels=c("no","si")))
data <- transform(data,enferm=factor(enferm,labels=c("no","si")))

str(data)

summary(data)

attach(data)
detach(data)
##variables transformadas##

ltest <- log(test)
imc2 <- imc*imc
grasa2 <- grasa*grasa
peso2 <- peso*peso
altura2 <- altura*altura

####Analisis exploratorio de la relació de la testosterona con las diferentes variables###

plot(test~comunidad+etnia+edad+imc+imc2+grasa+grasa2+altura+altura2+peso+peso2+comida+periodon+enferm+med+hora)


####Relacion de la test con comunidad/etnia*edad###

m1 <- lm(test~etnia*edad)
m1
summary(m1)

m2 <- lm(test~lugar)
m2
summary(m2)

###Relacion de caracteres fisicos (imc/cintura-cadera/hombro) con test##

c1 <- lm(imc~test*edad*comunidad)
c1
summary(c1)

c2 <- lm(cin_cad~test*edad*comunidad)
c2
summary(c2)

c3 <- lm(hom_cin~test*edad*comunidad)
c3
summary(c3)

###RElacion entre % musculo y test/imc

p1 <- lm(musculo~imc2)
p1
summary(p1)

p2 <- lm(musculo~ltest)
p2
summary(p2)

###modelo robusto de la variable test (log)

t1 <- lm(ltest~comunidad+edad+enferm+periodon+comida+
           hora+hora*comida+comunidad*edad)

summary(t1)

t2 <- lm(ltest~comunidad+edad+enferm+comida+
           hora+hora*comida+comunidad*edad)

summary(t2)

anova(t1,t2)

t3 <- lm(ltest~comunidad+edad+comida+hora+hora*comida+
           comunidad*edad) 

summary(t3)

t4 <- lm(ltest~comunidad+edad+comunidad*edad) 

summary(t4)





detach(data)



