#         TP3
setwd("~/UdeSA/Maestría/Econometría avanzada/TPs/3")

library(haven) # para abrir bases de datos en formato .dta
library(stargazer) # para tablas 
library(tidyverse) # librería muy usada para manipular los datos y hacer gráficos
library(AER) # para usar el comando de variables instrumentales

data <- read_dta("qob.dta")

#A)

promedios <- data %>% summarise_all(mean)
promedios
minimos <- data %>% summarise_all(min)
minimos
maximos <- data %>% summarise_all(max)
maximos

#Como el minimo año de nacimiento es 30 y el maximo es 49 no eliminamos nada
aggregate(data$educ, by=list(data$qob), mean)


#B) 

data2 <- subset(data, yob>= 30 & yob <= 39) 


attach(data2)

lm.fit <- lm(lwage ~ educ + ageq + ageqsq + race +smsa + married) 
summary(lm.fit)
stargazer(lm.fit,  
          type="latex",
          dep.var.labels=c("Sales"), 
          out="lm.fit")

#C)

residuos = lm.fit$residuals
cov(residuos, data2$qob)
cov(data2$educ, data2$qob)

#D)

data2$z1 <- ifelse(data2$qob==1,1,0)
data2$z2 <- ifelse(data2$qob==2,1,0)
data2$z3 <- ifelse(data2$qob==3,1,0)

#E)

#Abi uso el comando linealHypothesis, ni idea
cor(data2$educ, data2$z1)
cor(data2$educ, data2$z2)
cor(data2$educ, data2$z3)


#F)
#First stage: 
#regresion de la variable endogena en los instrumentos y en las variables del modelo:
attach(data2)
first.stage <- lm(educ ~ ageq + ageqsq + race + married + smsa + z1 + z2 + z3) 
summary(first.stage)
stargazer(first.stage,  
          type="latex",
          dep.var.labels=c("First"), 
          out="first.stage")
data2$lwage_hat <- first.stage$fitted.values #me quedo con la proyeccion ortogonal 

#G) 
var.test(educ,z1, alternative="two.sided")
var.test(educ,z2, alternative="two.sided")
var.test(educ,z3, alternative="two.sided")

#H)
iv.fit <- ivreg(lwage ~ educ + ageq + ageqsq + race +smsa + married | #barra y del lado derecho pongo los instrumentos
                  ageq + ageqsq + race +smsa + married  + z1 + z2 + z3, # instrumentos (cada variable exógena es su propio instrumento)
                data = data2) 
summary(iv.fit) 
stargazer(iv.fit,  
          type="latex",
          dep.var.labels=c("iv"), 
          out="iv.fit")