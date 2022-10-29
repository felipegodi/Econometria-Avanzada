#install.packages("stargazer")
#install.packages("plm")
#install.packages("Formula")
#install.packages("plyr")
#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(plyr)
library(stargazer)
library(plm)
library(Formula)

data = read_csv("cornwell.csv")

#Renombro las columnas para que coincidan con las del paper
data2 = plyr::rename(data, c("prbarr" = "Pa", 
                         "prbconv" = "Pc",
                         "prbpris" = "Pp",
                         "avgsen" = "S",
                         "polpc" = "police",
                         "pctymle" = "pym",
                         "pctmin80" = "pm"))

#Establezco el index en el año y county
data2 = pdata.frame(data2, index = c("county","year"))

#Defino el modelo
modelo = Formula(log(crmrte) ~ log(Pa) + log(Pc) + log(Pp) + log(S) + log(police) + log(density) + log(pym) + log(wcon)
                  + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + 
                    west + central + urban + log(pm))

#Corro la regresión con between
between = plm(modelo, data = data2, model = "between") 

#Exporto la tabla de regresión
stargazer(between,  
          type="latex",
          dep.var.labels=c("Crime rate"),
          out="between")

#Corro la regresión con within
within = plm(modelo, data = data2, model = "within")

#exporto la tabla de regresión
stargazer(within,  
          type="latex",
          dep.var.labels=c("Crime rate"),
          out="within")

#Corremos la regresión OLS
ols = lm(modelo, data = data2)

# F test for individual effects
pFtest(within, ols)

#Corro la regresión con random
random = plm(modelo, data = data2, model = "random")

#Test de hausman
phtest(within,random)

#Todas las regresiones juntas
stargazer(between, within, random,  
          type="latex",
          dep.var.labels=c("Crime rate"),
          out="regresiones")

# Test conjuntos de correlación serial y efectos aletorios
pbsytest(modelo, data = data2, test = "J")
pbsytest(modelo, data = data2, test = "ar")
pbsytest(modelo, data = data2, test = "re")

# Test robusto local para correlación serial o Efectos Aleatorios
pbsytest(modelo, data = data2)

# Test robusto local para correlación serial o Efectos Aleatorios
pbsytest(modelo, data = data2, test = "RE")

# Test condicional LM para errores AR(1) o MA(1) bajo Efectos Aleatorios
pbltest(log(crmrte) ~ log(Pa) + log(Pc) + log(Pp) + log(S) +
          log(police) + log(density) + log(pym) + log(wcon)
        + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg)
        + log(wfed) + log(wsta) + log(wloc) +
          west + central + urban + log(pm), data = data2,
        alternative = "onesided")

# Test de correlación serial general
pbgtest(random)