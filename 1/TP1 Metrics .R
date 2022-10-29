library("writexl")
library("readxl")

#Ejercicio 1

#1

#Creo lista con dataframes, cada uno con 4 columnas: x1, x2, x3 y u. Con las características que pide el tp.
set.seed(1)
lista_data = replicate(n = 50,
                       expr = {data.frame(x1 = runif(100,0,100),
                                          x2 = runif(100,0,100), 
                                          x3 = runif(100,0,100), 
                                          u = rnorm(100,0,sqrt(1400)))},
                       simplify = F)

#Hago un for loop para agregarle a cada dataframe la columna de Y
for (i in 1:50) {
  lista_data[[i]]["Y"] = 400 + 2*lista_data[[i]]["x1"]+ 2*lista_data[[i]]["x2"] -2*lista_data[[i]]["x3"] + lista_data[[i]]["u"]
}
rm(i)

#2) 

#Hago la matriz de correlaciones entre las variables independientes.
round(cor(lista_data[[1]][1:4]),2)

#3) 
#Hago una lista con todas las regresiones
lista_reg = list()
for (i in 1:50) {
  lista_reg[[i]] = lm(lista_data[[i]]$Y~lista_data[[i]]$x1+lista_data[[i]]$x2+lista_data[[i]]$x3)
}
rm(i)

#Hago una lista con solo los coeficientes de cada regresión
lista_coef = list()
for (i in 1:50) { 
  lista_coef[[i]] = coef(lista_reg[[i]])
}
rm(i)

#Paso los coeficientes a dataframe para poder pasarlo a excel
df_regresiones <- data.frame(matrix(unlist(lista_coef), nrow=length(lista_coef), byrow=TRUE))

#Le cambio las columnas para que tengan sentido en el excel
colnames(df_regresiones) = c("B0","B1","B2","B3")

#Creo el excel, fijense que ustedes tienen que cambiarle la ruta del archivo para que tenga sentido
write_xlsx(df_regresiones,"C:\\Users\\felip\\Documents\\UdeSA\\Maestría\\Econometría avanzada\\TPs\\primera_estimación.xlsx")

#4)
#Cargo el excel
coefs <- read_excel("UdeSA/Maestría/Econometría avanzada/TPs/primera_estimación.xlsx")

#Hago gráficos con los coeficientes
plot(coefs$B1,coefs$B2,xlab = "Estimador β MCO 1", ylab = "Estimador β MCO 2", main = "corr β MCO 1 - β MCO 2")
plot(coefs$B1,coefs$B3, xlab = "Estimador β MCO 1", ylab = "Estimador β MCO 3", main = "corr β MCO 1 - β MCO 3")

sd(coefs$B0)
mean(coefs$B0)
sd(coefs$B1)
mean(coefs$B1)
sd(coefs$B2)
mean(coefs$B2)
sd(coefs$B3)
mean(coefs$B3)

#5)
set.seed(1)
lista_data2 = replicate(n = 50,
                       expr = {data.frame(x1 = runif(100,0,100),
                                          x2 = runif(100,0,100), 
                                          x3 = runif(100,0,100), 
                                          u = rnorm(100,0,sqrt(1400)))},
                       simplify = F)

for (i in 1:50) {
  set.seed(1)
  x2 <- scale(matrix( rnorm(100), ncol=1 ))
  x1 = lista_data2[[1]]["x1"]
  x1 = unlist(x1)
  xs <- cbind(scale(x1),x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(
    c(1 , 0.987,
      0.987, 1 ), ncol=2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx %*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
  lista_data2[[i]]["x2"] = x2
}
rm(i)

#6)
for (i in 1:50) {
  lista_data2[[i]]["Y"] = 400 + 2*lista_data2[[i]]["x1"]+ 2*lista_data2[[i]]["x2"] -2*lista_data2[[i]]["x3"] + lista_data2[[i]]["u"]
}
rm(i)

#Hago una lista con todas las regresiones
lista_reg2 = list()
for (i in 1:50) {
  lista_reg2[[i]] = lm(lista_data2[[i]]$Y~lista_data2[[i]]$x1+lista_data2[[i]]$x2+lista_data2[[i]]$x3)
}
rm(i)

#Hago una lista con solo los coeficientes de cada regresión
lista_coef2 = list()
for (i in 1:50) { 
  lista_coef2[[i]] = coef(lista_reg2[[i]])
}
rm(i)

#Paso los coeficientes a dataframe para poder pasarlo a excel
df_regresiones2 <- data.frame(matrix(unlist(lista_coef2), nrow=length(lista_coef2), byrow=TRUE))

#Le cambio las columnas para que tengan sentido en el excel
colnames(df_regresiones2) = c("B0","B1","B2","B3")

#Creo el excel, fijense que ustedes tienen que cambiarle la ruta del archivo para que tenga sentido
write_xlsx(df_regresiones2,"C:\\Users\\felip\\Documents\\UdeSA\\Maestría\\Econometría avanzada\\TPs\\segunda_estimación.xlsx")

#7)

#Cargo el excel
coefs <- read_excel("UdeSA/Maestría/Econometría avanzada/TPs/primera_estimación.xlsx")
coefs2 <- read_excel("UdeSA/Maestría/Econometría avanzada/TPs/segunda_estimación.xlsx")

#Hago gráficos con los coeficientes
par(mfrow=c(1,2))
plot(coefs$B1,coefs$B2,xlab = "Estimador X1", ylab = "Estimador X2", main = "Normal")
plot(coefs2$B1,coefs2$B2,xlab = "Estimador X1", ylab = "Estimador X2", main = "Corr alta")

#Ejercicio 2
#1 

#Hago una lista con todas las regresiones
lista_reg3 = list()
for (i in 1:50) {
  lista_reg3[[i]] = lm(lista_data[[i]]$Y~lista_data[[i]]$x1+lista_data[[i]]$x3)
}
rm(i)

#Hago una lista con solo los coeficientes de cada regresión
lista_coef3 = list()
for (i in 1:50) { 
  lista_coef3[[i]] = coef(lista_reg3[[i]])
}
rm(i)

#Paso los coeficientes a dataframe para poder pasarlo a excel
df_regresiones3 <- data.frame(matrix(unlist(lista_coef3), nrow=length(lista_coef3), byrow=TRUE))

#Le cambio las columnas para que tengan sentido en el excel
colnames(df_regresiones3) = c("B0","B1","B3")

#Creo el excel, fijense que ustedes tienen que cambiarle la ruta del archivo para que tenga sentido
write_xlsx(df_regresiones3,"C:\\Users\\felip\\Documents\\UdeSA\\Maestría\\Econometría avanzada\\TPs\\tercera_estimación.xlsx")

coefs3 = read_excel("UdeSA/Maestría/Econometría avanzada/TPs/tercera_estimación.xlsx")

par(mfrow=c(1,1))
plot(coefs3$B1,coefs3$B3, xlab = "Estimador X1", ylab = "Estimador X3", main = "Ejercicio 2")

#Hago una lista con todas las regresiones
lista_reg4 = list()
for (i in 1:50) {
  lista_reg4[[i]] = lm(lista_data2[[i]]$Y~lista_data2[[i]]$x1+lista_data2[[i]]$x3)
}
rm(i)

#Hago una lista con solo los coeficientes de cada regresión
lista_coef4 = list()
for (i in 1:50) { 
  lista_coef4[[i]] = coef(lista_reg4[[i]])
}
rm(i)

#Paso los coeficientes a dataframe para poder pasarlo a excel
df_regresiones4 <- data.frame(matrix(unlist(lista_coef4), nrow=length(lista_coef4), byrow=TRUE))

#Le cambio las columnas para que tengan sentido en el excel
colnames(df_regresiones4) = c("B0","B1","B3")

#Creo el excel, fijense que ustedes tienen que cambiarle la ruta del archivo para que tenga sentido
write_xlsx(df_regresiones4,"C:\\Users\\felip\\Documents\\UdeSA\\Maestría\\Econometría avanzada\\TPs\\cuarta_estimación.xlsx")

coefs4 = read_excel("UdeSA/Maestría/Econometría avanzada/TPs/cuarta_estimación.xlsx")

par(mfrow=c(1,2))
plot(coefs3$B1,coefs3$B3, xlab = "Estimador X1", ylab = "Estimador X3", main = "Normal")
plot(coefs4$B1,coefs4$B3, xlab = "Estimador X1", ylab = "Estimador X3", main = "Corr alta")
