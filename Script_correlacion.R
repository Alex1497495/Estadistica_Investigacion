# Importar datos 

datos <-read.csv("bosque.csv" , header =T)

#Distribucion de pares de datos
plot(datos$diametro, datos$altura,
     pch =19, col = "blue",
     xlab = "variable independiente (dbh)",
     ylab = "variable dependiente (h)")

#Prueba de normalidad y boxplot de la variables 
shapiro.test(datos$diametro)
boxplot(datos$diametro)
hist(datos$diametro)

shapiro.test(datos$altura)
boxplot(datos$altura)
hist(datos$altura)

datos$dbh_log <- log10(datos$diametro)
datos$dbh_sqr <-sqrt(datos$diametro)


hist(datos$dbh_log)
shapiro.test(datos$dbh_log)
shapiro.test(datos$dbh_sqr)
hist(datos$dbh_sqr)

#prueba de correlación no parametrica

cor.test(datos$diametro, datos$altura, method = "pearson")
