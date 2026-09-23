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

# Datos de Gesiser 

# Se toma en cuenta en la correalacion, el tiempo 

# Faithful geyser 

geyser <- faithful

fivenum(geyser$eruptions)
fivenum(geyser$waiting)

mean(geyser$eruptions)
mean(geyser$waiting)

plot(geyser$waiting, geyser$eruptions,
     pch = 19, col = "red",
     xlab = "Tiempo de espera (min)",
     ylab = "Erupcion(min)")

cor.test(geyser$waiting, geyser$eruptions)

g.lm <- lm(geyser$eruption ~ geyser$waiting)
summary(g.lm)

# adjusted r-squared 0.81 representa la varianza

plot(geyser$waiting, geyser$eruptions,
     pch = 19, col = "blue",
     xlab = "Tiempo de espera (min)",
     ylab = "Erupcion(min)")
abline(g.lm,
       col = "red",
       lwd = "2")

g.lm$coefficients
g.lm$residuals

# 286.478 es mayor que 270

anova(g.lm)

geyser$yp <-g.lm$fitted.values
geyser$res <- g.lm$residuals
sum(geyser$res) 
geyser$modelo <- -1.874016 + 0.07563*geyser$waiting

tiem <- c(45, 55, 68)
-1.874016 + 0.07563 * tiem
 
#termino
