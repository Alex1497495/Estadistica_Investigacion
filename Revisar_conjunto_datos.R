# Alejandro
# 1497495
# 18/08/2026

# Importar datos----
# Funcion read.csv importa datos de excel a R

IE<- read.csv("Datos/crudos/vivero.csv", header =T)
View(IE)
IE$Tratamiento <- as.factor(IE$Tratamiento)

IE$planta
IE$IE
IE$Tratamiento

# Revisar solo una porcion de datos
head(IE) # Primeras filas de la BD IE
tail(IE) # Ultimas filas de BD IE


# Graficar

hist(IE$IE,
     main= " ",
     xlab = "Indice",
     ylab = "Frecuencia",
     ylim = c(0,12),
     col = "brown")

# Normalidad datos ----
shapiro.test(IE$IE)

mean(IE$IE)
sd(IE$IE)
var(IE$IE)
fivenum(IE$IE)

boxplot(IE$IE~IE$Tratamiento,
        col = "blue",
        xlab = "Tratamientos", 
        Ylab = "indice")

bartlett.test(IE$IE ~ IE$Tratamiento)
