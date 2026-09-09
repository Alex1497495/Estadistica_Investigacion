#Alejandro Solis Sanchez
# Matricula 1497495

# Ejercicio 3.1 
# Arboles

#URL de los datos 
url<-paste0( 
  "https://raw.githubusercontent.com/mgtagle/", 
  "Estadistica_Investigacion_2026/refs/heads/main/", 
  "Datos/Crudos/cuadro1.csv" 
  ) 

#Importar los datos directamente desde GitHub 
cuadro1<-read.csv(url,header=TRUE) 

#Revisar los datos 
head(cuadro1) 
tail(cuadro1)
str(cuadro1)

# Hipotesis nula (Ho): No se presentan diferencias en los diametros medios
# de los arboles con respecto a la medida establecida de 16.5 cm
# Hipotesis alternativa (H1): Se prresentan diferencias en los diametros 
# medios de los arboles de 16.5 cm

# Requiere una prueba de una cola o de dos colas? De dos colas ya que sirven 
# para determinar si el valor de la media es diferente de un valor de referencia
# que en este caso es de 16.5 cm

# Calcule la media de la variable Diametro.

mean(cuadro1$Diametro)

# calcule la desviación estándar de la variable Diametro

sd(cuadro1$Diametro)


# Realice la pruba T de una muestra
boxplot(cuadro1$Diametro)
t.test(cuadro1$Diametro, mu = 16.5, alternative = "two.sided")

# Identifique en los resultados: 
  # • estadístico de t;     = -1.547
  # • grados de libertad;   = 49
  # • valor de p:           = 0.1283
  # • media de la muestra;  = 15.794
  # • intervalo de confianza del 95%.  = 14.87689 a 16.71111

# Con α =0,05, indique si se rechaza o no se rechaza H0. No se rechaza
# debido que el valor de p-value 0.1283 es mayor al 0.05

# Redacte una conclusión en términos del diámetro medio de los árboles del rodal. 
# Con los resultados de la prueba t se demostro que no se rechaza la (HO) 
# ademas se comprobó que el valor del diametro medio de 16.5 cm entra en los 
# intervalos de confianza.


# Preguntas de reflexión 
# El intervalo de confianza del 95% contiene el valor de referencia de 16.5 cm? 
# Si, se confirmo que el valor entra dentro de los intervalos de confinza
  
# ¿Cómo se relaciona este resultado con la decisión obtenida 
# mediante el valor de p?
# se relaciona ya que el valor de p value es 0.1283 es mayor que 0.05, esto 
# coincide con el intervalo de confianza, que contiene el valor de referencia 
# de 16.5 cm




#Ejercicio 3.2
# Costales 



costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5,  77.87, 
            81.94, 80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 
            81.4, 74.89, 82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 
            78.44, 79.01, 80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 
            82.45, 77.29, 75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

# Explique con sus propias palabras qué representan H0 y H1.
# La hipotesis nula (H0) representa que los costales pesan 80 kg mientras que 
# la hipotesis alternativa (H1) indica que los costales pesarian menos de 80 kg

# ¿Por qué corresponde utilizar una prueba de una cola? Se utiliza una prueba 
# de una cola porque se quiere comprobar especificamente si l peso promedio de 
# los costales es menor a 80 kg

# Calcule la media de los 44 costales.
mean(costal)

# Calcule la desviación estándar de los 44 costales
sd(costal)

# Realice la pruba T de una muestra
boxplot(costal)
t.test(costal, mu = 80, alternative = "less")

#Realice la prueba: Identifique: 
# • estadístico t;       = -2.3644
# • grados de libertad;  = 43
# • valor de p;          = 0.01132
# • media observada.     = 78.91068

# Compare el valor de p con α = 0,05.¿Se rechaza o no se rechaza H0? 
# Si, se rechaza debido que el valor de p-value 0.01132 es menor que el nivel de
# significancia al 0.05. Esto indica que existe evidencia suficiente para 
# considerar que hay una diferencia significativa

# Redacte una conclusión en el contexto del problema, indicando si existe 
# evidencia estadística suficiente para afirmar que los costales contienen, 
# en promedio, menos de 80 kg.
# Como p= 0.01132 es menor que 0.05, se rechaza H0. por lo tanto, si existe 
# evidencia suficiente para afrimar que los costales contien en promedio 
# menos de 80 kg





