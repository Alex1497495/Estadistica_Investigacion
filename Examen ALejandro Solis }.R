#Alejandro Solis Sanchez
# Matricula 1497495
#Datos obtenidos del examen

datos <- data.frame(
  Parcela = sprintf("P%02d" , 1:30),  
  Tratamiento = factor(
    rep(c("Testigo", "Ligero", "Moderado"), each =10),
    levels = c("Testigo", "Ligero", "Moderado")
  ),
  Area_basal_inicial = c(
    32, 26, 39, 29, 43, 35, 28, 41, 34, 37, 
    31, 42, 27, 38, 33, 45, 29, 40, 36, 44, 
    30, 37, 46, 28, 41, 34, 43, 32, 39, 35
  ),
  Incremento = c( 
    1.32, 1.16, 1.61, 1.42, 1.59,
    1.27, 1.35, 1.54, 1.43, 1.62,
    1.48, 1.72, 1.39, 1.82, 1.61,
    1.53, 1.88, 1.67, 1.59, 1.76,
    1.57, 1.93, 1.81, 2.08, 1.69,
    1.84, 2.16, 1.73, 1.98, 1.88
    )
)

testigo <- subset(datos, Tratamiento == "Testigo")
dos_grupos <- droplevels(
  subset(datos, Tratamiento %in% c("Testigo","Moderado"))
)
str(datos)
table(datos$Tratamiento)

#4.3 Estadistica descrptiva, tamaño de muestra, media, mediana, desviacion estandar, 
#varianza, minimo, maximo y coeficiente de variacion

resultados <- datos %>%
  group_by(Tratamiento) %>%
  summarise(
    n        = sum(!is.na(Incremento)),
    Media    = mean(Incremento, na.rm = TRUE),
    Mediana  = median(Incremento, na.rm = TRUE),
    DE       = sd(Incremento, na.rm = TRUE),
    Varianza = var(Incremento, na.rm = TRUE),
    Minimo   = min(Incremento, na.rm = TRUE),
    Maximo   = max(Incremento, na.rm = TRUE),
    CV       = ifelse(Media != 0, 100 * DE / Media, NA_real_),
    .groups  = "drop"
  )
print(resultados)

# Respuesta el tratamiento que presenta mayor media, mayor dispersion absoluta mayor 
# dispersion relativa es el tratamiento moderado ##

# 4.4 Actividad B. Representacion gráfica

op <- par(no.readonly = TRUE)

# Márgenes: inferior, izquierdo, superior y derecho
par(mar = c(5, 7, 3, 2))

boxplot(
  Incremento ~ Tratamiento,
  data = datos,
  col = c("#D9D9D9", "#A6CEE3", "#B2DF8A"),
  xlab = "Tratamiento",
  ylab = expression(
    "Incremento de área basal (" *
      plain(m)^2 ~ plain(ha)^{-1} ~ "año"^{-1} * ")"
  ),
  outline = FALSE
)

set.seed(123)

stripchart(
  Incremento ~ Tratamiento,
  data = datos,
  vertical = TRUE,
  method = "jitter",
  jitter = 0.10,
  pch = 16,
  col = "#333333",
  add = TRUE
)
modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = testigo)

plot(
  testigo$Area_basal_inicial, testigo$Incremento,
  pch = 16, col = "green",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
bline(modelo_reg, col = "red", lwd = 2)
##las medianas de cada tratamiento son aproximadamnte para testigo 1.25 para ligero 1.55
# y moderado 1.81por lo tanto el incremento del area basal aumenta conforme se pase del 
#Tratamiento, se observa poca supersposion en las distribuciones 
#ademas no se encuentras valorres atipicos

# cuenta con dirrecion positiva y forma lineal los puntos muestras una dispersion 
#moderada alrededor de la recta

#4.5 Actividad C. Correlacion de Pearson

resultado_cor <- cor.test(

  testigo$Area_basal_inicial, testigo$Incremento,
  method = "pearson", alternative = "two.sided"
)
resultado_cor
#Respuesta Ho: No existe correlacion lineal en el testigo
#H1: existe una correlacion lineal en el testigo

#4.6 Regresion Lineal Simple

modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = testigo)
confint(modelo_reg)
plot(
  testigo$Area_basal_inicial, testigo$Incremento,
  pch = 16, col = "#2166AC",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "#B2182B", lwd = 2)
summary(modelo_reg)
 
#4.7 Comparacion de dos muestras 

resultado_t <- t.test(
  Incremento ~ Tratamiento, data = dos_grupos,
  alternative = "two.sided", var.equal = TRUE,
  conf.level = 0.95
)
resultado_t

#4.8 Actividad F Comparacion de tres muestras 

modelo_anova <- aov(Incremento ~ Tratamiento, data = datos)
summary(modelo_anova)

# 4.9 Evaluacion de Supestos 

par(mfrow = c(1, 2))
plot(modelo_reg, which = 1)
plot(modelo_reg, which = 2)
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plot(modelo_anova, which = 1)
plot(modelo_anova, which = 2)
par(mfrow = c(1, 1))

