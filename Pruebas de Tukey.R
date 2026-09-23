# ANOVA

paraje <- read.csv("Datos_Rascon_Anova.csv", header = T)


boxplot(paraje$DAP ~ paraje$Paraje)
range(paraje$EDAD)
range(paraje$DAP)

tapply(paraje$DAP, paraje$Paraje, mean)
tapply(paraje$DAP, paraje$Paraje, var )
tapply(paraje$DAP, paraje$Paraje, sd)
tapply(paraje$DAP, paraje$Paraje,length)

#Aplicar normalidad y homogeneidad
#aov
pa.aov <- aov(paraje$DAP ~ paraje$Paraje)
summary(pa.aov)

TukeyHSD(pa.aov)
plot(TukeyHSD(pa.aov))
