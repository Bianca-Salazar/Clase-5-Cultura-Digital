2+2 #ctrl + enter 

b <- 4*4

a <- 2+2

multiplicacion <-  a*b
print(multiplicacion)

#Vectores#

vector1 <- c("Eva","Santiago","Camilo","Guadalupe")

vector2 <- c(25,50,40,36)

vector1[3]
vector2[3]

##indices##

indice <- vector2<=40
menor40 <-  vector2[indice]  
print(menor40)  

#Data Frames

df <- data.frame(nombres=vector1,edad=vector2)
df1 <- data.frame(nombres=vector1,edad=vector2,ranking=c(1,3,4,2))
df1$ranking <- NULL
vector3 <- c(4,3,1,2) 
df1$ranking <- vector3
df1$profesiones <- c("lic","pdh","msc","ing")

colnames(df1) <- c("Nombres","Edad","Ranking","Profesiones")

indice1 <- df1$Edad==40
menor <- df1[indice1,1:2]
print(menor)

indice1 <- df1$Edad==40
menor <- df1[indice1,c(1,4)]
print(menor)



library(dplyr)
library(ggpubr)  
library(ggplot2)

data <- read.csv("Salary.csv",header=T, sep=",",dec=".")
colnames(data) <- c("Experiencia","Salario")
summary(data)
sum(is.na(data)) #no hay imgorfacion NA

glimpse(data)
#rows=filas instancias, colums=columnas atributos
#dbl=doble(decimales), int=numero entero

#GRÁFICAS DE REFERENCIA PARA EXPLIRAR PATRONES
plot(data)

# 1 y 5 col
plot(data$Salario,data$Experiencia)
barplot(data$Experiencia)
boxplot(data$Experiencia)
boxplot(data$Salario)

#Rl
modelo <- lm(Salario~Experiencia,data=data)
summary(modelo)
#estimate std y multiple R squared 0.9652
#ecuacion = y = 87319x + 28859.5 x=6 6años
#r = ajuste 0.9652 buen ajuste 
#ver para que sirve Adjusted R-squared:  0.9641 

residuos <-  modelo$residuals
shapiro.test(residuos)

#cuantil cuartil
ggqqplot(residuos,ylab = "Residuos Modelo")

plot(modelo$fitted.values, modelo$residuals)

coeficiente <- modelo$coefficients
Pendiente <- coeficiente[2]
intercepto <- coeficiente[1]

ecuacion <- paste("y = ", round(Pendiente, 2), "x + ", round(intercepto, 2))
ggplot(data, aes(x = Experiencia, y = Salario)) + 
  geom_point() +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = '#FF4040') + 
  theme_light() +
  labs(x = "Experiencia (años)", y = "Salario (dólares)") +
  annotate("text", x = 10, y = 50000, label = ecuacion, color = "blue", size = 3)













