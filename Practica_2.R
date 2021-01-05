#librerias que vamos a usar en el estudio
library(corrplot)
library(Hmisc)
library(arm)
library(ggplot2)
library(nortest)
library(car)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ggpubr)


#2. Integraci髇 y selecci髇 de los datos de inter閟 a analizar
setwd('C:/Users/ester/Desktop/Kaggle')
train <- read.csv('train.csv')
test <- read.csv('test.csv')
datos <- rbind(train[,-2], test)

#estudiar correlacion variables
datos_ <- datos
datos_$Pclass <- as.numeric(as.factor(datos$Pclass))
datos_$Sex <- as.numeric(as.factor(datos$Sex))
datos_$Embarked <- as.numeric(as.factor(datos$Embarked))
datos_cor <- datos_[,c(2,4:7,9,11)]

#funcion grafico correlacion
corrplot(as.matrix(datos_cor), color = c(1:5))


#test correlacion entre variables
res <- rcorr(as.matrix(datos_cor)) 
round(res$P, 3)
#Hay correlacion entre la variable `Pclass` y `Fare`, y entre las variables `SibSp` y `Parch`. Con lo cual prescindiremos de alguna de estas variables en los estudios posteriores.

#3. Limpieza de los datos.
#Transformamos algunas variables en factores:
datos$Pclass <- factor(datos$Pclass, levels = c(1,2,3), labels = c('First', 'Second', 'Third'))
datos$Sex <- factor(datos$Sex, levels = c('male','female'), labels = c('Male', 'Female'))
datos$Embarked <- factor(datos$Embarked, levels = c('C','Q','S'), labels = c('Cherbourg', 'Queenstown', 'Southampton'))

#Creamos nuevas variables
formula <- unlist(sapply(strsplit(datos$Name, ", "), function(x) x[2], simplify=FALSE))
datos$Formula <- unlist(sapply(strsplit(formula, ". "), function(x) x[1], simplify=FALSE))
datos$Nombre_Familia <- unlist(sapply(strsplit(datos$Name, ". "), function(x) x[1], simplify=FALSE))
datos$Num_Familiares_Totales <- datos$SibSp + datos$Parch
datos_clean <- datos[,c(1,2,4:7,9,11:14)]

#El dataset queda del siguento modo:
str(datos_clean)
head(datos_clean)


#3.1. 縇os datos contienen ceros o elementos vac韔s? 緾髆o gestionar醩 cada uno de estos casos?
#Algunas variables contienen ceros y/o elementos vacios. 
#Las variables `SibSp`, `Parch` y `Fare` contienen ceros.
plot(table(datos$Fare), ylab = 'Recuento valores',main = 'Tabla valores variable Fare')
datos_0 <- datos_clean[datos_clean$Fare == 0,]
head(datos_0)
#Vemos que todos son varones, mayores de edad y embarcaron en el puerto de Southampton.


#Las variables `Age`, `Fare` y `Embarked` contienen valores perdidos. 
#Realizaremos una imputacion simple por regresion de la variable `Age`:
datos_clean$Age_lm <- datos_clean$Age
lmod <- lm(Age ~ Sex + Pclass + Num_Familiares_Totales + Formula, data = datos_clean)
summary(lmod)
datos_clean$Age_lm[is.na(datos_clean$Age)] <- predict(lmod, newdata = subset(datos_clean, is.na(Age)))

#Comprobamos que se cumplen los supuestos del modelo lineal:
par(mfrow = c(1,2))
plot(lmod,1:2)

#Transformamos la variable `Age_lm` (`Age` con datos faltantes imputados) en un factor con tres niveles, Nino (0-15), Adulto Joven (16-40) y Adulto Mayor (> 41), ya que nos parece mas interesante para estudios posteriores:
datos_clean$Age_grupo <- datos_clean$Age_lm
datos_clean$Age_grupo[datos_clean$Age_lm <= 15] <- "Nino" 
datos_clean$Age_grupo[datos_clean$Age_lm > 15 & datos_clean$Age_lm <= 40] <- "Adulto Joven" 
datos_clean$Age_grupo[datos_clean$Age_lm > 40] <- "Adulto Mayor" 
datos_clean$Age_grupo <- factor(datos_clean$Age_grupo)


#3.2. Identificaci髇 y tratamiento de valores extremos.
par(mfrow = c(1,3))
boxplot(datos_clean$Age_lm, main= "Age_lm", col = 'coral2')
boxplot(datos_clean$Num_Familiares_Totales, main = "Num_familiares", col = 'lightblue')
boxplot(datos_clean$Fare, main = "Fare", col = 'gold')
#En este caso parece que los ouliers son valores son v醠idos y entran dentro del rango de las variables, por tanto, forman parte de la muestra, por lo que no se deben modificar ni eliminar, y se deben tener en cuenta en el an醠isis de los datos.

#4. An醠isis de los datos.
#4.1. Selecci髇 de los grupos de datos que se quieren analizar/comparar (planificaci髇 de los an醠isis a aplicar).

#- Mediante un algoritmo de clasificacion predecir que pasajeros sobrevivieron al hundimiento del Titanic segun sus caracteristicas. En este caso parece interesante utilizar un arbol de decision (decision trees), ya que nos da la informacion de como se clasificacn los pasajeros segun sus caracteristicas y nos devuelve un diagrama del mismo.

#- Mediante un modelo de regresion logistica predecir que probabilidad hay de que un pasajero sobreviva en base a sus caracteristicas.

#- Diferencia de la supervivencia entre las distintas clases del Titanic.
#- Diferencias de la mediana de edad entre hombres y mujeres a bordo del Titanic. 
#- Diferencia de las medianas de edad entre las distintas clases de pasajeros del Titanic. 
#- Diferencia de la mediana de numero total de familiares entre hombres y mujeres en el Titanic. 


#4.2. Comprobaci髇 de la normalidad y homogeneidad de la varianza.

#**Comprobacion de la normalidad:**
par(mfrow = c(1,3))
boxplot(datos_clean$Age_lm, main="Boxplot Age_lm", col = 6)
hist(datos_clean$Age_lm, main = "Histograma Age_lm", col = 7)
qqnorm(datos_clean$Age_lm, pch = 19, col = "gray50")
qqline(datos_clean$Age_lm)

#Graficos variable `Age_lm`
datos1 <- na.omit(datos_clean)
ggplot(data = datos1, aes(x = Age_lm)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",args = list(mean = mean(datos1$Age_lm),sd = sd(datos1$Age_lm))) +
  ggtitle("Histograma con curva normal te贸rica") +
  theme_bw()

#Graficos variable `Age_lm`
par(mfrow = c(1,3))
boxplot(datos_clean$Fare, main="Boxplot Fare", col = 6)
hist(datos_clean$Fare, main = "Histograma Fare", col = 7)
qqnorm(datos_clean$Fare, pch = 19, col = "gray50")
qqline(datos_clean$Fare)


ggplot(data = datos1, aes(x = Fare)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",args = list(mean = mean(datos1$Fare),sd = sd(datos1$Fare))) +
  ggtitle("Histograma con curva normal te贸rica") +
  theme_bw()

#Graficos variable `Num_Familiares_Totales`
par(mfrow = c(1,3))
boxplot(datos_clean$Num_Familiares_Totales, main="Boxplot Fam_totales", col = 6)
hist(datos_clean$Num_Familiares_Totales, main = "Histograma Fam_totales", col = 7)
qqnorm(datos_clean$Num_Familiares_Totales, pch = 19, col = "gray50")
qqline(datos_clean$Num_Familiares_Totales)

ggplot(data = datos1, aes(x = Num_Familiares_Totales)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",args = list(mean = mean(datos1$Num_Familiares_Totales),sd = sd(datos1$Num_Familiares_Totales))) +
  ggtitle("Histograma con curva normal te贸rica") +
  theme_bw()

#El test Lilliefors asume que la media y varianza son desconocidas, estando especialmente desarrollado para contrastar la normalidad. Es la alternativa al test de Shapiro-Wilk cuando el n鷐ero de observaciones es mayor de 50, como es nuestro caso.
lillie.test(x = datos_clean$Age_lm)
lillie.test(x = datos_clean$Fare)
lillie.test(x = datos_clean$Num_Familiares_Totales)

#**Comprobacion homogeneidad de la varianza:**
ggplot(data = datos_clean, aes(x = Sex, y = Age_lm , colour = Sex)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data = datos_clean, aes(x = Pclass, y = Age_lm , colour = Pclass)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data = datos_clean, aes(x = Sex, y = Num_Familiares_Totales , colour = Sex)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

#Tablas varianza:
aggregate(Age_lm~Sex, data = datos_clean, FUN = var)
aggregate(Age_lm~Pclass, data = datos_clean, FUN = var)
aggregate(Num_Familiares_Totales~Sex, data = datos_clean, FUN = var)

#Levene test:
leveneTest(datos_clean$Age_lm ~ datos_clean$Sex, center = "median")
leveneTest(datos_clean$Age_lm ~ datos_clean$Pclass, center = "median")
leveneTest(datos_clean$Num_Familiares_Totales ~ datos_clean$Sex, center = "median")

#4.3. Aplicaci髇 de pruebas estad韘ticas para comparar los grupos de datos. En funci髇 de los datos y el objetivo del estudio, aplicar pruebas de contraste de hip髏esis, correlaciones, regresiones, etc. Aplicar al menos tres m閠odos de an醠isis diferentes.

##**1. Algoritmo de clasificacion predecir que pasajeros sobrevivieron al hundimiento del Titanic segun sus caracteristicas: Decision tree**
#Utilizamos el dataset train.csv que es el que tiene la variable `Survived`.

#Tabla supervivientes en el dataset train:
prop.table(table(train$Survived))

train_clean <- cbind(datos_clean[c(1:891),], Survived = train[,2])
str(train_clean)
train_clean$Survived <- factor(train_clean$Survived, levels = c(0,1), labels = c('No', 'Yes'))


#Particion dataset train en entrenamiento y prueba, proporciones de supervivientes en los datasets obtenidos:
set.seed(123)
entrenamiento <- sample_frac(train_clean, .7)
prueba <- setdiff(train_clean, entrenamiento)
prop.table(table(entrenamiento$Survived))
prop.table(table(prueba$Survived))


#Modelo 1 Arbol de decision con las variables Age_lm, Pclass, Sex, SibSp y Parch:
fit1 <- rpart(Survived~ Age_lm + Pclass + Sex + SibSp + Parch , data = entrenamiento, method = 'class')
rpart.plot(fit1)
prediccion_1 <- predict(fit1, newdata = prueba, type = "class")
confusionMatrix(prediccion_1, prueba$Survived)

#Modelo 2 Arbol de decision con las variables Age_grupo, Pclass, Sex y Num_Familiares_Totales:
fit2 <- rpart(Survived~ Age_grupo + Pclass + Sex + Num_Familiares_Totales , data = entrenamiento, method = 'class')
rpart.plot(fit2)
prediccion_2 <- predict(fit2, newdata = prueba, type = "class")
confusionMatrix(prediccion_2, prueba$Survived)

##**2. Mediante un modelo de regresion logistica predecir que probabilidad hay de que un pasajero sobreviva en base a sus caracteristicas.**
#Modelo 1 de regresion logistica con las variables Age_lm, Pclass, Sex, SibSp y Parch:
modelo_logistico1 <- glm(Survived ~ Age_lm + Pclass + Sex + SibSp + Parch , data = entrenamiento, family = "binomial")
modelo_logistico1

prediccion_3 <- predict(modelo_logistico1, newdata = prueba, type = "response")
prediccion_3[prediccion_3 > 0.5] <- "Yes"
prediccion_3[prediccion_3 <= 0.5] <- "No"
prediccion_3 <- factor(prediccion_3)

confusionMatrix(prediccion_3, prueba$Survived)


#Modelo 1 de regresion logistica con las variables Age_grupo, Pclass, Sex y Num_Familiares_Totales:
modelo_logistico2 <- glm(Survived ~ Age_grupo + Pclass + Sex + Num_Familiares_Totales , data = entrenamiento, family = "binomial")
modelo_logistico2

prediccion_4 <- predict(modelo_logistico2, newdata = prueba, type = "response")
prediccion_4[prediccion_4 > 0.5] <- "Yes"
prediccion_4[prediccion_4 <= 0.5] <- "No"
prediccion_4 <- factor(prediccion_4)

confusionMatrix(prediccion_4, prueba$Survived)

##**3. Diferencia de la supervivencia entre las distintas clases del Titanic.**
table1 <- table(train_clean$Pclass, train_clean$Survived)
table1
plot(table1, color = c('coral2', 'lightblue'), main = 'Tabla supervivencia pasajeros por clase')

#Prueba chi2
chisq <- chisq.test(table1)
chisq

##**4. Diferencias de la mediana de edad entre hombres y mujeres. **

#Test Wilcoxon
wilcox.test(x = datos_clean$Age_lm[datos_clean$Sex == "Male"], y = datos_clean$Age_lm[datos_clean$Sex == "Female"], alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)

cordenada_y = length(datos_clean$Age_lm)
ggplot(data = datos_clean, aes(x = Age_lm, y = cordenada_y)) +
  geom_jitter(aes(colour = Sex), size = 3) +
  ylab("") + xlab("rango") +
  theme_bw() +
  theme(axis.text.y = element_blank()) + 
  ggtitle("Muestras")


##**5. Diferencia de las medias de edad entre las distintas clases de pasajeros del Titanic.**
  
#Checkear las condiciones para realizar un test ANOVA:
ggboxplot(datos_clean, x = "Pclass", y = "Age_lm", 
          color = "Pclass", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("First", "Second", "Third"),
          ylab = "Age_lm", xlab = "Pclass")
#test normalidad
shapiro.test(datos_clean$Age_lm[datos_clean$Pclass == "First"])
shapiro.test(datos_clean$Age_lm[datos_clean$Pclass == "Second"])
shapiro.test(datos_clean$Age_lm[datos_clean$Pclass == "Third"])

#Como las muestras no son normales, realizamos el test no parametrico Kruskal-Wallis. Se usa para probar si un grupo de datos proviene de la misma poblaci贸n. Intuitivamente, es id茅ntico al ANOVA con los datos reemplazados por categor铆as. Es una extensi贸n de la prueba de la U de Mann-Whitney para 3 o m谩s grupos.

#Test Kruskal-Wallis
kruskal.test(Age_lm ~ Pclass, data = datos_clean)

##**6. Diferencia de la media de familiares entre hombres y mujeres en el Titanic. **
#Test Wilcoxon
wilcox.test(x = datos_clean$Num_Familiares_Totales[datos_clean$Sex == "Male"], y = datos_clean$Num_Familiares_Totales[datos_clean$Sex == "Female"], alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)

cordenada_y = length(datos_clean$Num_Familiares_Totales)
ggplot(data = datos_clean, aes(x = Num_Familiares_Totales, y = cordenada_y)) +
  geom_jitter(aes(colour = Sex), size = 3) +
  ylab("") + xlab("rango") +
  theme_bw() +
  theme(axis.text.y = element_blank()) + 
  ggtitle("Muestras")

#5. Representaci髇 de los resultados a partir de tablas y gr醘icas.

#Resultados algoritmo arboles de decision y modelo de regresion logista para predecir la supervivencia de los pasajeros:

#El sistema de clasificacion con un mayor nivel de precision es el segundo modelo de arboles de decision:
rpart.plot(fit2)
#Las mujeres sobreviven un 75%, si no viajan en tercera clase sobreviven el 96%, si viajan en tercera clase solo sobrevivien el 52%. De las mujeres que viajan en tercera clase sobreviven mas las que tienen un numero de familiares totales menor a 4.
#Los hombres tiene una probabilidad del 20% de sobrevivir, y si son hombres adultos la probabilidad es del  17%, mientras que si son ninos la probabilidad es del 55%. Y de los ninos, los que tienen mas probabilidad de sobrevivir son los que tienen un numero total de familiares menor a 4.


plot(modelo_logistico2$coefficients, ylab = 'Valor coeficientes modelo regresion logistica', xlim=c(1,7), ylim=c(-4, 4.5))
text(modelo_logistico2$coefficients,  names(modelo_logistico2$coefficients), cex=0.65, pos=3,col="red") 
abline(h=0, col='blue')
#En cuanto a los modelos de regresion logistica el segundo modelo tambien presente mayor precision.
#Por los coeficientes de estemodelo de regresion logistica podemos ver que la variable mujeres y pertenecer al grupo de edad Nino aumenta la probabilidad de sobrevivir al humdimiento del Titanic. 
#Mientras que pertenecer a tercera y segunda clase disminuye la probabilidad de supervivencia, asi como pertener al grupo Adulto Mayor, y en menor proporcion el numero de familiares totales.


#Los tests que hemos realizado podemos concluir que hay relacion entre la clase y la supervivencia en el hundimiento, como muestra la tabla de valores absolutos, la tabla de proporciones y la grafica los pasajeros de primera clase son los que sobrevivieron en mayor proporcion.
table1
prop.table(table1)
plot(table1, main = 'Proporcion supervivientes en cada clase', col = c('coral2', 'lightblue'))


#Tambien podemos concluir que hay una diferencia significativa entre las medianas de edad entre hombres y mujeres, la mediana de los hombres es mayor que la de las mujeres. 
cordenada_y = length(datos_clean$Age_lm)
ggplot(data = datos_clean, aes(x = Age_lm, y = cordenada_y)) +
  geom_jitter(aes(colour = Sex), size = 3) +
  ylab("") + xlab("rango") +
  theme_bw() +
  theme(axis.text.y = element_blank()) + 
  ggtitle("Age_lm")


#Las medianas de edad entre las clases no son iguales, las clases mas altas presentan edades mas avanzadas. 
ggboxplot(datos_clean, x = "Pclass", y = "Age_lm", 
          color = "Pclass", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("First", "Second", "Third"),
          ylab = "Age_lm", xlab = "Pclass")

#Hay diferencias significativas entre las medianas del numero de familiares totales entre hombres y mujeres, hay mas hombres que viajan solos. 
cordenada_y = length(datos_clean$Num_Familiares_Totales)
ggplot(data = datos_clean, aes(x = Num_Familiares_Totales, y = cordenada_y)) +
  geom_jitter(aes(colour = Sex), size = 3) +
  ylab("") + xlab("rango") +
  theme_bw() +
  theme(axis.text.y = element_blank()) + 
  ggtitle("Num_familiares_totales")

