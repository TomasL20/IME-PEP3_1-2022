# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
#                                                                                                            capitán, comandante y general) califican a los snowtroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.

library ( tidyverse )
library ( ggpubr )
library ( ez )

# Lectura del archivo 

dir <- "E:/IME/PEP2_IME_2-2021"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")


#-----------------------------------------------PREGUNTA DOS ------------------------------------------------------------
# (24 puntos) A fin de determinar si es necesario establecer programas de entrenamiento diferenciados para clones y
# reclutas, Lord Vader quiere saber si es posible distinguir entre ambas clases de soldados con los datos actuales. Para ello,
# ha solicitado evaluar un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, para ser
# aceptable, el modelo:
#   • Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# • No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
# • No debe presentar autocorrelación (usando la prueba de Durbin-Watson para un retardo y un nivel de significación
#                                     α = .01)
# • No debe presentar multicolinealidad severa (considerando el factor de inflación de la varianza, con un VIF promedio
#                                              inferior a 1,03).
# Considere la semilla 4432 para obtener una muestra de 400 datos, 80% de los cuales serán empleados para ajustar el
# modelo y el 20% restante, para evaluarlo.

# Para este caso se piensa utilizar Regresión logística ya que la variable de respuesta
# se puede expresar como 0 o 1. 

library(pROC)
library(caret)
library(dplyr)
library(car)
library(ggpubr)

# Lectura del archivo
dir <- "E:/IME/PEP2_IME_2-2021"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")
#Crear la variable dicotómica para es_clon: 
# 1: Si 
# 0: No 
condicion <- ifelse(datos[["es_clon"]] =="S", 1,  0)
# Se tramsforma es_clon en variable categórica
datos[["es_clon"]] <- factor(condicion)

#se define la semilla con la que trabajaremos, lo cual nos permite
#trabajar siempre con los mismos números
set.seed(4432)

# se define el tamaño de la muestra
tam <- 400

# Se obtiene la muestra de 400 datos
datos <- datos[sample(nrow(datos), tam), ]


n <- nrow(datos)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

columnas <- colnames(datos)

i_es_clon <- which(columnas == "es_clon")
columnas <- columnas[-i_es_clon]

# Se seleccionan las 8 variables predictoras de forma aleatoria
variables <- sample(columnas, 8)
print(variables)

# Se escoge la variable velocidad y peso 
# ya que suponemos que un clon podría 
# ser mejor que un soldado respecto a la velocidad
# y que quizás tenga menor peso, lo cual
# implicaría que fuese más ágil y veloz.

# Ajustar modelo.
modelo <- glm(es_clon ~ peso + velocidad, family = binomial(link ="logit"),data = entrenamiento)
print(summary(modelo))


# ----------- EVALUACIÓN DEL MODELO -----------
# Obtener los residuos y las estadísticas .
output <- data.frame (predicted.probabilities = fitted(modelo))
output [["standardized.residuals"]] <- rstandard(modelo)
output [["studentized.residuals"]] <- rstudent( modelo)
output [["cooks.distance"]] <- cooks.distance(modelo)
output [["dfbeta"]] <- dfbeta(modelo )
output [["dffit"]] <- dffits(modelo)
output [["leverage"]] <- hatvalues(modelo)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deberían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which (abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos1, ]) )

# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(entrenamiento)/nrow(datos)
sospechosos3 <- which(output [["leverage "]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)

cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat (round(leverage.promedio, 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(rownames(entrenamiento[sospechosos3, ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1 ,1 ,any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos4 , ]))

# Detalle de las observaciones posiblemente atí picas .
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort (unique(sospechosos))
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print(entrenamiento[sospechosos, ])
cat("\n\n")
print(output[sospechosos , ])


# -------------- VERIFICACIÓN DE CONDICIONES ----------

# Verificación de multicolinealidad .
cat ("Verificación de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\n VIF :\n")
vifs <- vif (modelo)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )
# Si miramos los factores de inflación de la varianza, 
# en general no parecen ser preocupantes, por lo que se verifica
# la condición de multicolinealidad. 

# Independencia de los residuos.
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo) )


# ----------- EVALUAR EL MODELO -------

# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["es_clon"]]) )

ROC_e <- roc(entrenamiento[["es_clon"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["es_clon"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.

cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["es_clon"]]) )

ROC_p <- roc(prueba[["es_clon"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["es_clon"]])
print(matriz_p)


# Como se puede observar, la exactitud es de 0.9125 para el conjunto de prueba.

# No existen casos con distancia de Cook mayor a 1.

# Considerando un alfa = 0.01, se tiene que el p-valor obtenido 
# para la prueba de Durbin Watson es de 0.522, por lo que 
# se cumple la independencia de los residuos.

# Se verifica la condición de multicolinealidad dado que el promedio de VIF = 1.000187
# es menor que 1,03

# Es por lo anterior, que se puede decir que el modelo es aceptable y generalizable
# Además, se puede observar que la curva de ROC tanto para el grupo
# de entrenamiento como de prueba, se aleja notablemente de la diagonal.
