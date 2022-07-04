library(pROC)
library(caret)
library(dplyr)
library(car)
library(ggpubr)

# Lectura del archivo
#dir <- "E:/IME"
#base <- "Body-EP12.csv"
#arch <- file.path(dir,base)
#datos<-read.csv(arch, fileEncoding = "UTF-8")


# Se agrega la columna IMC
datos[["IMC"]] <- datos[["Weight"]] / ((datos[["Height"]] / 100)^2) 

#Crear la variable dicotómica
# 1: Sobrepeso
# 0: No sobrepeso
condicion <- ifelse(datos[["IMC"]] >= 25, 1,  0)
# Se tramsforma EN en variable categórica
datos[["EN"]] <- factor(condicion)

# RUT del integrante menor: 20433980-5
# Definir semilla a utilizar
semilla <- 3980 
set.seed(semilla)

# Como la semilla es un número par se obtienen sólo las mujeres
mujeres <- datos %>% filter(Gender == "0")

# Se seleccionan 25 datos de sobrepeso y 25 de no sobrepeso
# debido a un problema de datos que se tuvo al momento de 
# realizar el modelo de regresión logística, ya que en la muestra
# había muchas observaciones con EN = 0.
sobrepeso <- mujeres %>% filter(EN == 1)
muestra_sobrepeso <- sobrepeso[sample(nrow(sobrepeso), 25),]

no_sobrepeso <- mujeres %>% filter(EN == 0)
muestra_no_sobrepeso <- no_sobrepeso[sample(nrow(no_sobrepeso), 25),]

datos <- rbind(muestra_no_sobrepeso,muestra_sobrepeso)


# Recordando las 8 variables predictoras del ejercicio anterior:
# "Hip.Girth"           "Knees.diameter"      "Wrists.diameter"     "Wrist.Minimum.Girth"
# "Waist.Girth"         "Chest.depth"         "Age"                 "Ankle.Minimum.Girth"

# La variable seleccionada de las restantes es: 
# Chest.diameter (diámetro del pecho), ya que suponemos 
# que si una persona tiene mayor diámetro del pecho
# se podría pensar que también tiene mayor peso,
# es decir, al tener un mayor diámetro, tiene un mayor 
# volumen corporal, lo cual implica un mayor peso.

# Se construye el modelo de regresión logística con el predictor "Chest.diameter" 

# Separar conjuntos de entrenamiento y prueba
n <- nrow(datos)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

# Ajustar modelo de regresión logística
modelo <- glm (EN ~ Chest.diameter, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["EN"]]) )

ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["EN"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["EN"]]) )

ROC_p <- roc(prueba[["EN"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["EN"]])
print(matriz_p)

# ------ ANÁLISIS DEL MODELO Y MATRIZ DE CONFUSIÓN ------
# Al analizar el modelo podemos apreciar que el valor 
# de AIC es 51.256, y que la desviación del modelo con una variale
# (38 grados de libertad) es de  47.256.

# La curva de ROC tanto para el conjunto de entrenamiento
# como de prueba se aleja de la diagonal, 
# por lo que al parecer se trata de un buen modelo.

# Si nos fijamos en los valores obtenidos en la matriz de confusión
# para los datos de entrenamiento podemos observar que se obtiene
# una exactitud de 62,5% , una sensibilidad de 61,9% y la especificidad
# de 63,16% 

# Para el caso del conjunto de prueba se tiene una exactitud
# de 80%, una sensibilidad de 75%, y la especificidad de 
# 83,33%. 

# 4.- Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al
# modelo obtenido en el paso 3

# La variables seleccionadas son:
# - Hip.Girth
# - Wrists.diameter

# Ahora se agrega cada una de las variables seleccionadas al modelo
modelo_RLM <- update (modelo, . ~ . + Hip.Girth)
modelo_RLM <- update (modelo_RLM, . ~ . + Wrists.diameter)
print(summary(modelo_RLM))

# ----------- EVALUACIÓN DEL MODELO RLM -----------
# Obtener los residuos y las estadísticas .
output <- data.frame (predicted.probabilities = fitted(modelo_RLM))
output [["standardized.residuals"]] <- rstandard(modelo_RLM)
output [["studentized.residuals"]] <- rstudent( modelo_RLM )
output [["cooks.distance"]] <- cooks.distance(modelo_RLM)
output [["dfbeta"]] <- dfbeta(modelo_RLM )
output [["dffit"]] <- dffits(modelo_RLM)
output [["leverage"]] <- hatvalues(modelo_RLM)

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
vifs <- vif (modelo_RLM)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )
# Si miramos los factores de inflación de la varianza, 
# en general no parecen ser preocupantes, por lo que se verifica
# la condición de multicolinealidad. 

# Independencia de los residuos.
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo_RLM) )

# Como se obtiene un p-value = 0.724 > alfa = 0.05,
# se puede decir que se cumple la condición de 
# independencia de los residuos.

# Información incompleta
# Verificar esta condición se hace un poco complicado, debido a que mientras mas variables tengamos
# más problemas tendremos para encontrar datos que cumplan con las condiciones, por ejemplo con lo que pasa
# con las variables categoricas en este caso genero (hombre o mujer) y luego que un hombre o mujer tenga o no sobrepeso
# y que luego su altura se encuentre entre la categoria bajo, medio alto o alto, entonces sera dificil encontrar una dato que cumpla
# con todo lo que necesitamos. 


# ----------- EVALUAR PODER PREDICTIVO PARA RLM -------
# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo_RLM, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["EN"]]) )

ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["EN"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo_RLM, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["EN"]]) )

ROC_p <- roc(prueba[["EN"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["EN"]])
print(matriz_p)

# Al observar la matriz de confusión del modelo que utiliza 3 variables 
# predictivas, se puede observar que tanto su exactitud, como la 
# sensibilidad y la especificidad es mucho mejor que el modelo donde 
# se utiliza sólo una variable.

# Además, al observar los gráficos de la curva de ROC para los grupos 
# de entrenamiento y de prueba, éstas mejoran, alejándose mucho 
# más de la diagonal.


#----------------------------- COMPARANDO LOS MODELOS -------------------------------------- 

# Hipótesis a contrastar                  

# H0: Ambos modelos son iguales
# HA: Un modelo es mejor que otra

# Comparar los modelos RLS y el RLM
cat ("\n\n")
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print (anova(modelo, modelo_RLM, test = "LRT") )

# Observando los resultados obtenidos al comparar los dos modelos,
# podemos decir que el modelo_RLM(tres predictores) es mejor que el 
# modelo simple(solo con un predictor), debido a que tiene 
# su p-valor = 0.001207, el cual es muy significativo. 




