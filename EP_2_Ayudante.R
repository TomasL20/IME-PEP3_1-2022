# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas
# mediciones están disponibles en el archivo Body-EP12.csv que acompaña a este enunciado. El estudio
# incluyó nueve mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce
# mediciones de grosor (circunferencias) que incluyen el tejido. 

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el
# dígito verificador) del integrante de menor edad del equipo

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la
# semilla es impar)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la
# variable Peso, justificando bien esta selección

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado
# en el paso anterior

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresión líneal simple obtenido en el paso 5

# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que
# deben cumplir

# 8. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando
# validación cruzada)

library(dplyr)
library(ggpubr)
library (caret)
library(car)


dir <- "E:/IME"
base <- "Body-EP12.csv"

arch <- file.path(dir,base)

datos<-read.csv(arch, fileEncoding = "UTF-8")
# Se transforma la variable Género a factor 
#datos[["Gender"]] <- factor(datos[["Gender"]])

# RUT del integrante menor: 20433980-5
# Definir semilla a utilizar
semilla <- 3980 
set.seed(semilla)
# Como la semilla es un número par se obtienen sólo las mujeres
mujeres <- datos %>% filter(Gender == "0")
# Se selecciona la muestra de 50 mujeres
muestra <- mujeres[sample(nrow(mujeres), 50),]

# Se obtienen las columnas del dataframe
columnas <- colnames(muestra)

i_gender <- which(columnas == "Gender")
i_weight <- which(columnas == "Weight")
# Se elimina la variable Peso y Género del vector que contiene 
# los nombres de variables 
columnas <- columnas[-i_gender]
columnas <- columnas[-i_weight]

# Se seleccionan las 8 variables predictoras de forma aleatoria
variables <- sample(columnas, 8)

estan <- c()
for (i in 1:length(variables)){
  indice <- which(columnas == variables[i])
  estan <- c(estan, indice)
}
restantes <- columnas[-estan]

# La variable seleccionada es: 
# Chest.diameter (diámetro del pecho), ya que suponemos 
# que si una persona tiene mayor diámetro del pecho
# se podría pensar que también tiene mayor peso,
# es decir, al tener un mayor diámetro, tiene un mayor 
# volumen corporal, lo cual implica un mayor peso.


# Tal como se solicita, se piensa usar un modelo de regresión lineal 
# simple, lo cual se hará mediante mínimos cuadrados.

# Cumplimiento de condiciones:
# 1 .- Para medir la fuerza de la relación lineal entre la variable 
# Peso y el predictor, se debe calcular 
# el coeficiente de correlación que existe entre ellas

correlacion <- cor(muestra[["Chest.diameter"]], muestra[["Weight"]])
print(correlacion)

# De acuerdo al valor obtenido del coeficiente de correlación, 
# el cual es 0.625134 cercano a 1 y positivo, se puede afirmar que
# sí existe una relación lineal directa entre la variable Peso 
# y el predictor Diámetro del pecho. Además si se observa el gráfico 
# de la relación entre las variables, se puede observar la pendiente 
# positiva rearfimando la relación directa.

# 2.- De acuerdo al gráfico Q-Q de los residuos y al valor 
# obtemido por la prueba de Shapiro, se puede
# decir que estos siguen una distribución cercana a la normal, por
# lo que se verifica esta condición.

# 3.- Como se puede apreciar en el gráfico de los residuos, se puede decir
# que la variabildiad de los puntos en torno a la línea de los mínimos 
# cuadrados es aproximadamente constante ya que los puntos 
# están distribuidos se manera aleatoria, sin seguir un patrón, y 
# se puede apreciar que no forman un "embudo".

# 4.- Se cumple que las observaciones son independientes entre sí 
# ya que se elige una muestra al azar de tamaño 50, por lo que 
# no se usa regresión lineal con series de tiempo, ya que los datos no
# dependen de situaciones o acciones del pasado.


modelo <- lm( Weight ~ Chest.diameter, data = muestra)
print(summary(modelo))
# Test de Sahpiro para los residuos
shapiro <- shapiro.test(modelo$residuals)
print(shapiro)

# Gráfico Q-Q para la distribución de los residuos
datos_residuos <- data.frame(modelo$residuals)
g <- ggqqplot(datos_residuos, x = "modelo.residuals", color = "Steelblue", xlab = "Teórico", ylab = "Residuos",
              title = "Gráfico Q-Q residuos v/s distr. normal")
print(g)

# Se grafica el modelo
p <- ggscatter(muestra, x = "Chest.diameter", y = "Weight",
               color = "blue", fill = "blue", xlab = "Diametro del pecho [cm]",
               ylab = "Peso [Kg]")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print(p)

# Crear gráficos para evaluar el modelo
plot(modelo)

# CONCLUSIONES
# De acuerdo a los resultados obtenidos, se espera que al aumentar
# el diámetro del pecho en 1 centímetro, el peso aumente en 3,6660 
# kilogramos, lo cual tiene sentido, ya que los 3,6660 kilogramos se 
# distribuirán en su cuerpo, es decir, no sólo aumentará el diámetro de
# su pecho.

# Punto 6: 
# La variables seleccionadas son:
# - Hip.Girth
# - Age
# - Wrists.diameter

# Ahora se agrega cada una de las variables al modelo
modelo_RLM <- update (modelo, . ~ . + Hip.Girth)
modelo_RLM <- update (modelo_RLM, . ~ . + Age)
modelo_RLM <- update (modelo_RLM, . ~ . + Wrists.diameter)
print(summary(modelo_RLM))

#Se evalúan los dos modelos obtenidos
# Evaluación del RLS
modelo

# ------------------- Evaluación del modelo RLM -------------------
# Reducir matriz de datos para que solo contenga los predictores
# empleados y la respuesta .
predictores <- names(coef (modelo_RLM))[ -1]
muestra2 <- muestra[ , c(predictores , "Weight") ]
# Construir una matriz de datos con la respuesta predicha , los
# residuos y algunas estadísticas para evaluar la influencia de
# cada observación.
resultados <- data.frame(respuesta_predicha = fitted(modelo_RLM))
resultados [["residuos_estandarizados"]] <- rstandard(modelo_RLM)
resultados [["residuos_estudiantizados"]] <- rstudent(modelo_RLM)
resultados [["distancia_Cook"]] <- cooks.distance(modelo_RLM)
resultados [["dfbeta"]] <- dfbeta(modelo_RLM)
resultados [["dffit"]] <- dffits(modelo_RLM)
resultados [["apalancamiento"]] <- hatvalues(modelo_RLM)
resultados [["covratio"]] <- covratio(modelo_RLM)

cat ("Identificación de valores atípicos :\n")
# Observaciones con residuos estandarizados fuera del 95 % esperado.
sospechosos1 <- which(abs(resultados [["residuos_estandarizados"]]) > 1.96)

cat ("- Residuos estandarizados fuera del 95 % esperado :", sospechosos1 , "\n")

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(resultados [["distancia_Cook"]] > 1)

cat ("- Residuos con una distancia de Cook alta :", sospechosos2 , "\n")

# Observaciones con apalancamiento mayor igual al doble del
# apalancamiento promedio.

apal_medio <- (ncol(muestra2) + 1) / nrow (muestra2)
sospechosos3 <- which(resultados [["apalancamiento"]] > 2 * apal_medio)

cat ("- Residuos con apalancamiento fuera de rango :",sospechosos3, "\n")

# Observaciones con DFBeta mayor o igual a 1.
sospechosos4 <- which(apply( resultados [["dfbeta"]] >= 1, 1 , any))
names(sospechosos4) <- NULL

cat ("- Residuos con DFBeta >= 1:",sospechosos4, "\n")
# Observaciones con razón de covarianza fuera de rango .
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3 * apal_medio
sospechosos5 <- which (resultados [["covratio"]] < inferior | resultados [["covratio"]] > superior )

cat ("- Residuos con razón de covarianza fuera de rango :", sospechosos5 , "\n")
# Resumen de valores sospechosos .
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 ,sospechosos4 , sospechosos5 )

sospechosos <- sort(unique(sospechosos))

cat ("\n Resumen de valores sospechosos :\n")
cat (" Apalancamiento promedio :", apal_medio , "\n")
cat (" Intervalo razón de covarianza : [", inferior , "; ",superior , "]\n\n", sep = "")

print(round(resultados[sospechosos , c("distancia_Cook", "apalancamiento","covratio") ], 3) )

#-------------- Análisis de la evaluación del modelo RLM -----------------------
# La distancia de Cook estimada para todas las observaciones potencialmente influyentes 
# están lejos de sobrepasar el valor recomendado, por lo que en este caso no 
# parece ser necesario quitar observaciones, aún cuando algunas muestren
# valores un tanto alto de apalancamiento y covarianza.

# ------------------- Verificación de condiciones para modelo RLM -------------------
# Comprobar independencia de los residuos.
cat ("Prueba de Durbin - Watson para autocorrelaciones")
cat ("entre errores :\n")
print(durbinWatsonTest (modelo_RLM))


# Comprobar normalidad de los residuos para el modelo RLM.
cat ("\n Prueba de normalidad para los residuos :\n")
print(shapiro.test(modelo_RLM$residuals))

# Comprobar homocedasticidad de los residuos para el modelo RLM.
cat ("Prueba de homocedasticidad para los residuos :\n")
print(ncvTest(modelo_RLM))

# Comprobar la multicolinealidad.
vifs <- vif(modelo_RLM)
cat("\n Verificar la multicolinealidad :\n")
cat ("- VIFs :\n")
print(vifs)
cat ("- Tolerancias :\n")
print (1 / vifs )
cat ("- VIF medio :", mean ( vifs ) , "\n")

# ------ ANÁLISIS DE LA VERIFICACIÓN DE CONDICIONES ------
# 1. Al comprobar la independencia de los residuos se obtiene
# un p-valor = 0.874 mayor a nuestro nuestro alpha (0.05), por lo
# tanto podemos concluir que los residuos son independientes.

# 2.- Al aplicar la prueba de Shapiro a los residuos del modelo
# se obtiene un p-valor = 0.3992 > alfa = 0.05, por lo que
# se puede asumir que los residuos siguen una distribución normal.

# 3.- La prueba de homocedasticidad arroja un p-valor = 0.14645,
# el cual es mayor a alfa = 0.05, por lo que se cumple 
# el supuesto de homocedasticidad.

# 4.- Al comprobar la multicolinealidad, podemos observar
# los VIFS individuales y considerando VIF>= 2.5 como preocupante,
# obtenemos que todos son menores a dicho valor. Además todo los 
# valores de tolerancia son mayores a 0.4, por lo 
# que se cumple la condición de multicolinealidad.

# ------------------- Evaluar poder predictivo de los modelos-------------------
# Se crean los conjuntos de entrenamiento y prueba
n <- nrow(muestra)
n_entrenamiento <- floor(0.85*n)
var <- sample.int (n=n , size = n_entrenamiento ,replace = FALSE )
entrenamiento <- muestra[var, ]
prueba <- muestra [ -var, ]



# Se calcula el poder predictivo del modelo RLM 

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_val_cruz <- train ( Weight ~ Chest.diameter + Hip.Girth + Age + Wrists.diameter, data = muestra, method = "lm",
                           trControl = trainControl ( method = "cv", number = 5) )

print (summary(modelo_val_cruz))

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
rmse_entrenamiento <- modelo_val_cruz$results$RMSE
cat (" MSE para el conjunto de entrenamiento modelo RLM:", rmse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict(modelo_val_cruz,prueba)
# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["Weight"]] - predicciones
mse_prueba <- mean (error** 2)
rmse_prueba <- sqrt(mse_prueba)
cat (" MSE para el conjunto de prueba modelo RLM:", rmse_prueba)


# Se calcula el poder predictivo del modelo RLS
# Ajustar modelo usando validación cruzada de 5 pliegues.
modeloRLS_val_cruz <- train ( Weight ~ Chest.diameter, data = muestra, method = "lm",
                              trControl = trainControl ( method = "cv", number = 5) )

print (summary(modeloRLS_val_cruz))

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
rmse_entrenamiento_RLS <- modeloRLS_val_cruz$results$RMSE
cat (" MSE para el conjunto de entrenamiento modelo RLS:", rmse_entrenamiento_RLS , "\n")

# Hacer predicciones para el conjunto de prueba .
prediccionesRLS <- predict(modeloRLS_val_cruz,prueba)
# Calcular error cuadrado promedio para el conjunto de prueba .
errorRLS <- prueba [["Weight"]] - prediccionesRLS
mse_prueba_RLS <- mean (errorRLS** 2)
rmse_prueba_RLS <- sqrt(mse_prueba_RLS)
cat (" MSE para el conjunto de prueba modelo RLS:", rmse_prueba_RLS)

# Para el caso del modelo de regresión lineal simple, como se puede observar, para el conjunto de entrenamiento
# la raíz del error cuadrático medio es RMSEe = 6.903146 
# mientras que para el conjunto de prueba obtenemos RMSEp = 5.008835. 
# No existe mucha diferencia entre ambos errores, por lo que se puede concluir
# que el modelo no está sobreajustado, ya que los datos se adapatan bien tanto al conjunto entrenamiento
# como el de prueba, de este modo el modelo podría ser generalizado.


# Para el caso del modelo de regresión lineal múltiple, como se puede notar, para el conjunto de entrenamiento, 
# la raíz del error cuadrático medio es RMSEe = 3.794945,
# mientras que para el conjunto de prueba obtenemos RMSEp = 3.45992.
# Como los errores son muy parecidos, se puede decir que el modelo no está sobreajustado porque
# los datos se adapatan bien tanto al conjunto de datos de entrenamiento como el de prueba,
# por lo que podemos suponer que este modelo podría ser generalizable.