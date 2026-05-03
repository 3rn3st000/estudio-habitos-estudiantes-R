install.packages(c("readr", "dplyr", "forcats"))
library(readr) 
library(dplyr)   
library(forcats) 
encuesta = read_csv("/home/ernesto/Descargas/encuesta_estudiantes.csv")
#Comprobamos que los datos se han importado correctamente
summary(encuesta) 
head(encuesta)
#Damos formato a las columnas y les cambiamos el nombre para facilitarnos la manipulación
encuesta <- encuesta %>%
  mutate(
    genero = as.factor(Género),
    bebidas = as.integer(`Consumo de bebidas estimulantes semanal`),
    asignaturas = as.integer(`Asignaturas cursadas este año académico`),
    nota = as.numeric(`Nota media actual sobre 10`),
    suenio = as.numeric(`Media de horas de sueño diario`),
    horas = as.numeric(`Horas de estudio semanal (incluya clases en las que ha prestado atención)`),
    estudia = as.factor(`Rama de estudios`),
    estres = factor(`Nivel de estrés percibido`,levels = c(1,2,3,4,5),ordered = TRUE),
    deporte = factor(`Frecuencia de actividad física`,levels = c("Nunca","A veces","2-3 veces por semana","+3 veces"),ordered = TRUE)
  )%>%
  #Borramos las columnas sin formato y la de la fecha, al no ser una variable en nuestro estudio
  select(-`Nivel de estrés percibido`, -Género, -`Rama de estudios`, -`Frecuencia de actividad física`, -Timestamp, -`Asignaturas cursadas este año académico`,
         -`Consumo de bebidas estimulantes semanal`,-`Nota media actual sobre 10`,-`Media de horas de sueño diario`,-`Horas de estudio semanal (incluya clases en las que ha prestado atención)` )
#Comprobamos que los cambios se hayan aplicado de forma correcta
glimpse(encuesta)
#Aunque todos los campos de la encuesta eran obligatorios comprobamos si hay valores nulos que limpiar
colSums(is.na(encuesta))
#Ahora que sabemos que nuestros datos están limpios y formateados vamos a proceder a primero sacar un resumen y luego representarlas gráficamente
summary(encuesta$genero)
barplot(table(encuesta$genero))

summary(encuesta$estudia)
barplot(sort(table(encuesta$estudia)),horiz = TRUE,las = 1)

summary(encuesta$estres)
barplot(table(encuesta$estres))

summary(encuesta$deporte)
barplot(table(encuesta$deporte),horiz = TRUE, las = 1)


summary(encuesta$bebidas)
#Para las bebidas al estar muy concentradas en el 0 y luego en los valores altos estar más dispersas, agruparemos los valores mayores de 3(1.5 * tercer cuartil) para la represetación gráfica
mas3bebidas = pmin(encuesta$bebidas, 3)
bebidasGrafico = table(mas3bebidas)
names(bebidasGrafico)[names(bebidasGrafico) == "3"] = "+3"
barplot(bebidasGrafico,horiz = TRUE, las = 1)
boxplot(encuesta$bebidas)


summary(encuesta$asignaturas)
barplot(table(encuesta$asignaturas),horiz = TRUE, las = 1)
boxplot(encuesta$asignaturas)



summary(encuesta$suenio)
hist(encuesta$suenio,main= "Horas de sueño")
boxplot(encuesta$suenio)


summary(encuesta$nota)
hist(encuesta$nota,main = "Nota media")
boxplot(encuesta$nota)


summary(encuesta$horas)
hist(encuesta$horas,main = "Horas de estudio semanal")
boxplot(encuesta$horas)


#Ahora vamos a intentar modelar una regresión, primero usando un mapa de calor buscamos las variables más relacionadas con las otras
#También damos formato numérico a las variables ordinales para que puedan ser consideradas también
numericas = encuesta %>%
  mutate(
    estresNum = as.numeric(estres),
    deporteNum = as.numeric(deporte)
  ) %>%
  select(bebidas, asignaturas, nota, suenio, horas, estresNum, deporteNum)
#Usamos spearman ya que al haber variables ordinales nos asegura más precisión
correlacion = cor(numericas, method = "spearman")
heatmap(correlacion, main = "Matriz con Variables Ordinales")
horasNot = cor(encuesta$nota,encuesta$horas, method = "spearman") 
suenioNot = cor(encuesta$nota,encuesta$suenio, method = "spearman") 
asigNot = cor(encuesta$asignaturas,encuesta$nota,method = "spearman") 


#Como podemos observar las variables más relacionadas y por ello predecibles son horas, nota y un poco menos el sueño
#Elegimos hacer el modelo de la nota media, para evitar el overfitting y quedarnos con el modelo más simple probamos 3 modelos diferentes para quedarnos con el mejor

regresion1 = lm(nota ~ horas, data = encuesta)
summary(regresion1)

regresion2 = lm(nota ~ horas+suenio, data = encuesta)
summary(regresion2)

regresion3 = lm(nota ~ horas+suenio+asignaturas, data = encuesta)
summary(regresion3)


r2 = c(
  summary(regresion1)$adj.r.squared,
  summary(regresion2)$adj.r.squared,
  summary(regresion3)$adj.r.squared
)


modelos = c("nota ~ horas", "nota ~ horas + suenio", "nota ~ horas + suenio + asignaturas")
barplot(r2, names.arg = modelos, horiz = TRUE, las = 1  )
#Como podemos ver el mejor modelo es el que usa el sueño junto a las horas de estudio
plot(regresion2)
predicciones = predict(regresion2)
error = abs(encuesta$nota - predicciones) / encuesta$nota
err05 = mean(error < 0.05) 
err10 = mean(error < 0.10)
print(err05)
print(err10)


#Los estudiantes de media se perciben como estresados
#Las mejores notas sacrifican horas de sueño para obtener mejores puntuaciones?
#Es real el estereotipo de que las mejores notas no hacen deporte al estar "encerrados en la biblioteca"
#Los estudiantes que más horas estudian toman más bebidas estimulantes
#Empezamos ahora a comprobar las hipótesis

estresNum = as.numeric(encuesta$estres)
t.test(estresNum, mu = 3, alternative = "greater")  

encuesta$notaComp = ifelse(encuesta$nota >= median(encuesta$nota), "alta", "baja")
t.test(suenio ~ notaComp, data = encuesta, alternative = "less")
boxplot(suenio ~ notaComp, data = encuesta, main = "Horas de Sueño por Grupo de Nota")

deporteNum = as.numeric(encuesta$deporte)
t.test(deporteNum ~ notaComp, data = encuesta, alternative = "less")
boxplot(deporteNum ~ notaComp, data = encuesta, main = "Actividad Física por Grupo de Nota")


encuesta$horasComp = ifelse(encuesta$horas>= median(encuesta$horas), "alta","baja")
t.test(bebidas ~ horasComp, data = encuesta, alternative = "greater")
boxplot(bebidas ~ horasComp, data = encuesta)
