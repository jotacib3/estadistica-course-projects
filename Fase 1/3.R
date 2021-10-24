rm(list=ls()) # Clean the global environment

data <- read.csv("9 - Bank Marketing/bank-full.csv" ) #load dataset

married_data <- data[data$marital == "married",]
single_data <- data[data$marital == "single",]

married_duration <- married_data$duration
single_duration <- single_data$duration

#Como no conocemos las varianzas y tampoco conocemos si estas son iguales
#o diferentes. Pues sería necesario realizar primero una prueba de igualdad 
#contra diferencia en las varianzas y en dependencia del resultado de la misma 
#realizamos la prueba de medias con varianza desconocida correspondiente.


#Prueba de hipotesis para la comparacion de varianza
#H0: var_married = var_single
#H1: var_married != var_single

varTest <- var.test(married_duration,single_duration)
varTest
varTest$p.value

alpha = 0.05

#Prueba para la media
meanTest<- t.test(married_duration,single_duration, varTest$p.value < alpha, alternative = "less")
meanTest
meanTest$p.value


