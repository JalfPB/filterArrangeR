library (tidyverse)
library (haven)
library(sjlabelled)
library(pillar)

#El csv ha sido obtenido de https://www.kaggle.com/datasets/uciml/adult-census-income

#Se lee el csv con los datos y se pasa a tibble
censo <- read_csv("./Data/adult.csv") %>% 
  remove_all_labels() %>% 
  as_tibble()

#Nos quedamos con las personas que nunca se han casado

nmarried <- censo %>% 
  select(marital.status, contains("Never_married"))

glimpse(nmarried)

#Usando una funcion de dpylr para filtrar columnas, se filtra el dataset para 
#encontrar los hombres que ganan mas de 50k

hombresMas50k <- censo %>% filter(sex == 'Male' & income == '>50K')

glimpse(hombresMas50k)

#Usando la misma funcion,ver quien tiene un bachelors degree y ademas sean de EEUU

bachelorsEEUU <- censo %>% filter(education == 'Bachelors' & native.country == 'United-States')

glimpse(bachelorsEEUU)

#Ordenar por horas trabajadas

censoHoras <- censo[order(censo$hours.per.week), ]

glimpse(censoHoras)

#Igual, pero usando la funcion arrange de dpylr

censoHoras <- arrange(censo,hours.per.week)
glimpse(censoHoras)

#Ordenar por edad en orden descendente

censoEdad <- arrange(censo,desc(age))
glimpse(censoEdad)

#Cambiar de nombre a la columna age por edad

censoEdad %>% 
  rename(edad = age) %>% 
  names()