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

#Crear columna de edad tipo character

censoEdad$edad_char <- as.character(censoEdad$age)
typeof(censoEdad$edad_char)

#Cambiar variables por una constante

censoHoras40 <- censo %>% 
  mutate(hours.per.week = 40)

censoHoras40 %>% 
  select(hours.per.week) %>% 
  head()

#Aplicar transformacion a variable existente

censoHoras40 <- censoHoras40 %>% 
  mutate(hours.per.week.nueva = hours.per.week - 1)

censoHoras40 %>% 
  select(starts_with("hours")) %>% 
  head

#Cambiar tipo de variable de variable existente

censoNuevo <- censoHoras40 %>% 
  mutate(hours.per.week = as.character(hours.per.week))

censoNuevo %>% 
  select(hours.per.week.nueva, hours.per.week) %>% 
  glimpse()

#Recodificar los items

censoMutado <- censo %>% 
mutate(income = recode(income,
                        `>50K` = 'Mas50k', # `old value` = new value
                       `<=50K` = 'Menos50k'))
table(censoMutado$income, censo$income)

#Recodificar con case_match

censoMutadoNuevo <- censo %>% 
  mutate(income = case_match(income,
                             ">50K" ~ 'Mas50k', # `old value` = new value
                             "<=50K" ~ 'Menos50k')) # `old value` = new value~
table(censoMutadoNuevo$income, censo$income)

#Recodificar con case_match dejando los valores no especificados intactos

censoMutadoNuevo2 <- censo %>% 
  mutate(income = case_match(income,
                             ">50K" ~ 'Mas50k', # `old value` = new value
                             "<=50K" ~ 'Menos50k',
                             .default = income)) # `old value` = new value~
table(censoMutadoNuevo2$income, censo$income)



