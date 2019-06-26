library(tidyverse)
library(rio)
library(estimatr)
library(lmtest)
library(skimr)

#diamonds - встроенный

# если бы надо было прочитать набор данных
#diamonds = import('.... откуда')

head(diamonds)
skim(diamonds)

d2 = mutate(diamonds, sum_xy = x + y)

model_0 = lm(data=diamonds, price ~ x + y + z + depth + table + cut)
summary(model_0)
# R обнаружил, что проблемы с сильной мультиколлинеарностью. Качеств огранки 5. Нужно вводить 4 дамми-пере-
# менных. R обнаружил, что это не приводит к успеху из-за мультиколлинеарности. R сам ортогонализировал их,
# чтобы мультиколлинеарности не было. cut.Q,C... -- замена переменных.

confint(model_0) #доверительные интервалы

model_bad = lm(data=d2, price ~ x + y + sum_xy)
summary(model_bad)
# автоматом выкинул sum_xy, выкидывает последний

model_r = lm(data=diamonds, price ~ x + y + z + cut)
#H0: верна model_r
#HA: model_r не верна, но верна model_0
# model_r -- частный случай model_0

waldtest(model_r, model_0)
# H0: beta_depth = 0, beta_table = 0

library(car)
H0 = c('depth=0', 'table=0')
linearHypothesis(model_0, H0)

# Проверить гипотезу, построить регрессию сделали и там, и там.
