library(rio) # Импорт данных
library(tidyverse) # Обработка данных
library(stringr) # Работа с текстовыми переменными
library(forecast) # Прогнозирование

m0 = import('marriage_data_raw.xls')

# Другие способы импорта
# m0 = import('~/Downloads/file')
# Sys.setenv(language='russian')
# library(readxl)
# m0 = read_excel(.)

head(m0)
# view(m0)

m1 = tail(m0, -2) # Первые две строки удаляем
colnames(m1) = c('region', 'unit', 'period', 2006:2018) # Присваиваем нормальные имена переменным

qplot(data = m1, x = unit) # Кроме unit ничего нет, unit -- удаляем

m2 = select(m1, -unit)

glimpse(m2)

m3 = filter(m2, !str_detect(period, '-')) # Удаляем неправильные (по нашему предположению) месяцы

glimpse(m3)

strange_names = unique(m3$period)
strange_names

coder = tibble(period = strange_names, month = 1:12)
coder

m4 = left_join(m3, coder, by = 'period')
glimpse(m4)

m5 = select(m4, -period)
glimpse(m5) # такой формат называется широким (wide)

m6 = gather(m5, key = 'year', value = 'marriage', -region, -month) # сделать отдельный столбец для года,
# а значение в исходной таблице превратить в значение столбца marriage
glimpse(m6)

m7 = mutate(m6, year = as.numeric(year), marriage = as.numeric(marriage))
glimpse(m7)

m8 = filter(m7, str_detect(region, 'Архангельская'))
m9 = arrange(m8, year, month)

unique(m7$region)

m8 = filter(m7, str_detect(region, '11000000000 Архангельская область'))
m9 = arrange(m8, year, month)
m9

y = ts(m9$marriage, start =  c(2006, 1), frequency = 12)
ggseasonplot(y)

ggtsdisplay(y)

model_aaa = ets(y, model = 'AAA', damped = FALSE)
autoplot(model_aaa)

model_aaa$states %>% head()

prognoz = forecast(model_aaa, h = 24)
prognoz
autoplot(prognoz) # CI уходит в минус -- лучше мультипликативная модель
