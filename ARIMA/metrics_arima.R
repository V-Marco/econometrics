library(rio) # import data
library(tidyverse) #data manipulations
library(forecast) # 1d time series models

marriage = import('marriage_data_clean.csv')
glimpse(marriage)

# если есть проблема с кракозябрами под виндой
# marriage = read_csv("~/...")

m2 = filter(marriage, str_detect(region, 'Россия'))
m2

y = ts(m2$marriage, start=c(2006, 1), freq = 12)
ggtsdisplay(y)
ggseasonplot(y)

ar1 = Arima(y, order = c(1, 0, 0))
ar1

future = forecast(ar1, h = 12)
future
autoplot(future)
autoplot(ar1) # единичный круг для существования стационарного решения

sarima_100_100 = Arima(y, order = c(1, 0, 0),
                       seasonal = c(1, 0, 0))
sarima_100_100

future = forecast(sarima_100_100, h = 12)
future
autoplot(future)

# берём, если не знаем, какую брать
sarima_111_111 = Arima(y, order = c(1, 1, 1),
                       seasonal = c(1, 1, 1))
sarima_111_111

# вторая рабочая лошадка
auto_model = auto.arima(y)
auto_model







