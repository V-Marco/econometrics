library(rstan)
library(bayesplot)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE) # компилируем один раз

coin_model = stan_model(file = 'coin.stan')

coin_data = list(y = c(1, 1, 1, 0, 0, 1), N = 6)

post_sample = sampling(coin_model, coin_data)

post_sample # сгененрировал 4 выборки (chains) из апостериорного распределения, каждая содержала
# 2000 наблюдений; но так как это наблюдения, сходящиеся к апостериорному, первые тысячу наблюдений
# он сжёг (так как далеко от сходимости); итого, 4 выборки по 1000 наблюдений.

post_array = as.array(post_sample)
mcmc_hist(post_array)
# вероятность, что y = 1, равна 0.59.
