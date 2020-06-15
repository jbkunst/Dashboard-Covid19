library(forecast)

d <- serie_fallecidos_anio_semana()

dnormal <- d %>% 
  arrange(anio, nro_semana) %>% 
  filter(nro_semana != 53) %>% 
  filter(anio <= 2019)

x <- ts(
  dnormal$nro_fallecidos,
  frequency = 52
)

plot(x)

mod <- auto.arima(x, trace = TRUE)

saveRDS(mod, "data/arima_mod.rds")

tsdiag(mod, 52*3)

plot(mod)

fct <- forecast(mod, h = 52)

plot(fct)

