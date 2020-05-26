grafico_casos_confirmados_diarios <- function(){
  # ruta
  rcasos_totales_cumulativos <-
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv"
  # lectura
  dcasos_totales_cumulativos <- read_csv(rcasos_totales_cumulativos)
  #grafico
  dcasos_totales_cumulativos <-
    dcasos_totales_cumulativos %>%
    gather(fecha, casos, -Region) %>%
    filter(Region == "Total") %>%
    mutate(
      fecha = date(fecha),
      casos_nuevos = lag(casos),
      casos_nuevos = ifelse(is.na(casos_nuevos), casos, casos - casos_nuevos),
      media_movil = roll_mean(
        casos_nuevos,
        n = 7,
        fill = NA,
        align = "right"))
  dcasos_totales_cumulativos %>% 
    hchart(hcaes(fecha, casos_nuevos), type = "column") %>% 
    hc_add_series_times_values(dcasos_totales_cumulativos$fecha, dcasos_totales_cumulativos$media_movil,
                               color = "red")
}
