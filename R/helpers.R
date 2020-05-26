grafico_casos_confirmados_diarios <- function(){
  
  dcasos_totales_cumulativos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv")
  
  dcasos_totales_cumulativos <- dcasos_totales_cumulativos %>%
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
  
  hchart(
    dcasos_totales_cumulativos,
    type = "column",
    hcaes(fecha, casos_nuevos),
    name = "Casos confirmados diarios"
    ) %>% 
    hc_add_series(
      dcasos_totales_cumulativos, "line",
      hcaes(datetime_to_timestamp(fecha), media_movil),
      name = "Media movil último 7 días"
      ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0)
  
}
