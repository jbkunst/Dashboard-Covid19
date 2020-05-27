
grafico_casos_confirmados_diarios <- function(){
  dcasos_totales_cumulativos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv")
  dcasos_totales_cumulativos <- dcasos_totales_cumulativos %>%
    gather(fecha, casos, -Region) %>%
    filter(Region == "Total") %>%
    mutate(fecha = date(fecha),
           casos_nuevos = lag(casos),
           casos_nuevos = ifelse(is.na(casos_nuevos), casos, casos - casos_nuevos),
           media_movil = roll_mean(casos_nuevos, n = 7, fill = NA, align = "right"))
  
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

grafico_examenes_informados_casos_fallecidos_confirmados <- function(){
  dcasos_totales_cumulativos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv")
  dcasos_fallecidos <- read_csv('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-04-08-CasosConfirmados-totalRegional.csv')
  dcasos_examenes <- read_csv('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto7/PCR.csv')
  
  dcasos_totales_hoy <- dcasos_totales_cumulativos %>% 
    gather(dia, nro_casos, -Region) %>%
    filter(dia == max(date(dia)), !str_detect(Region, "Total")) %>% 
    select(-dia)
  
  dcasos_examenes %>% 
    gather(dia, nro_examenes, -Region, -`Codigo region`, -Poblacion) %>% 
    mutate(nro_examenes = replace_na(nro_examenes, 0)) %>% 
    group_by(Region, `Codigo region`, Poblacion) %>% 
    summarise(nro_examenes = sum(nro_examenes)) %>% 
    ungroup() %>% 
    mutate(nro_examenes_x100mh = 1e5*nro_examenes/Poblacion) %>% 
    left_join(dcasos_totales_hoy) %>% 
    mutate(nro_casos_x100mh = 1e5*nro_casos/Poblacion)
  
  

  
  }

