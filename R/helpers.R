
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
  dcasos_fallecidos_0804 <- read_csv('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-04-08-CasosConfirmados-totalRegional.csv')
  dcasos_examenes <- read_csv('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto7/PCR.csv')
  dcasos_fallecidos <-  read_csv(paste0('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/', today()-1,'-CasosConfirmados-totalRegional.csv'))
  
  dcasos_totales_hoy <- dcasos_totales_cumulativos %>% 
    gather(dia, nro_casos, -Region) %>%
    filter(dia == max(date(dia)), !str_detect(Region, "Total")) %>% 
    select(-dia)
  
  dcasos_fallecidos2 <- dcasos_fallecidos %>% 
    select(Region, nro_fallecidos = Fallecidos) %>% 
    mutate(Region = case_when(
      str_detect(Region, "Arauca") ~ "Araucanía",
      str_detect(Region, "Arica") ~ "Arica y Parinacota",
      str_detect(Region, "Ays") ~ "Aysén",
      str_detect(Region, "Bio") ~ "Biobío",
      str_detect(Region, "Lagos") ~ "Los Lagos",
      str_detect(Region, "Rios") ~ "Los Ríos",
      str_detect(Region, "Nuble") ~ "Ñuble",
      str_detect(Region, "O'Higgins") ~ "O’Higgins",
      str_detect(Region, "Tara") ~ "Tarapacá",
      str_detect(Region, "Valpa") ~ "Valparaíso",
      TRUE ~ Region
    ))
  
  d <- dcasos_examenes %>% 
    gather(dia, nro_examenes, -Region, -`Codigo region`, -Poblacion) %>% 
    mutate(nro_examenes = replace_na(nro_examenes, 0)) %>% 
    group_by(Region, `Codigo region`, Poblacion) %>% 
    summarise(nro_examenes = sum(nro_examenes)) %>% 
    ungroup() %>% 
    mutate(nro_examenes_x100mh = 1e5*nro_examenes/Poblacion) %>% 
    left_join(dcasos_totales_hoy) %>% 
    mutate(nro_casos_x100mh = 1e5*nro_casos/Poblacion) %>% 
    left_join(dcasos_fallecidos2) %>% 
    mutate(nro_fallecidos_x100mh = 1e5*nro_fallecidos/Poblacion) %>% 
    arrange(-nro_examenes_x100mh) %>% 
    mutate(Region = fct_inorder(Region))
  
 
   hchart(d, "bar",
          hcaes(Region, nro_examenes_x100mh),
          name = "Nro de exámenes por 100 mil habitantes") %>% 
     hc_add_series(
       d, "bar",
       hcaes(Region, nro_casos_x100mh),
       name = "Nro de casos por 100 mil habitantes"
     ) %>% 
     hc_add_series(
       d, "bar",
       hcaes(Region, nro_fallecidos_x100mh),
       name = "Nro de fallecidos por 100 mil habitantes"
     ) %>% 
     hc_tooltip(table = TRUE, valueDecimals = 0)
    
    
  
  }

grafico_casos_confirmados_rango_edad <- function(){
  dcasos_confirmados_rango_edad <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario.csv") %>%
    gather(-`Grupo de edad`, -Sexo, key = "fecha", value="n") %>% 
    mutate(fecha = as_date(fecha, format="%Y-%m-%d"))
  
  hchart(
    dcasos_confirmados_rango_edad,
    type = "column",
    hcaes(x=fecha, y=n, group=`Grupo de edad`),
    name = "n"
  ) %>% 
    hc_title(
      text = "COVID-19 en Chile: Evolución de casos confirmados por rango de edad"
    )
}