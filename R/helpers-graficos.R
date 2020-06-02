
grafico_vb_letalidad <- function(){
  d <- serie_letalidad()
  f <- d %>% select(dia) %>% mutate_all(date) %>% pull() %>% last()  
  
  d <- d %>%
    mutate(dia = ymd(dia)) %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = porc)
  
  lbl <- d %>% pull(y) %>% last() %>% percent(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  aux <- d %>% 
    tail(7) %>% 
    select(y) %>% 
    pull()
  nuevos_casos <- last(aux) %>% percent(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  total_casos_ult_7_dias <- mean(aux) %>% percent(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  
  hc <- d %>% 
    mutate(y = y*100) %>% 
    hchart("line", color = PARS$sparkline_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(
      valueDecimals = 2,
      valueSuffix = " %",
      pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
        color = PARS$sparkline_color,
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.0, "transparent"),
            list(1.0, PARS$sparkline_color)
          )
        )
      )
    )
  
  valueBoxSpark(
    value = lbl,
    subtitle = HTML(paste0(total_casos_ult_7_dias, " promedio tasa últimos 7 días")),
    spark = hc,
    minititle = paste0("Tasa de Letalidad")
  )
}

grafico_casos_confirmados_diarios <- function(){
  
  dcasos_totales_cumulativos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv")
  
  dcasos_totales_cumulativos <- dcasos_totales_cumulativos %>%
    gather(fecha, casos, -Region) %>%
    filter(Region == "Total") %>%
    mutate(
      fecha = date(fecha),
      casos_nuevos = lag(casos),
      casos_nuevos = ifelse(is.na(casos_nuevos), casos, casos - casos_nuevos),
      media_movil = roll_mean(casos_nuevos, n = 7, fill = NA, align = "right"),
      media_movil = round(media_movil, 0)
    )
  
  
  evento <- tibble(
    fecha = c(ymd("2020-04-29"), ymd("2020-05-15")),
    texto = c("Se suman casos<br>asintomáticos", "Inicio cuarentena<br>en la RM")
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  h1 <- hchart(
    dcasos_totales_cumulativos,
    type = "line",
    hcaes(fecha, casos_nuevos),
    name = "Casos confirmados diarios",
    showInLegend = TRUE,
    color = PARS$primary_color,
    lineWidth = 4
  ) %>% 
    hc_add_series(
      dcasos_totales_cumulativos, "line",
      hcaes(datetime_to_timestamp(fecha), media_movil),
      name = "Media móvil últimos 7 días",
      color = "gray",
      showInLegend = TRUE,
      size = 2
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de casos")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
  h1
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
  
  
  hchart(d, "area",
         hcaes(Region, nro_examenes_x100mh),
         name = "Exámenes",
         showInLegend = TRUE) %>% 
    hc_add_series(
      d, "area",
      hcaes(Region, nro_casos_x100mh),
      name = "Casos",
      showInLegend = TRUE
    ) %>% 
    hc_add_series(
      d, "area",
      hcaes(Region, nro_fallecidos_x100mh),
      name = "Fallecidos",
      showInLegend = TRUE
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_yAxis(
      title = list(text = "Nº de Casos")
    ) 
  
  
  
}

grafico_casos_confirmados_rango_edad <- function(){
  
  dcasos_confirmados_rango_edad <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario.csv") %>%
    gather(-`Grupo de edad`, -Sexo, key = "fecha", value="n") %>% 
    mutate(fecha = as_date(fecha, format="%Y-%m-%d")) %>% 
    separate(`Grupo de edad`, c("min","max")) %>% 
    mutate_at(c("min", "max"), ~as.numeric(.)) %>% 
    mutate(Grupo_edad = case_when(
      max <= 39 ~ "<= 39",
      max <= 49 ~ "40-49",
      max <= 59 ~ "50-59",
      max <= 69 ~ "60-69",
      max <= 79 ~ "70-79",
      min >= 80 ~ ">= 80",
      TRUE ~ NA_character_
    )) %>% 
    group_by(fecha, Grupo_edad) %>% 
    summarise(n = sum(n)) %>% 
    ungroup()
  
  hchart(
    dcasos_confirmados_rango_edad,
    type = "column",
    hcaes(x=fecha, y=n, group=Grupo_edad)
  ) %>% 
    hc_plotOptions(
      series = list(
        stacking = list(enabled = TRUE)  
      )
    ) %>% 
    hc_tooltip(
      shared = TRUE
    ) %>% 
    hc_exporting(
      enabled = TRUE
    ) %>% 
    hc_yAxis(
      title = list(text = "Nº de Casos")
    ) 
}

grafico_defunciones_anuales <- function(){
  
  dcasos_fallecidos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto32/Defunciones.csv")
  
  d <- dcasos_fallecidos %>% 
    gather(dia, nro_fallecidos, -Region, -`Codigo region`, -Comuna, -`Codigo comuna`) %>% 
    mutate(
      dia = ymd(dia),
      nro_semana = week(dia),
      anio = year(dia)) %>% 
    group_by(nro_semana, anio) %>% 
    summarise(nro_fallecidos = sum(nro_fallecidos)) %>% 
    ungroup() %>% 
    arrange(-anio) %>% 
    mutate(anio = as.character(anio)) %>% 
    mutate( anio = fct_inorder(anio))
  # 
  # d %>% 
  #   group_by(anio) %>% 
  #   summarise(nro_fallecidos = sum(nro_fallecidos))
  
  d %>%
    group_by(anio) %>% 
    filter(nro_semana != max(nro_semana)) %>% 
    ungroup() %>% 
    hchart(
      c("area", rep("line", 10)),  
      hcaes(nro_semana, nro_fallecidos, group = anio),
      visible = c(rep(TRUE, 3), rep(FALSE, 8))
      # color = c(rep("grey", 10), "red")
    ) %>% 
    hc_tooltip(
      split = TRUE
    ) %>% 
    hc_exporting(
      enabled = TRUE
    ) %>% 
    hc_yAxis(
      title = list(text = "Nº de defunciones")
    )  %>% 
    hc_xAxis(
      title = list(text = "Semana")
    ) 
  
}

grafico_tasa_letalidad <- function(){
  
  dfallecidos <- serie_nro_fallecidos()
  dcontagiados <- serie_nro_casos()
  dfallecidos_contagiados <- dfallecidos %>% 
    full_join(
      dcontagiados,
      by="dia"
    ) %>% 
    mutate(porc=nro_fallecidos/nro_casos) 
  
  dfallecidos_contagiados %>% 
    mutate(porc = porc*100) %>% 
    hchart(., "line",
           hcaes(dia, porc),
           color = "black",
           name = "Tasa de Letalidad"
    ) %>% 
    hc_yAxis(
      allowDecimals = TRUE,
      title = list(text="%")
    ) %>% 
    hc_xAxis(
      title = list(text="")
    ) %>% 
    hc_tooltip(
      valueDecimals = 2,
      pointFormat = " {series.name}: <b>{point.y}</b> ({point.nro_fallecidos}/{point.nro_casos}) <br/>",
      valueSuffix = " %",
      split = TRUE
    )
}
