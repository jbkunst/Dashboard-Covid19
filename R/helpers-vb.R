grafico_vb_confirmados <- function(){
  
  d <- serie_nro_casos()
  f <- d %>% select(dia) %>% pull() %>% last()  
  
  d <- d %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = nro_casos)
  
  lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
  aux <- d %>% 
    tail(8) %>% 
    mutate(z = lag(y)) %>% 
    mutate(v = y-z) %>% 
    select(v) %>% 
    filter(!is.na(v)) %>% 
    pull()
  nuevos_casos <- last(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  total_casos_ult_7_dias <- sum(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  
  hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(pointFormat = "{point.x:%A %e de %B}<br><b>{point.y}</b> confirmardos") %>% 
    hc_plotOptions(
      series = list(
        color = PARS$sparkline_color,
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.0, "transparent"),
            list(0.5, PARS$sparkline_color)
          )
        )
      )
    )
  
  valueBoxSpark(
    value = lbl,
    subtitle = HTML(paste0(nuevos_casos, " casos nuevos", "<br>", total_casos_ult_7_dias, " útlimos 7 días")),
    spark = hc,
    minititle = paste0("Confirmados"),
    color = "blue"
  )
}

grafico_vb_examenes <- function(){
  
  d <- serie_nro_examenes()
  f <- d %>% select(dia) %>% pull() %>% last()  
  
  d <- d %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = nro_examenes)
  
  lbl <- d %>% mutate(y = cumsum(y))  %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
  aux <- d %>% 
    tail(7) %>% 
    select(y) %>% 
    pull()
  nuevos_casos <- last(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  total_casos_ult_7_dias <- sum(aux) %>% comma(big.mark = ".", decimal.mark = ",")   
  
  
  hc <- d %>% 
    mutate(y = cumsum(y)) %>% 
    hchart("areaspline", color = PARS$primary_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.0, "transparent"),
            list(1.0, PARS$primary_color)
          )
        )
      )
    )
  
  valueBoxSpark(
    value = lbl,
    subtitle = HTML(paste0(nuevos_casos, " exámenes nuevos","<br>", total_casos_ult_7_dias, " últimos 7 días")),
    spark = hc,
    minititle = paste0("Exámenes")
  )
}

grafico_vb_fallecidos <- function(){
  d <- serie_nro_fallecidos()
  f <- d %>% select(dia) %>% pull() %>% last()  
  
  d <- d %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = nro_fallecidos)
  
  lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
  aux <- d %>% 
    tail(8) %>% 
    mutate(z = lag(y)) %>% 
    mutate(v = y-z) %>% 
    select(v) %>% 
    filter(!is.na(v)) %>% 
    pull()
  nuevos_casos <- last(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  total_casos_ult_7_dias <- sum(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  
  hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
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
    subtitle = HTML(paste0(nuevos_casos, " nuevos fallecidos", "<br>", total_casos_ult_7_dias, " últimos 7 días")),
    spark = hc,
    minititle = paste0("Fallecidos"),
    color = "blue"
  )
}

grafico_vb_uci <- function(){
  d <- serie_nro_pascientes_UCI()
  f <- d %>% select(dia) %>% mutate_all(date) %>% pull() %>% last()  
  
  d <- d %>%
    mutate(dia = ymd(dia)) %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = nro_pascientes_uci)
  
  lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
  aux <- d %>% 
    tail(8) %>% 
    mutate(z = lag(y)) %>% 
    mutate(v = y-z) %>% 
    select(v) %>% 
    filter(!is.na(v)) %>% 
    pull()
  nuevos_casos <- last(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  total_casos_ult_7_dias <- sum(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  
  hc <- hchart(d, "areaspline", color = PARS$primary_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
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
    subtitle = HTML(paste0(nuevos_casos, " nuevos pacientes UCI", "<br>", total_casos_ult_7_dias, " últimos 7 días")),
    spark = hc,
    minititle = paste0("Pacientes UCI")
  )
}

grafico_vb_recuperados <- function(){
  
  d <- serie_recuperados()
  
  f <- d %>% select(dia) %>% mutate_all(date) %>% pull() %>% last()  
  
  d <- d %>%
    mutate(dia = ymd(dia)) %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    select(x = dia, y = casos_recuperados)
  
  lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
  aux <- d %>% 
    tail(8) %>% 
    mutate(z = lag(y)) %>% 
    mutate(v = y-z) %>% 
    select(v) %>% 
    filter(!is.na(v)) %>% 
    pull()
  nuevos_casos <- last(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  total_casos_ult_7_dias <- sum(aux) %>% comma(big.mark = ".", decimal.mark = ",")
  
  hc <- hchart(d, "areaspline", color = PARS$primary_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
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
    subtitle = HTML(paste0(nuevos_casos, " nuevos recuperado", "<br>", total_casos_ult_7_dias, " últimos 7 días")),
    spark = hc,
    minititle = paste0("Recuperados")
  )
}

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
    hchart("spline", color = PARS$primary_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(
      valueDecimals = 2,
      valueSuffix = " %",
      pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
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
    subtitle = HTML(paste0(round(100 * (d %>% pull(y) %>% last())), " de cada 100 personas<br>contagiadas fallece")),
    spark = hc,
    minititle = paste0("Letalidad")
  )
}

grafico_vb_ventiladores<- function(){
  
  d <- readRDS("data/producto20/NumeroVentiladores_T.rds")
  
  d <- d %>%
    mutate(dia = ymd(Ventiladores)) %>% 
    mutate(dia = datetime_to_timestamp(dia)) %>% 
    mutate(porc = disponibles / total) %>% 
    select(x = dia, y = porc)
  
  lbl <- d %>% pull(y) %>% last() %>% percent(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  
  hc <- d %>% 
    mutate(y = y*100) %>% 
    hchart("spline", color = PARS$primary_color) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_add_theme(hc_theme_sparkline2()) %>% 
    hc_tooltip(
      valueDecimals = 2,
      valueSuffix = " %",
      pointFormat = "{point.x:%A %e de %B}: {point.y}") %>% 
    hc_plotOptions(
      series = list(
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
    subtitle = HTML(paste0(round(100 * (d %>% pull(y) %>% last())), " ventiladores disponibles<br> por cada 100")),
    spark = hc,
    minititle = paste0("Ventiladores")
  )
 
  
  
  }