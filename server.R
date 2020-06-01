shinyServer(function(input, output) {
  
  output$vb_confirmados <- renderValueBox({
    
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
      subtitle = paste0("Nuevos: ", nuevos_casos, ". Ult. 7 días: ", total_casos_ult_7_dias),
      color = "black",
      spark = hc,
      minititle = paste0("Total Casos Confirmados al ", format(f, "%d de %b del %Y"))
    )
    
  })
  
  output$vb_examenes <- renderValueBox({
    
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
      hchart("area", color = PARS$sparkline_color) %>% 
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
      subtitle = paste0("Nuevos: ", nuevos_casos, ". Ult. 7 días: ", total_casos_ult_7_dias),
      color = "black",
      spark = hc,
      minititle = paste0("Total Exámenes Realizados al ", format(f, "%d de %b del %Y"))
    )
    
  })
  
  output$vb_fallecidos <- renderValueBox({
    
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
      subtitle = paste0("Nuevos: ", nuevos_casos, ". Ult. 7 días: ", total_casos_ult_7_dias),
      color = "black",
      spark = hc,
      minititle = paste0("Total Fallecidos al ", format(f, "%d de %b del %Y"))
    )
    
  })
  
  output$vb_uci <- renderValueBox({
    
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
      subtitle = paste0("Nuevos: ", nuevos_casos, ". Ult. 7 días: ", total_casos_ult_7_dias),
      color = "black",
      spark = hc,
      minititle = paste0("Total Pacientes en UCI al ",  format(f, "%d de %b"))
    )
    
  })
  
  output$hc_confirmados <- renderHighchart({
    
    grafico_casos_confirmados_diarios()
    
  })
  
  output$hc_casos_por100mh <- renderHighchart({
    
    grafico_examenes_informados_casos_fallecidos_confirmados()
    
  })
    
  output$hc_confirmados_rango_edad <- renderHighchart({
  
    grafico_casos_confirmados_rango_edad()
    
  })
  
  output$hc_fallecidos_por_anio <- renderHighchart({
    
    grafico_defunciones_anuales()
    
  })

})
