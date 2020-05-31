shinyServer(function(input, output) {
  
  output$vb_confirmados <- renderValueBox({
    
    d <- serie_nro_casos()
    f <- d %>% select(dia) %>% pull() %>% last()  
    
    d <- d %>% 
      mutate(dia = datetime_to_timestamp(dia)) %>% 
      select(x = dia, y = nro_casos)
    
    lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
    nuevos_casos <- d %>% tail(2) %>% select(y) %>% mutate(c = row_number()) %>% spread(c, y) %>% 
      mutate(nuevos_casos = `2` - `1`) %>% select(nuevos_casos) %>% pull() %>% comma(big.mark = ".", decimal.mark = ",")
    
    
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
      subtitle = paste0(nuevos_casos, " nuevos casos respecto el ", format(f-2, "%d de %b")),
      color = "black",
      spark = hc,
      minititle = "Total Casos Confirmados"
    )
    
  })
  
  output$vb_examenes <- renderValueBox({
    
    d <- serie_nro_examenes()
    f <- d %>% select(dia) %>% pull() %>% last()  
    
    d <- d %>% 
      mutate(dia = datetime_to_timestamp(dia)) %>% 
      select(x = dia, y = nro_examenes)
    
    lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
    nuevos_examenes <- d %>% tail(2) %>% select(y) %>% mutate(c = row_number()) %>% spread(c, y) %>% 
      mutate(nuevos_examenes = `2` - `1`) %>% select(nuevos_examenes) %>% pull() %>% comma(big.mark = ".", decimal.mark = ",")
    
    hc <- hchart(d, "area", color = PARS$sparkline_color) %>% 
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
      subtitle = paste0(nuevos_examenes, " nuevos exámenes respecto el ", format(f-2, "%d de %b")),
      color = "black",
      spark = hc,
      minititle = "Total Exámenes Realizados"
    )
    
  })
  
  output$vb_fallecidos <- renderValueBox({
    
    d <- serie_nro_fallecidos()
    f <- d %>% select(dia) %>% pull() %>% last()  
    
    d <- d %>% 
      mutate(dia = datetime_to_timestamp(dia)) %>% 
      select(x = dia, y = nro_fallecidos)
    
    lbl <- d %>% pull(y) %>% last() %>% comma(big.mark = ".", decimal.mark = ",")
    nuevos_fallecidos <- d %>% tail(2) %>% select(y) %>% mutate(c = row_number()) %>% spread(c, y) %>% 
      mutate(nuevos_examenes = `2` - `1`) %>% select(nuevos_examenes) %>% pull() %>% comma(big.mark = ".", decimal.mark = ",")
    
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
      subtitle = paste0(nuevos_examenes, " nuevos fallecidos respecto el ", format(f-2, "%d de %b")),
      color = "black",
      spark = hc,
      minititle = "Total Fallecidos"
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
    nuevos_uci <- d %>% tail(2) %>% select(y) %>% mutate(c = row_number()) %>% spread(c, y) %>% 
      mutate(nuevos_examenes = `2` - `1`) %>% select(nuevos_examenes) %>% pull() %>% comma(big.mark = ".", decimal.mark = ",")
    
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
      subtitle = paste0(nuevos_uci, " nuevos pascientes en UCI respecto el ", format(f-2, "%d de %b")),
      color = "black",
      spark = hc,
      minititle = "Total Pascientes en UCI"
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
