shinyServer(function(input, output, session) {
  
  descargar_datos()
  
  observeEvent(input$vb_chart,{
    
    tab <- switch(input$vb_chart,
      "confirmados" = "Confirmados Diarios",
      "fallecidos" = "Fallecidos Diarios",
      "examenes" = "Exámenes Diarios",
      "letalidad" = "Tasa Letalidad",
      "ventiladores" = "Ventiladores",
      "uci" = "Pacientes UCI",
    )
    
    updatebs4TabItems(session, inputId = "tabcard", selected = tab)
    
  })
  

  # valueBoxes --------------------------------------------------------------
  output$vb_confirmados <- renderValueBox(grafico_vb_confirmados())
  
  output$vb_examenes <- renderValueBox(grafico_vb_examenes())
  
  output$vb_fallecidos <- renderValueBox(grafico_vb_fallecidos())
  
  output$vb_uci <- renderValueBox(grafico_vb_uci())
  
  output$vb_letalidad <- renderValueBox(grafico_vb_letalidad())
  
  output$vb_recuperados <- renderValueBox(grafico_vb_recuperados())
  
  output$vb_ventiladores <- renderValueBox(grafico_vb_ventiladores())
  

  # dashboard ---------------------------------------------------------------
 
  output$hc_confirmados <- renderHighchart(grafico_casos_confirmados_diarios())
  
  output$hc_fallecidos_diarios <- renderHighchart(grafico_fallecidos_diarios())
  
  #output$hc_examenes_realizados <- renderHighchart(grafico_examenes_realizados())
  output$hc_examenes_realizados <- renderHighchart(grafico_examenes_realizados_establecimiento())
  
  output$hc_pacientes_uci <- renderHighchart(grafico_pacientes_uci())
  
  output$hc_tasa_letalidad <- renderHighchart(grafico_tasa_letalidad())
  
  output$hc_ventiladores <- renderHighchart(grafico_ventiladores())
  

  # fallecidos --------------------------------------------------------------
  
  output$hc_defunciones_esperadas <- renderHighchart(grafico_defunciones_esperadas())
  
  output$hc_fallecidos_por_region <- renderHighchart(grafico_fallecidos_por_region())


  # geografico --------------------------------------------------------------

  output$tbl_chile <- DT::renderDataTable({ 
    
    d <- serie_consolidado_region() %>% 
      group_by(Region, `Codigo region`, Poblacion) %>% 
      summarise_if(is.numeric, sum) %>% 
      ungroup() %>% 
      arrange(desc(casos_nuevos)) %>% 
      rename(
        `Exámenes` = examenes,
        `Casos Nuevos` = casos_nuevos,
        Fallecidos = fallecidos, 
        `Pacientes UCI` = nro_pacientes_uci
        )
    
    total <- d %>% 
      summarise_if(is.numeric, sum) %>% 
      mutate(Region = "Chile")
    
    d <- bind_rows(total, d) %>% 
      select(-`Codigo region`) %>% 
      select(Region, everything())
    
    DT::datatable(
      d, 
      caption = "Click sobre una fila para ver detalles en el mapa",
      rownames = FALSE, # para negrita formatStyle
      selection = "single",
      callback =   JS("table.on('click.dt', 'td', function() {
            var data = table.row(this).data();
            Shiny.onInputChange('click_tbl_chile',data);});"),
      options = list(
        searching = FALSE,
        bPaginate = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(className = 'dt-right', targets = 1:5))
        )) %>% 
      DT::formatRound(2:6, mark = ".", digits = 0)
      
      
  })
  
  output$hc_map <- renderHighchart({
    
    print(input$click_tbl_chile)
    
    reg <- input$click_tbl_chile[1]
    if(is.null(reg)) reg <- "Metropolitana"
    # reg <- "Tarapacá"
    
    d <- serie_nro_casos_comuna()
    
    cod_region <- d %>% distinct(Region, `Codigo region`)
    
    d <- d %>% 
      filter(Region == reg) %>% 
      group_by(Comuna, `Codigo comuna`) %>% 
      summarise(value = sum(`Casos confirmados`)) %>% 
      ungroup()
    
    get_dir_json_region<- function(reg = "Metropolitana"){
      
      cod_reg <- cod_region %>% 
        filter(Region == reg) %>% 
        select(`Codigo region`) %>% 
        pull()

      dir("data/geojson/", full.names = TRUE) %>% 
        str_subset(cod_reg)
    }
    
    url_gs_geojson <- get_dir_json_region(reg)
    
    gjson <- jsonlite::fromJSON(url_gs_geojson)
    
    # str(gjson)
    
    df <- gjson$features$properties
    # df %>% glimpse()
    df <- df %>% 
      left_join(d, by = c("codigo_comuna" = "Codigo comuna"))
    #d %>% tail(10)
    
    gjson <- geojsonio::as.json(gjson)
    
    highchart(type = "map") %>%
      hc_add_series(
        mapData = gjson,
        data = list_parse(df),
        # "COMUNA" es la key en el geojson, "code" es la key en nuestros datos: dvar
        joinBy = c("codigo_comuna", "codigo_comuna"),
        showInLegend = FALSE,
        name = "Numero de Fallecidos",
        dataLabels = list(enabled = TRUE, format = "{point.Comuna}", color = "white")
      ) %>% 
      hc_colorAxis(
        stops = color_stops(n = 10, colors = viridis_pal(option = "B")(10)),
        startOnTick = FALSE,
        endOnTick =  FALSE
      ) %>%
      hc_legend(
        align = "right",
        layout = "vertical",
        verticalAlign = "middle",
        symbolHeight = 500
      ) %>% 
      hc_tooltip(
        shared = TRUE,
        valueDecimals = 0
      )
  })
  
})
