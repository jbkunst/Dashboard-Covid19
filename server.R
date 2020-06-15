shinyServer(function(input, output, session) {
  
  descargar_datos()
  
  fecha <- reactive({ format(Sys.Date(), "%d de %B, %Y") })
  
  output$fecha1 <- renderText({ fecha() })
  output$fecha2 <- renderText({ fecha() })
  
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
  
  output$hc_examenes_realizados <- renderHighchart(grafico_examenes_realizados_establecimiento())
  
  output$hc_pacientes_uci <- renderHighchart(grafico_pacientes_uci())
  
  output$hc_tasa_letalidad <- renderHighchart(grafico_tasa_letalidad())
  
  output$hc_ventiladores <- renderHighchart(grafico_ventiladores())
  

  # fallecidos --------------------------------------------------------------
  
  output$hc_defunciones_esperadas <- renderHighchart(grafico_defunciones_esperadas())
  
  output$hc_fallecidos_por_region <- renderHighchart(grafico_fallecidos_por_region())
  
  output$rmd_fallecidos_region <- renderUI({
    
    RMD_to_HTML(file = "md/fallecidos_region.Rmd")
    
  })
  
  


  # geografico --------------------------------------------------------------
  
  output$rmd_geografico_region <- renderUI({
    
    RMD_to_HTML(file = "md/geografico_region.Rmd")
    
  })
  
  output$rmd_geografico_gs <- renderUI({
    
    RMD_to_HTML(file = "md/geografico_gs.Rmd")
    
  })

  output$tbl_chile <- DT::renderDataTable({ 
    
    d <- serie_consolidado_region() 
    
    d <- d %>% 
      filter(Fecha == max(Fecha)) %>% 
      ungroup() %>% 
      select(Region, casos_nuevos, examenes, fallecidos, nro_pacientes_uci, Poblacion) %>% 
      arrange(desc(casos_nuevos)) %>% 
      rename(
        `Exámenes` = examenes,
        `Confirmados` = casos_nuevos,
        Fallecidos = fallecidos, 
        `Pacientes UCI` = nro_pacientes_uci
        )

    
    DT::datatable(
      d, 
      rownames = FALSE,
      selection = "single",
      extensions = "Responsive",
      callback =   JS("table.on('click.dt', 'td', function() {
            var data = table.row(this).data();
            Shiny.onInputChange('click_tbl_chile',data);});"),
      options = list(
        searching = FALSE,
        bPaginate = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(className = 'dt-right', targets = 1:5)),
        initComplete = JS(
          "function(settings, json) {",
          "$('td').css({'cursor': 'pointer'});",
          "$('th').css({'cursor': 'pointer'});",
          # "$(this.api().tables().header()).css({'font-family': 'Alegreya Sans SC', sans-serif'});",
          "$(this.api().tables().body()).css({'font-size': '0.9em'});",
          "}")
        )) %>% 
      DT::formatRound(2:6, mark = ".", digits = 0)
      
      
  })
  
  output$hc_map <- renderHighchart({
    
    # print(input$click_tbl_chile)
    
    reg <- input$click_tbl_chile[1]
    
    if(is.null(reg)) reg <- "Metropolitana"
    
    grafico_map(reg)
    
  
  })
  
  output$hc_map_gs <- renderHighchart({ grafico_map_gs() })
  

  # economia ----------------------------------------------------------------
  
  output$rmd_economia_desempleo <- renderUI({
    
    RMD_to_HTML(file = "md/economia_desempleo.Rmd")
    
  })
  
  output$hc_tasa_desempleo <- renderHighchart({ grafico_tasa_desempleo() })
  
  
})
