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
      options = list(
        searching = FALSE,
        bPaginate = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(className = 'dt-right', targets = 1:5))
        )
      ) %>% 
      DT::formatRound(2:6, mark = ".", digits = 0)
      
      
  })
  
  output$hc_map <- renderHighchart({
    
    grafico_map_chile("variable") 
  
    })
  
})
