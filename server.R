shinyServer(function(input, output, session) {
  
  figletr::figlet("Descargar datos")
  descargar_datos()
  
  output$vb_confirmados <- renderValueBox({
    
    grafico_vb_confirmados()
    
  })
  
  output$vb_examenes <- renderValueBox({
    
    grafico_vb_examenes()
    
  })
  
  output$vb_fallecidos <- renderValueBox({
    
    grafico_vb_fallecidos()
    
  })
  
  output$vb_uci <- renderValueBox({
    
    grafico_vb_uci()
    
  })
  
  output$vb_letalidad <- renderValueBox({
    
    grafico_vb_letalidad()
    
  })
  
  output$vb_recuperados <- renderValueBox({
    
    grafico_vb_recuperados()
    
  })
  
  output$vb_ventiladores <- renderValueBox({
    
    grafico_vb_ventiladores()
    
  })
  
  output$hc_confirmados <- renderHighchart({
    
    grafico_casos_confirmados_diarios()
    
  })
  
  output$hc_examenes_realizados <- renderHighchart({
    
    grafico_examenes_realizados()
    
  })
  
  output$hc_fallecidos_diarios <- renderHighchart({
    
    grafico_fallecidos_diarios()
    
  })
  
  output$hc_recuperados_diarios <- renderHighchart({
    
    grafico_recuperados_diarios()
    
  })
  
  output$hc_pacientes_uci <- renderHighchart({
    
    grafico_pacientes_uci()
    
  })
  
  output$hc_tasa_letalidad <- renderHighchart({
    
    grafico_tasa_letalidad()
    
  })
  
  output$hc_casos_por100mh <- renderHighchart({
    
    grafico_examenes_informados_casos_fallecidos_confirmados()
    
  })
  
  output$hc_confirmados_rango_edad <- renderHighchart({
    
    grafico_casos_confirmados_rango_edad()
    
  })
  
  output$hc_defunciones_esperadas <- renderHighchart({
    
    grafico_defunciones_esperadas()
    
  })
  
  output$hc_ventiladores <- renderHighchart({
    
    grafico_ventiladores()
    
  })
  
})
