shinyServer(function(input, output) {

    output$hc_confirmados <- renderHighchart({
        
        grafico_casos_confirmados_diarios()

    })
    
    output$hc_casos_por100mh <- renderHighchart({
      
      grafico_examenes_informados_casos_fallecidos_confirmados()
      
    })
    
    output$hc_confirmados_rango_edad <- renderHighchart({
      
      grafico_casos_confirmados_rango_edad()
      
    })

})
