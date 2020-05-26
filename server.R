shinyServer(function(input, output) {

    output$hc_confirmados <- renderHighchart({
        
        grafico_casos_confirmados_diarios()

    })

})
