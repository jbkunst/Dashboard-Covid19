shinyUI(
    fluidPage(
        theme = shinytheme("paper"),
        
        titlePanel("Dashboard-Covid19"),
        
        column(6, highchartOutput("hc_confirmados")),
        column(6, highchartOutput("hc_casos_por100mh")),
        column(6, highchartOutput("hc_confirmados_rango_edad"))
        
        )
    )
