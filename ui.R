dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        column(
            12,
            valueBoxOutput("vb_confirmados", 3)
        ),
        
        
        column(6, highchartOutput("hc_confirmados")),
        column(6, highchartOutput("hc_casos_por100mh")),
        column(6, highchartOutput("hc_confirmados_rango_edad"))
        )
    )