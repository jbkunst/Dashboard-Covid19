dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        h1(strong("COVID-19 en Chile")),
        hr(),
        br(),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        column(
            12,
            valueBoxOutput("vb_confirmados", 3),
            valueBoxOutput("vb_examenes", 3),
            valueBoxOutput("vb_fallecidos", 3),
            valueBoxOutput("vb_uci", 3)
        ),
        
        
        column(6, highchartOutput("hc_confirmados")),
        column(6, highchartOutput("hc_casos_por100mh")),
        column(6, highchartOutput("hc_confirmados_rango_edad")),
        column(6, highchartOutput("hc_fallecidos_por_anio")),
        )
    )