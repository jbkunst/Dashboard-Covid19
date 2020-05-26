shinyUI(
    fluidPage(
        theme = shinytheme("paper"),
        
        titlePanel("Dashboard-Covid19"),
        
        column(6, highchartOutput("hc_confirmados"))
        
        )
    )
