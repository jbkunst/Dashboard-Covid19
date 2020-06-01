bs4DashPage(
    enable_preloader = TRUE,
    sidebar_collapsed = TRUE,
    sidebar = bs4DashSidebar(
        title = "C191",
        src = "virus (1).png",
        expand_on_hover = FALSE,
        skin = "light",
        bs4SidebarMenu(
            bs4SidebarMenuItem(
                text = "Dashboard",
                tabName = "item1",
                icon =  "tachometer-alt"
                ),
            bs4SidebarMenuItem(
                text = "Geográfico",
                tabName = "item2",
                icon = "map-marker-alt"
                ),
            bs4SidebarMenuItem(
                text = "Acerca de",
                tabName = "item3",
                icon = "question-circle"
                )
            )
        ),
    body = bs4DashBody(
        bs4TabItems(
            bs4TabItem(
                tabName = "item1",
                tags$h2("COVID-19"),
                
                fluidRow(
                    valueBoxOutput("vb_confirmados", 2),
                    valueBoxOutput("vb_examenes", 2),
                    valueBoxOutput("vb_fallecidos", 2),
                    valueBoxOutput("vb_uci", 2)
                ),
                
                fluidRow(
                    bs4Card(
                        status = "primary",
                        solidHeader = TRUE, 
                        collapsible = FALSE,
                        closable = FALSE,
                        elevation = 5,
                        width = 12,
                        bs4TabSetPanel(
                            id = "tabcard",
                            vertical = TRUE,
                            side = "left",
                            bs4TabPanel(
                                tabName = "Confirmados Diarios",
                                bs4CardHC(addSpinner(highchartOutput("hc_confirmados")))
                            ),
                            bs4TabPanel(
                                tabName = "Fallecidos Semanales",
                                bs4CardHC(addSpinner(highchartOutput("hc_fallecidos_por_anio")))
                            ),
                            bs4TabPanel(
                                tabName = "Gran Santiago",
                                bs4CardHC()
                            ),
                            bs4TabPanel(
                                tabName = "Tab 4",
                                bs4CardHC()
                                )
                            )
                        )
                    )
                )
            )
        )
    )
