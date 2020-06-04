bs4DashPage(
    enable_preloader = TRUE,
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
        title = tags$small("COVID-19 Chile 20"),
        src = "virus (1).png",
        expand_on_hover = TRUE,
        fixed = FALSE,
        skin = "light",
        bs4SidebarMenu(
            bs4SidebarMenuItem(
                text = "Inicio",
                tabName = "inicio",
                icon =  "tachometer-alt"
                ),
            bs4SidebarMenuItem(
                text = "Geográfico",
                tabName = "item2",
                icon = "map-marker-alt"
                ),
            bs4SidebarMenuItem(
                text = "Fallecidos",
                tabName = "fallecidos",
                icon = "skull"
            ),
            bs4SidebarMenuItem(
                text = "Acerca de",
                tabName = "acerca",
                icon = "question-circle"
                )
            )
        ),
    body = bs4DashBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")),
        bs4TabItems(
            bs4TabItem(
                tabName = "inicio",
                tags$h2(icon("tachometer-alt"), "Inicio", class = "titulo"),
                fluidRow(
                    valueBoxOutput("vb_confirmados", 2),
                    valueBoxOutput("vb_fallecidos", 2),
                    valueBoxOutput("vb_examenes", 2),
                    valueBoxOutput("vb_letalidad", 2),
                    valueBoxOutput("vb_ventiladores", 2),
                    valueBoxOutput("vb_uci", 2)
                    
                ),
                
                fluidRow(
                    bs4CardCustom(
                        bs4TabSetPanel(
                            id = "tabcard",
                            vertical = TRUE,
                            side = "left",
                            bs4TabPanel(
                                tabName = "Confirmados Diarios",
                                bs4CardHC(addSpinner(highchartOutput("hc_confirmados")))
                            ),
                            bs4TabPanel(
                                tabName = "Fallecidos Diarios",
                                bs4CardHC(addSpinner(highchartOutput("hc_fallecidos_diarios")))
                            ),
                            bs4TabPanel(
                                tabName = "Exámenes Diarios",
                                bs4CardHC(addSpinner(highchartOutput("hc_examenes_realizados")))
                            ),
                            bs4TabPanel(
                                tabName = "Tasa Letalidad",
                                bs4CardHC(addSpinner(highchartOutput("hc_tasa_letalidad")))
                                ),
                            bs4TabPanel(
                                tabName = "Ventiladores",
                                bs4CardHC(addSpinner(highchartOutput("hc_ventiladores")))
                            ),
                            bs4TabPanel(
                                tabName = "Pacientes UCI",
                                bs4CardHC(addSpinner(highchartOutput("hc_pacientes_uci")))
                            )
                            )
                        )
                    )
                ),
            bs4TabItem(
                tabName = "fallecidos",
                tags$h2(icon("skull"), "Fallecidos", class = "titulo"),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            column(width = 4, shinipsum::random_text(nwords = 50)),
                            column(width = 8, bs4CardHC(addSpinner(highchartOutput("hc_defunciones_esperadas"))))
                            )
                        )
                    )
                ),
            bs4TabItem(
                tabName = "acerca",
                tags$h2(icon("question-circle"), "Acerca de esta aplicación", class = "titulo"),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            column(width = 4, shinipsum::random_text(nwords = 50)),
                            column(width = 4, shinipsum::random_text(nwords = 50)),
                            column(width = 4, shinipsum::random_text(nwords = 50))
                            )
                        ),
                    ),
                fluidRow(
                    bs4UserCard(
                        src = "https://pbs.twimg.com/profile_images/1246097605521793024/4CcubJFq_400x400.jpg",
                        title = "Alonso Silva",
                        subtitle = "CEO",
                        width = 3,
                        includeMarkdown("md/bio_alonso.md")
                    ),
                    bs4UserCard(
                        src = "https://avatars1.githubusercontent.com/u/4108139?s=400&v=4",
                        title = HTML("Héctor <i>Tito</i> González"),
                        subtitle = "CEO",
                        width = 3,
                        shinipsum::random_text(nwords = 50)
                    ),
                    bs4UserCard(
                        src = "https://pbs.twimg.com/profile_images/1246097605521793024/4CcubJFq_400x400.jpg",
                        title = "Alonso Silva",
                        subtitle = "CEO",
                        width = 3,
                        shinipsum::random_text(nwords = 50)
                    ),
                    bs4UserCard(
                        src = "https://pbs.twimg.com/profile_images/1246097605521793024/4CcubJFq_400x400.jpg",
                        title = "Alonso Silva",
                        subtitle = "CEO",
                        width = 3,
                        shinipsum::random_text(nwords = 50)
                        )
                    )
                )
            )
        )
    )
