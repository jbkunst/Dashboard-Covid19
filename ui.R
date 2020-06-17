bs4DashPage(
    enable_preloader = TRUE,
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
        title = tags$small("COVID-19 · Chile2020"),
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
                tabName = "geografico",
                icon = "map-marker-alt"
                ),
            bs4SidebarMenuItem(
                text = "Fallecidos",
                tabName = "fallecidos",
                icon = "skull"
            ),
            bs4SidebarMenuItem(
                text = "Economía",
                tabName = "economia",
                icon = "money-check-alt"
            ),
            bs4SidebarMenuItem(
                text = "Acerca de",
                tabName = "acerca",
                icon = "question-circle"
                ),
            bs4SidebarMenuItem(
                text = textOutput("fecha2", inline = TRUE),
                tabName = "",
                icon = "calendar"
            )
            )
        ),
    body = bs4DashBody(
        tags$head(tags$link(rel="shortcut icon", href="fa.png")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")),
        tags$script(src = "js/custom.js"),
        bs4TabItems(
            
# inicio ------------------------------------------------------------------
            
            bs4TabItem(
                tabName = "inicio",
                tags$h2(icon("tachometer-alt"), "Inicio", tags$small(" · datos al ", textOutput("fecha1", inline = TRUE))),
                tags$hr(),
                fluidRow(
                    
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_confirmados", 12), class  = PARS$classcol),
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_fallecidos", 12), class  = PARS$classcol),
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_examenes", 12), class  = PARS$classcol),
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_letalidad", 12), class  = PARS$classcol),
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_ventiladores", 12), class  = PARS$classcol),
                    htmltools::tagAppendAttributes(valueBoxOutput("vb_uci", 12), class  = PARS$classcol)
                    
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

# geografico --------------------------------------------------------------

            bs4TabItem(
                tabName = "geografico",
                tags$h2(icon("map-marker-alt"), "Geográfico"),
                tags$hr(),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            column(6, uiOutput("rmd_geografico_region"))
                        ),
                        tags$br(),
                        fluidRow(
                            column(6, DT::dataTableOutput("tbl_chile")),
                            column(6, bs4CardHC(addSpinner(highchartOutput("hc_map", height = "600px"))))
                        )
                    ),
                    bs4CardCustom(
                        fluidRow(
                            column(4, uiOutput("rmd_geografico_gs")),
                            column(8, bs4CardHC(addSpinner(highchartOutput("hc_map_gs", height = "800px"))))
                        )
                    )
                )
            ),


# fallecido ---------------------------------------------------------------

            bs4TabItem(
                tabName = "fallecidos",
                tags$h2(icon("skull"), "Fallecidos"),
                tags$hr(),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            column(width = 3, uiOutput("rmd_fallecidos_region")),
                            column(width = 9, bs4CardHC(addSpinner(highchartOutput("hc_fallecidos_por_region", height = 600))))
                        )
                    ),
                    bs4CardCustom(
                        fluidRow(
                            column(width = 4, includeMarkdown("md/fallecidos_exceso_mortalidad.md")),
                            column(width = 8, bs4CardHC(addSpinner(highchartOutput("hc_defunciones_esperadas")))),
                            ),
                        tags$hr(),
                        fluidRow(
                            column(width = 8, bs4CardHC(addSpinner(highchartOutput("hc_defunciones_esperadas_v2")))),
                            column(width = 4, uiOutput("rmd_fallecidos_exceso_v2"))
                            )
                        )
                    ),
                ),

# economia ----------------------------------------------------------------

            bs4TabItem(
                tabName = "economia",
                tags$h2(icon("money-check-alt"), "Economia"),
                tags$hr(),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            column(width = 3, uiOutput("rmd_economia_desempleo")),
                            column(width = 9, bs4CardHC(addSpinner(highchartOutput("hc_tasa_desempleo", height = 600))))
                        )
                    )
                ),
            ),

# acerca ------------------------------------------------------------------

            bs4TabItem(
                tabName = "acerca",
                tags$h2(icon("question-circle"), "Acerca de esta aplicación"),
                tags$hr(),
                fluidRow(
                    bs4CardCustom(
                        fluidRow(
                            includeMarkdown("md/acerca_de_esta_app.md")
                            )
                        ),
                    ),
                fluidRow(
                    bs4UserCard(
                        src = "http://www.dim.uchile.cl/~alsilva/Images/Foto.jpg",
                        title = "Alonso Silva",
                        width = 3,
                        includeMarkdown("md/bio_alonso.md")
                    ),
                    bs4UserCard(
                        status = "success",
                        src = "https://avatars1.githubusercontent.com/u/56481?s=400&u=1d71bf2f7744313929cd0ad18936065c2a06286e&v=4",
                        title = "Joshua Kunst",
                        width = 3,
                        includeMarkdown("md/bio_joshua.md")
                    ),
                    bs4UserCard(
                        status = "warning",
                        src = "https://avatars0.githubusercontent.com/u/4108139?s=400&u=f25684d4ce34d3d9d109d00ce9de9c8ded869742&v=4",
                        title = HTML("Héctor González"),
                        width = 3,
                        includeMarkdown("md/bio_hector.md")
                    ),
                    bs4UserCard(
                        status = "danger",
                        src = "https://avatars1.githubusercontent.com/u/31071412?s=400&u=d76cfe73d64ed32d046b2de603b80fc69d05149b&v=4",
                        title = "Ignacio Rossi",
                        subtitle = "",
                        width = 3,
                        includeMarkdown("md/bio_ignacio.md")
                        )
                    )
                )
            )
        )
    )
