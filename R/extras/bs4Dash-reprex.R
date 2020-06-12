library(bs4Dash)

ui <- bs4DashPage(
  sidebar_collapsed = TRUE,
  sidebar_mini = TRUE,
  navbar = bs4DashNavbar(compact = TRUE),
  sidebar = bs4DashSidebar(
    expand_on_hover = TRUE,
    fixed = TRUE,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Dashboard",
        tabName = "inicio",
        icon =  "tachometer-alt"
      ),
      bs4SidebarMenuItem(
        text = "Geográfico",
        tabName = "item2",
        icon = "map-marker-alt"
      )
    )
  ),
  body = bs4DashBody()
)

server <- function(input, output) {}

# runApp(list(ui = ui, server = server))

