library(shiny)
library(bs4Dash)
library(highcharter)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(scales)
library(googlesheets)
library(shinycssloaders)


source("../R/helpers.R", encoding = "utf-8")


bs4CardHC <- function(...){
  
  bs4Card(
    ...,
    elevation = 1,
    closable = FALSE,
    width = 12,
    collapsible = FALSE
  )
  
}



PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

# highcharter conf
newlang_opts <- getOption("highcharter.lang")

# f <- Sys.Date()
# dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))
# dput(dias)
# dput(as.character(lubridate::month(1:12, label = TRUE, abbr = FALSE)))
# dput(as.character(lubridate::month(1:12, label = TRUE, abbr = TRUE)))

# newlang_opts$downloadPDF <- "Descargar la wah"
newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre") 
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
newlang_opts$thousandsSep <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = FALSE,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
        ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 3000)
          )
        ),
      
      exporting = list(
        buttons = list(
          contextButton = list(
            symbol = 'url(https://image.flaticon.com/icons/png/512/660/660474.png)',
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar"
          )
        )
      ),
      

      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
          )
        )
      )
  )


