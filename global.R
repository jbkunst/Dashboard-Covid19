library(shiny)
library(bs4Dash)
library(highcharter)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(scales)
library(googlesheets)
library(shinyWidgets)
library(figletr)
library(shinipsum)
library(shinyjs)

source("R/helpers-shiny.R", encoding = "utf-8")
source("R/helpers-data.R", encoding = "utf-8")
source("R/helpers-graficos.R", encoding = "UTF-8")
source("R/helpers-vb.R", encoding = "utf-8")
source("R/helpers-series.R", encoding = "utf-8")

PARS <- list(
  debug = FALSE,
  classcol = "col-xg-2 col-lg-2 col-md-6 col-sm-12",
  color = list(
    sparkline = "#F4F6F9", # color de fondo de value boxes "blancos"
    primary = "#007bff",
    gray = "#C0C0C0"
  ),
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)


Sys.setlocale("LC_ALL", "Spanish_Spain.1252")
# Sys.setlocale("LC_ALL","English")
# f <- Sys.Date()
# dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))

newlang_opts <- getOption("highcharter.lang")

newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
                              "oct", "nov", "dic")
newlang_opts$thousandsSep <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = FALSE,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.85em")),
      
      xAxis = list(
        title = list(
          align = "high",
          style = list(
            fontSize = "0.85em"
          )  
        )
      ),
      
      yAxis = list(
        title = list(
          align = "high",
          style = list(
            fontSize = "0.85em"
          )  
        )
      ),
      
      chart = list(
        backgroundColor = "white",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
        ),
      
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 2000)
          ),
        line = list(
          lineWidth = 4
        ),
        arearange = list(
          lineWidth = 1,
          fillOpacity = 0.25
        )
      ),
      
      exporting = list(
        buttons = list(
          contextButton = list(
            symbol = 'url(https://cdn.icon-icons.com/icons2/936/PNG/512/download-to-storage-drive_icon-icons.com_73590.png)',
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar",
            y = -05
          )
        )
      ),
      
      tooltip = list(
        useHTML = TRUE
      ),
      

      legend = list(
        verticalAlign = "top",
        align = "left",
        itemStyle =  list(
          fontWeight = "normal"
          )
        )
      )
  )

