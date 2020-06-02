library(shiny)
library(bs4Dash)
library(highcharter)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(scales)
library(googlesheets)
library(shinyWidgets)

source("R/helpers.R", encoding = "utf-8")

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#FFFFFF",
  primary_color = "#007bff",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)



Sys.setlocale("LC_ALL", "Spanish_Spain.1252")
# Sys.setlocale("LC_ALL","English")

f <- Sys.Date()
dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))

newlang_opts <- getOption("highcharter.lang")


newlang_opts$weekdays <- dias
newlang_opts$months <- as.character(lubridate::month(1:12, label = TRUE, abbr = FALSE))
newlang_opts$shortMonths <- as.character(lubridate::month(1:12, label = TRUE, abbr = TRUE))
newlang_opts$thousandsSep <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = FALSE,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      
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
            titleKey = "Descargar",
            y = -05
          )
        )
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


