library(shiny)
library(shinythemes)
library(highcharter)
library(dplyr)
library(tidyr)
library(lubridate)
library(RcppRoll)
library(readr)


# highcharter conf
newlang_opts <- getOption("highcharter.lang")

f <- Sys.Date()
dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))

newlang_opts$weekdays <- dias
newlang_opts$months <- as.character(lubridate::month(1:12, label = TRUE, abbr = FALSE))
newlang_opts$shortMonths <- as.character(lubridate::month(1:12, label = TRUE, abbr = TRUE))
newlang_opts$thousandsSep <- ","

options(highcharter.lang = newlang_opts)