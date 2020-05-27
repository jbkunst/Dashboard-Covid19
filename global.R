library(shiny)
library(shinythemes)
library(highcharter)
library(dplyr)
library(tidyr)
library(lubridate)
library(RcppRoll)
library(readr)
library(stringr)

source("R/helpers.R")

# highcharter conf
newlang_opts <- getOption("highcharter.lang")

# f <- Sys.Date()
# dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))
# dput(dias)
# dput(as.character(lubridate::month(1:12, label = TRUE, abbr = FALSE)))
# dput(as.character(lubridate::month(1:12, label = TRUE, abbr = TRUE)))

newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre") 
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
newlang_opts$thousandsSep <- ","

options(highcharter.lang = newlang_opts)