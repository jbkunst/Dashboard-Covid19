grafico_casos_confirmados_diarios <- function(){
  
  dcasos_totales_cumulativos <- readRDS("data/producto3/CasosTotalesCumulativo.rds")
  
  dcasos_totales_cumulativos <- dcasos_totales_cumulativos %>%
    gather(fecha, casos, -Region) %>%
    filter(Region == "Total") %>%
    mutate(
      fecha = date(fecha),
      casos_nuevos = lag(casos),
      casos_nuevos = ifelse(is.na(casos_nuevos), casos, casos - casos_nuevos),
      media_movil = roll_mean(casos_nuevos, n = 7, fill = NA, align = "right"),
      media_movil = round(media_movil, 0)
    )
  
  
  evento <- tibble(
    fecha = c(ymd("2020-04-29"), ymd("2020-05-15")),
    texto = c("Se suman casos<br>asintomáticos", "Inicio cuarentena<br>en la RM")
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  hchart(
    dcasos_totales_cumulativos,
    type = "line",
    hcaes(fecha, casos_nuevos),
    name = "Casos confirmados diarios",
    showInLegend = TRUE,
    color = PARS$color$primary,
    lineWidth = 4,
    zIndex = 2
  ) %>% 
    hc_add_series(
      dcasos_totales_cumulativos, "line",
      hcaes(datetime_to_timestamp(fecha), media_movil),
      name = "Media móvil últimos 7 días",
      color = PARS$color$gray,
      showInLegend = TRUE,
      lineWidth = 3,
      zIndex = 0
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de casos")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_caption(
      text =  "Contagios por Coronavirus confirmados por exámenes de laboratorio 
      y notificados por el sistema de vigilancia epidemiológica EPIVIGILA 
      del Ministerio de Salud. Esta corresponde a la famosa curva de contagios,
      que considera sólo a las personas activamente afectadas por el virus"
    ) %>% 
    hc_exporting(enabled = TRUE)
  
}

grafico_examenes_informados_casos_fallecidos_confirmados <- function(){
  
  dcasos_totales_cumulativos <- readRDS("data/producto3/CasosTotalesCumulativo.rds")
  dcasos_fallecidos_0804 <- readRDS('data/producto4/2020-04-08-CasosConfirmados-totalRegional.rds')
  dcasos_examenes <- readRDS('data/producto7/PCR.rds')
  dcasos_fallecidos <-  read_csv(paste0('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/', today()-1,'-CasosConfirmados-totalRegional.csv'))
  
  dcasos_totales_hoy <- dcasos_totales_cumulativos %>% 
    gather(dia, nro_casos, -Region) %>%
    filter(dia == max(date(dia)), !str_detect(Region, "Total")) %>% 
    select(-dia)
  
  dcasos_fallecidos2 <- dcasos_fallecidos %>% 
    select(Region, nro_fallecidos = `Fallecidos totales`) %>% 
    mutate(Region = case_when(
      str_detect(Region, "Arauca") ~ "Araucanía",
      str_detect(Region, "Arica") ~ "Arica y Parinacota",
      str_detect(Region, "Ays") ~ "Aysén",
      str_detect(Region, "Bio") ~ "Biobío",
      str_detect(Region, "Lagos") ~ "Los Lagos",
      str_detect(Region, "Rios") ~ "Los Ríos",
      str_detect(Region, "Nuble") ~ "Ñuble",
      str_detect(Region, "O'Higgins") ~ "O’Higgins",
      str_detect(Region, "Tara") ~ "Tarapacá",
      str_detect(Region, "Valpa") ~ "Valparaíso",
      TRUE ~ Region
    ))
  
  d <- dcasos_examenes %>% 
    gather(dia, nro_examenes, -Region, -`Codigo region`, -Poblacion) %>% 
    mutate(nro_examenes = replace_na(nro_examenes, 0)) %>% 
    group_by(Region, `Codigo region`, Poblacion) %>% 
    summarise(nro_examenes = sum(nro_examenes)) %>% 
    ungroup() %>% 
    mutate(nro_examenes_x100mh = 1e5*nro_examenes/Poblacion) %>% 
    left_join(dcasos_totales_hoy) %>% 
    mutate(nro_casos_x100mh = 1e5*nro_casos/Poblacion) %>% 
    left_join(dcasos_fallecidos2) %>% 
    mutate(nro_fallecidos_x100mh = 1e5*nro_fallecidos/Poblacion) %>% 
    arrange(-nro_examenes_x100mh) %>% 
    mutate(Region = fct_inorder(Region))
  
  
  hchart(d, "area",
         hcaes(Region, nro_examenes_x100mh),
         name = "Exámenes",
         showInLegend = TRUE) %>% 
    hc_add_series(
      d, "area",
      hcaes(Region, nro_casos_x100mh),
      name = "Casos",
      showInLegend = TRUE
    ) %>% 
    hc_add_series(
      d, "area",
      hcaes(Region, nro_fallecidos_x100mh),
      name = "Fallecidos",
      showInLegend = TRUE
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_yAxis(
      title = list(text = "Nº de Casos")
    ) 
  
  
  
}

grafico_casos_confirmados_rango_edad <- function(){
  
  dcasos_confirmados_rango_edad <- readRDS("data/producto16/CasosGeneroEtario.rds") %>%
    gather(-`Grupo de edad`, -Sexo, key = "fecha", value="n") %>% 
    mutate(fecha = as_date(fecha, format="%Y-%m-%d")) %>% 
    separate(`Grupo de edad`, c("min","max")) %>% 
    mutate_at(c("min", "max"), ~as.numeric(.)) %>% 
    mutate(Grupo_edad = case_when(
      max <= 39 ~ "<= 39",
      max <= 49 ~ "40-49",
      max <= 59 ~ "50-59",
      max <= 69 ~ "60-69",
      max <= 79 ~ "70-79",
      min >= 80 ~ ">= 80",
      TRUE ~ NA_character_
    )) %>% 
    group_by(fecha, Grupo_edad) %>% 
    summarise(n = sum(n)) %>% 
    ungroup()
  
  hchart(
    dcasos_confirmados_rango_edad,
    type = "column",
    hcaes(x=fecha, y=n, group=Grupo_edad)
  ) %>% 
    hc_plotOptions(
      series = list(
        stacking = list(enabled = TRUE)  
      )
    ) %>% 
    hc_tooltip(
      shared = TRUE
    ) %>% 
    hc_exporting(
      enabled = TRUE
    ) %>% 
    hc_yAxis(
      title = list(text = "Nº de Casos")
    ) 
}

grafico_defunciones_esperadas <- function(){
  
  dcasos_fallecidos <- readRDS("data/producto32/Defunciones.rds")
  
  d <- dcasos_fallecidos %>% 
    gather(dia, nro_fallecidos, -Region, -`Codigo region`, -Comuna, -`Codigo comuna`) %>% 
    mutate(
      dia = ymd(dia),
      nro_semana = week(dia),
      anio = year(dia)) %>% 
    group_by(nro_semana, anio) %>% 
    summarise(nro_fallecidos = sum(nro_fallecidos)) %>% 
    ungroup() %>% 
    arrange(-anio) %>% 
    mutate(anio = as.character(anio))
  
  gr <- d %>% 
    group_by(anio) %>% 
    summarise(nro_fallecidos = sum(nro_fallecidos)) %>% 
    arrange(anio) %>% 
    mutate(nro_fallecidos = ifelse(anio %in% c(2012, 2016), nro_fallecidos*365/366, nro_fallecidos)) %>% 
    mutate(
      g = (nro_fallecidos - lag(nro_fallecidos))/lag(nro_fallecidos)
    ) %>% 
    filter(anio != 2020) %>% 
    filter(complete.cases(.)) %>% 
    filter(anio > 2012) %>%
    summarise(mean(g)) %>% 
    pull()
  
  d <- d %>% 
    group_by(anio) %>% 
    filter(nro_semana != max(nro_semana)) %>% 
    ungroup()
  
  d <- d %>% 
    mutate(
      anio = as.numeric(anio),
      nro_fallecidos_adj = round(nro_fallecidos * ((1 + gr)^(2020 - anio)), 0)
    )
  
  desp <- d %>% 
    filter(anio != 2020) %>% 
    group_by(nro_semana) %>% 
    summarise(
      nro_fallecidos_esperados = mean(nro_fallecidos_adj),
      amplitud = 2*sd(nro_fallecidos_adj)
      ) %>% 
    mutate(fecha = ymd("2020-01-01") + weeks(nro_semana - 1)) 
  
  d <- d %>% 
    mutate(fecha = ymd("2020-01-01") + weeks(nro_semana - 1)) %>% 
    filter(anio == 2020) 
  
  hchart(
    d,
    hcaes(fecha, nro_fallecidos_adj),
    type = "line", 
    color = "red",
    name = "Fallecimientos semanales 2020",
    showInLegend = TRUE
    ) %>% 
    hc_add_series(
      desp,
      type = "line",
      hcaes(x = fecha, y = nro_fallecidos_esperados),
      name = "Número de fallecidos esperados",
      id = "numero_fallecidos_esperados",
      lineWidth = 1,
      color = "blue",
      showInLegend = TRUE
    ) %>% 
    hc_add_series(
      desp,
      type = "arearange",
      hcaes(x = fecha, low = nro_fallecidos_esperados - amplitud, high = nro_fallecidos_esperados + amplitud),
      color = PARS$color$gray,
      fillOpacity = 0.25,
      linkedTo = "numero_fallecidos_esperados",
      zIndex = -3,
      showInLegend = FALSE,
      name = "2 desviaciones estándar"
      ) %>% 
    hc_tooltip(
      shared = TRUE,
      valueDecimals = 0
      ) %>% 
    hc_yAxis(
      title = list(text = "Número de fallecidos"),
      min = 0
      ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
      ) %>%
    hc_exporting(enabled = TRUE)
  
}

grafico_tasa_letalidad <- function(){
  
  dfallecidos <- serie_nro_fallecidos()
  dcontagiados <- serie_nro_casos()
  dfallecidos_contagiados <- dfallecidos %>% 
    full_join(
      dcontagiados,
      by="dia"
    ) %>% 
    mutate(nro_fallecidos = if_else(is.na(nro_fallecidos), 0, nro_fallecidos)) %>% 
    mutate(porc=(nro_fallecidos/nro_casos)*100) 
  
  dfallecidos_contagiados <- dfallecidos_contagiados %>% 
    arrange(dia) %>% 
    mutate(desest = 3*zoo::rollapplyr(porc, 7, sd, fill = 0)) %>% 
    mutate(desest = round(desest,2))
  
  evento_100fallecidos <- dfallecidos_contagiados %>%
    filter(nro_fallecidos>=100) %>%
    slice(1) %>%
    pull(dia)
  
  evento <- tibble(
    fecha = c(ymd(evento_100fallecidos), ymd("2020-04-29"), ymd("2020-05-15")),
    texto = c("Primeros 100 fallecidos", "Se suman casos<br>asintomáticos", "Inicio cuarentena<br>en la RM")
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 10)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  dfallecidos_contagiados %>% 
    hchart(., "line",
           hcaes(dia, porc),
           color = PARS$color$primary,
           name = "Tasa de Letalidad",
           id = "fallecidos_contagiados",
           showInLegend = TRUE
    ) %>%
    hc_add_series(
      dfallecidos_contagiados,
      type = "arearange",
      hcaes(x = dia, low = round(porc,2) - desest, high = round(porc,2) + desest),
      color = hex_to_rgba("gray", 0.05), 
      linkedTo = "fallecidos_contagiados",
      zIndex = -3,
      showInLegend = FALSE, 
      name = "Desviación estandar"
    ) %>% 
    hc_yAxis(
      allowDecimals = TRUE,
      title = list(text="%"),
      min = 0
    ) %>% 
    hc_xAxis(
      title = list(text=""),
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_tooltip(
      pointFormat = " {series.name}: <b>{point.y}</b> ({point.nro_fallecidos}/{point.nro_casos}) <br/>",
      valueSuffix = " %",
      valueDecimals = 2,
      shared = TRUE
    )
}

grafico_examenes_realizados <- function(){
  
  d <- serie_nro_examenes()
  
  
  evento <- tibble(
    fecha = ymd("2020-05-15"),
    texto = "Inicio cuarentena<br>en la RM"
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  d %>% 
    hchart(
      hcaes(dia, nro_examenes),
      type = "line",
      name = "Exámenes",
      showInLegend = TRUE,
      color = PARS$color$primary,
      lineWidth = 4
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de exámenes")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
}

grafico_fallecidos_diarios <- function(){
  
  d <- serie_nro_fallecidos()
  
  d <- d %>% 
    mutate(v  = nro_fallecidos -  lag(nro_fallecidos)) %>% 
    mutate(v = ifelse(is.na(v), nro_fallecidos, v)) %>% 
    mutate(nro_fallecidos = v) %>% 
    select(-v)
  
  evento <- tibble(
    fecha = ymd("2020-05-15"),
    texto = "Inicio cuarentena<br>en la RM"
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  d %>% 
    hchart(
      hcaes(dia, nro_fallecidos),
      type = "line",
      name = "Fallecidos",
      showInLegend = TRUE,
      color = PARS$color$primary,
      lineWidth = 4
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de fallecidos")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
}

grafico_recuperados_diarios <- function(){
  
  d <- serie_recuperados()
  
  d <- d %>% 
    mutate(v  = casos_recuperados -  lag(casos_recuperados)) %>% 
    mutate(v = ifelse(is.na(v), casos_recuperados, v)) %>% 
    mutate(casos_recuperados = v) %>% 
    select(-v)

  evento <- tibble(
    fecha = ymd("2020-05-15"),
    texto = "Inicio cuarentena<br>en la RM"
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  d %>% 
    hchart(
      hcaes(dia, casos_recuperados),
      type = "line",
      name = "Recuperados",
      showInLegend = TRUE,
      color = PARS$color$primary,
      lineWidth = 4
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de recuperados")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
}

grafico_pacientes_uci <- function(){
  
  d <- serie_nro_pascientes_UCI()
  
  evento <- tibble(
    fecha = ymd("2020-05-15"),
    texto = "Inicio cuarentena<br>en la RM"
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  d %>% 
    mutate(dia = ymd(dia)) %>% 
    hchart(
      hcaes(dia, nro_pascientes_uci),
      type = "line",
      name = "Pacientes UCI",
      showInLegend = TRUE,
      color = PARS$color$primary,
      lineWidth = 4
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de pacientes UCI")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
}

grafico_ventiladores <- function(){
  
  d <- readRDS("data/producto20/NumeroVentiladores_T.rds") %>% 
    mutate(dia = ymd(Ventiladores))
  
  evento <- tibble(
    fecha = ymd("2020-05-15"),
    texto = "Inicio cuarentena<br>en la RM"
  )
  
  data_plotLine <- evento %>% 
    transmute(
      value = datetime_to_timestamp(fecha),
      label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
    ) %>% 
    mutate(color = "gray", width = 1, zIndex = 5)
  
  d %>% 
    hchart(
      hcaes(dia, total),
      type = "line",
      name = "Disponibles",
      showInLegend = TRUE,
      color = PARS$color$primary,
      lineWidth = 4
    ) %>% 
    hc_add_series(
      data = d,
      hcaes(dia, ocupados),
      type = "line",
      name = "Ocupados",
      showInLegend = TRUE,
      color = "red",
      lineWidth = 4
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>% 
    hc_xAxis(
      plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de Ventiladores")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_exporting(enabled = TRUE)
  
}








