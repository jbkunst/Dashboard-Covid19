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
  
  peak <- dcasos_totales_cumulativos %>% 
    filter(casos_nuevos == max(casos_nuevos) | fecha==max(fecha)) %>% 
    mutate(texto = str_c(format(fecha, "%d"), " de ",format(fecha, "%B"), " del ", format(fecha,"%Y"))) %>% 
    select(fecha, casos_nuevos, texto) %>% 
    filter(row_number()==n() | row_number()==1) %>% 
    arrange(desc(fecha))
  
  
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
  
  texto <- str_glue("Contagios por Coronavirus confirmados por exámenes. La curva de contagios
                    considera sólo a las personas activamente afectadas por el virus. ",
                    "La cifra mas reciente es de { cifra_mas_reciente } (al { fecha_mas_reciente }), mientras que 
                    el máximo registrado es de { maximo_registrado } el { fecha_maximo_registrado}.",
                    cifra_mas_reciente = commac(peak[1,]$casos_nuevos),
                    fecha_mas_reciente =  peak[1,]$texto,
                    maximo_registrado = commac(peak[2,]$casos_nuevos),
                    fecha_maximo_registrado = peak[2,]$texto)
  
  hchart(
    dcasos_totales_cumulativos,
    type = "line",
    hcaes(fecha, casos_nuevos),
    name = "Casos confirmados diarios",
    showInLegend = TRUE,
    color = PARS$color$primary
  ) %>% 
    hc_add_series(
      dcasos_totales_cumulativos, "line",
      hcaes(datetime_to_timestamp(fecha), media_movil),
      name = "Media móvil últimos 7 días",
      color = PARS$color$gray,
      showInLegend = TRUE,
      lineWidth = 3,
      zIndex = -1,
      visible = FALSE
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
    hc_subtitle(
      text =  texto
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
  
  dexc <- d %>%
    left_join(desp, by = c("nro_semana", "fecha")) %>%
    filter(nro_semana >= 18) %>% 
    # filter(nro_fallecidos > nro_fallecidos_esperados) %>% 
    mutate(
      # limlow = pmin(nro_fallecidos, nro_fallecidos_esperados),
      # limhigh = pmax(nro_fallecidos, nro_fallecidos_esperados),
      limlow = nro_fallecidos,
      limhigh = nro_fallecidos_esperados,
      diff = nro_fallecidos - nro_fallecidos_esperados,
      diffacum = round(cumsum(diff))
    )
  
  dexclbl <- dexc %>% 
    summarise(
      nro_semanas = dplyr::n(),
      excestot_tot = max(diffacum),
      nro_semana = ifelse(
        dplyr::n() %% 2 == 0,
        nth(nro_semana, dplyr::n() / 2 + 2),
        nth(nro_semana, dplyr::n() / 2 + 2)
        ),
      nro_fallecidos = ifelse(
        dplyr::n() %% 2 == 0,
        nth(nro_fallecidos, dplyr::n() / 2 + 1), 
        nth(nro_fallecidos, dplyr::n() / 2 + 1)
        )
    ) %>% 
    left_join(dexc %>% select(nro_semana, fecha), by = "nro_semana") %>% 
    mutate(
      x = datetime_to_timestamp(fecha),
      y = nro_fallecidos,
      text = str_c("Diferencia entre fallecimientos<br> reales y esperados en ", nro_semanas, " semanas<br><b>", commac(excestot_tot) ,"</b>")
    ) %>% 
    rowwise() %>% 
    mutate(point = list(list(x = x, y = y, xAxis = 0, yAxis = 0))) %>% 
    select(-x, -y)
  
  hchart(
    d,
    hcaes(fecha, nro_fallecidos_adj),
    type = "line", 
    color = PARS$color$danger,
    name = "Fallecimientos semanales 2020",
    showInLegend = TRUE,
    states = list(inactive= list(opacity = 1)),
    zIndex = 1,
    ) %>% 
    hc_add_series(
      desp,
      type = "line",
      hcaes(x = fecha, y = nro_fallecidos_esperados),
      name = "Número de fallecidos esperados en año normal",
      id = "numero_fallecidos_esperados",
      color = PARS$color$primary,
      showInLegend = TRUE
    ) %>% 
    hc_add_series(
      desp,
      type = "arearange",
      hcaes(x = fecha, low = nro_fallecidos_esperados - amplitud, high = nro_fallecidos_esperados + amplitud),
      color = PARS$color$gray,
      linkedTo = "numero_fallecidos_esperados",
      zIndex = -3,
      showInLegend = FALSE,
      name = "Itervalo de 2 desviaciones estándar"
      ) %>% 
    hc_add_series(
      dexc,
      type = "arearange",
      hcaes(x = fecha, low = limlow, high = limhigh),
      color = "black",
      zIndex = -2,
      showInLegend = TRUE,
      fillOpacity = 0.35,
      name = "Exceso de fallecidos",
      tooltip = list(pointFormat = "<span style='color:{point.color};'>&#9679;</span> {series.name}: <b>{point.diffacum:,.0f}</b><br/>")
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
    hc_exporting(enabled = TRUE) %>% 
    hc_annotations(
      list(
        labelOptions = list(
          shape = "connector",
          align = "right",
          y = 200,
          x = 125,
          justify = FALSE,
          crop = TRUE,
          style = list(fontSize = "0.75em", textOutline = "1px white")
        ),
        labels = list_parse(dexclbl)
      )
    )
  
}

grafico_tasa_letalidad <- function(){
  
  dfallecidos <- serie_nro_fallecidos()
  
  dcontagiados <- serie_nro_casos()
  
  dfallecidos_contagiados <- dfallecidos %>% 
    full_join(
      dcontagiados,
      by="dia"
    ) %>% 
    mutate(
      nro_fallecidos = if_else(is.na(nro_fallecidos), 0, nro_fallecidos),
      porc = (nro_fallecidos/nro_casos)*100
      ) 
  
  dfallecidos_contagiados <- dfallecidos_contagiados %>% 
    arrange(dia) %>% 
    mutate(desest = 3*zoo::rollapplyr(porc, 7, sd, fill = 0)) %>% 
    mutate(desest = round(desest,2))
  
  dfallecidos_contagiados <- dfallecidos_contagiados %>% 
    # filter(nro_fallecidos > 50) %>%
    mutate(
      ic = map2(
        porc/100, 
        nro_casos,
        function(eval = 1.4410024, n = 14365){ 
          # message("eval = ", eval, " n = ", n)
          binom.test(round(n*c(eval, 1 - eval)), conf.level = .99)[["conf.int"]] 
        }),
      inferior = map_dbl(ic, first),
      inferior = ifelse(inferior < 0, 0, inferior),
      superior = map_dbl(ic, last),
      superior = ifelse(superior > 1, 1, superior)
    )
  
  evento_100fallecidos <- dfallecidos_contagiados %>%
    filter(nro_fallecidos>=100) %>%
    slice(1) %>%
    pull(dia)

  evento_1000fallecidos <- dfallecidos_contagiados %>%
    filter(nro_fallecidos>=1000) %>%
    slice(1) %>%
    pull(dia)
  
  evento <- tibble(
    fecha = c(ymd(evento_100fallecidos), ymd(evento_1000fallecidos), ymd("2020-04-29"), ymd("2020-06-07")),
    text = c("<br>Primeros 100 fallecidos", "<br>Primeros 1.000 fallecidos", "<br>Se suman casos asintomáticos", "<br>Se suman 631 casos")
  ) %>% 
    left_join(
      dfallecidos_contagiados %>% 
        select(dia, porc),
      by = c("fecha"="dia")
    ) %>% 
    mutate(
      x = datetime_to_timestamp(fecha),
      y = porc
    ) %>% 
    mutate(text = str_c(format(fecha, "%d")," de ",format(fecha, "%B: "), text)) %>% 
    rowwise() %>% 
    mutate(point = list(list(x = x, y = y, xAxis = 0, yAxis = 0))) %>% 
    select(-x, -y)
    
  
  peak <- dfallecidos_contagiados %>% 
    filter(porc == max(porc)) %>% 
    # mutate(texto = str_glue("{ dia } { num_dia } de {mes}",
    #                         dia = format(dia, "%A"),
    #                         num_dia = format(dia, "%d"),
    #                         mes = format(dia, "%B"))
    # )
    mutate(texto = str_c(format(dia, "%A"), " ",format(dia, "%d")," de ",format(dia, "%B"))) %>% 
    select(dia, porc, texto) %>% 
    arrange(desc(dia)) %>% 
    slice(1)

  texto <- str_glue("La <b>Tasa de Letalidad</b> corresponde a la razón entre el número de fallecidos totales registrados
      hasta la fecha, sobre el número de casos totales reportados hasta la fecha. La mayor tasa de letalidad regisrada ocurrió
       el { peak_texto } con una tasa de { peak }%",
                    peak_texto = peak$texto,
                    peak = round(peak$porc,2))
  # data_plotLine <- evento %>% 
  #   transmute(
  #     value = datetime_to_timestamp(fecha),
  #     label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
  #   ) %>% 
  #   mutate(color = "gray", width = 1, zIndex = 5)

  hchart(
    dfallecidos_contagiados, 
    "line",
    hcaes(dia, porc),
    color = PARS$color$primary,
    name = "Tasa de Letalidad",
    id = "fallecidos_contagiados",
    showInLegend = TRUE
    ) %>%
    hc_add_series(
      dfallecidos_contagiados %>% filter(nro_fallecidos>=100),
      type = "arearange",
      # hcaes(x = dia, low = round(porc,2) - desest, high = round(porc,2) + desest),
      hcaes(x = dia, low = 100*inferior, high = 100*superior),
      color = PARS$color$gray, 
      linkedTo = "fallecidos_contagiados",
      zIndex = -3,
      showInLegend = FALSE, 
      name = "Intervalo de Confianza (99%)"
    ) %>% 
    hc_yAxis(
      allowDecimals = TRUE,
      labels = list(format = "{value}%"),
      title = list(text = "Tasa de Letalidad"),
      min = 0,
      max = 3.5
    ) %>% 
    hc_xAxis(
      title = list(text = "Fecha")#,
      #plotLines = list_parse(data_plotLine)
    ) %>% 
    hc_tooltip(
      # pointFormat = " {series.name}: <b>{point.y}</b> ({point.nro_fallecidos}/{point.nro_casos}) <br/>",
      valueSuffix = " %",
      valueDecimals = 2,
      shared = TRUE
    ) %>% 
    hc_subtitle(
      text =  texto
    ) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_annotations(
      list(
        labelOptions = list(
          shape = "connector",
          align = "right",
          y = -50,
          x = 3,
          justify = FALSE,
          crop = TRUE,
          style = list(fontSize = "0.60em", textOutline = "1px white")
        ),
        labels = list_parse(evento)
      )
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
  
  peak <- d %>% 
    filter(nro_examenes == max(nro_examenes)) %>% 
    mutate(texto = str_c(format(dia, "%A"), " ",format(dia, "%d")," de ",format(dia, "%B"))) %>% 
    select(dia, nro_examenes, texto) %>% 
    arrange(desc(dia)) %>% 
    slice(1)
  ultimos_7_dias <- d %>% 
    arrange(desc(dia)) %>% 
    slice(1:7) %>% 
    pull(nro_examenes) %>% 
    mean
  
  texto <- str_glue("Total de exámenes <b>PCR diarios</b> reportados a nivel nacional. El mayor registro en cantidad de toma de exámenes 
                    fue el { peak_texto } con { peak_nro_examenes } test realizados, mientras que en los últimos 7 días se han efectuado,
                    en promedio { ultimos_7_dias } exámenes.",
                 peak_texto = peak$texto,
                 peak_nro_examenes = commac(peak$nro_examenes),
                 ultimos_7_dias = commac(ultimos_7_dias))
  
  d %>% 
    hchart(
      hcaes(dia, nro_examenes),
      type = "line",
      name = "Exámenes",
      showInLegend = TRUE,
      color = PARS$color$primary
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
    hc_subtitle(
      text = texto
    ) %>% 
    hc_exporting(enabled = TRUE)
  
}

grafico_examenes_realizados_establecimiento <- function(){
  
  d <- serie_nro_examenes_establecimiento()

  df <- d %>% 
    filter(Examenes == "informados ultimo dia") %>% 
    mutate(
      fecha = ymd(fecha),
      nombre = case_when(
        Establecimiento == "Total informados ultimo dia" ~ "Exámenes (Total)",
        Establecimiento == "Hospitales público" ~ "Exámenes Hospitales públicos",
        Establecimiento == "Instituto de salud pública" ~ "Exámenes Instituto Salud Pública",
        Establecimiento == "Privados" ~ "Exámenes Privados"
      ),
      nombre = factor(
        nombre, 
        c("Exámenes (Total)", "Exámenes Hospitales públicos", "Exámenes Instituto Salud Pública", "Exámenes Privados")
        )
    ) %>% 
    select(fecha, nro_examenes, nombre)
    
  peak <- d %>% 
    filter(Establecimiento=="Total informados ultimo dia") %>% 
    filter(nro_examenes == max(nro_examenes)) %>% 
    mutate(texto = str_c(format(dia, "%A"), " ",format(dia, "%d")," de ",format(dia, "%B"))) %>% 
    select(dia, nro_examenes, texto) %>% 
    arrange(desc(dia)) %>% 
    slice(1)
  
  ultimos_7_dias <- d %>% 
    filter(Establecimiento=="Total informados ultimo dia") %>% 
    arrange(desc(dia)) %>% 
    slice(1:7) %>% 
    pull(nro_examenes) %>% 
    mean
  
  texto <- str_glue(
    "Total de exámenes <b>PCR diarios</b> reportados a nivel nacional. 
    El mayor registro en cantidad de toma de exámenes fue el { max_ex_fec } con { max_ex } test realizados
    , mientras que en los últimos 7 días se han efectuado, en promedio, { prom_ult_7d } exámenes.",
    max_ex_fec = peak$texto,
    max_ex = commac(peak$nro_examenes),
    prom_ult_7d = commac(ultimos_7_dias)
  )
  
  hchart(
    df,
    type = "line",
    hcaes(fecha, nro_examenes, group = nombre),
    visible = c(TRUE, rep(FALSE, 3)),
    ) %>% 
    hc_colors(c(PARS$color$primary, covpal(3, begin = 0.2, end = 0.8))) %>%
    hc_tooltip(
      table = TRUE,
      valueDecimals = 0, 
      shared = TRUE
      ) %>% 
    hc_yAxis(title = list(text = "Número de exámenes")) %>%
    hc_xAxis(
      title = list(text = "Fecha")
      ) %>%
    hc_subtitle(text = texto) %>% 
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
  
  peak <- d %>% 
    filter(nro_fallecidos == max(nro_fallecidos) | dia==max(dia)) %>% 
    mutate(texto = str_c(format(dia, "%d"), " de ",format(dia, "%B"), " del ", format(dia,"%Y"))) %>% 
    select(dia, nro_fallecidos, texto) %>% 
    filter(row_number()==n() | row_number()==1) %>% 
    arrange(desc(dia))
  
  text_cond1 <- str_glue("La mayor cifra de fallecidos registrada es de { peak_nro_fallecidos }, correspondiente a la última actualización informada.",
                         peak_nro_fallecidos = peak$nro_fallecidos
                         )
  text_cond2 <- str_glue("La mayor cantidad de fallecidos registrada a la fecha es de { peak_nro_fallecidos }, que ocurrió el { fecha_peak }.
                      La última cifra informada al { ultima_fecha } corresponde a { peak_ultima_fecha } fallecidos.",
                      peak_nro_fallecidos = peak[2,]$nro_fallecidos,
                      fecha_peak = peak[2,]$texto,
                      ultima_fecha = peak[1,]$texto,
                      peak_ultima_fecha = peak[1,]$nro_fallecidos)
  texto <- if_else(
    nrow(peak) == 1,
    text_cond1[1],
    text_cond2)
  
  texto <- str_glue("Total de fallecidos diarios reportados a nivel nacional. { texto }",
                    texto = texto)
  
  hchart(
    d,
    hcaes(dia, nro_fallecidos),
    type = "line",
    name = "Fallecidos",
    showInLegend = TRUE,
    color = PARS$color$primary
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
    hc_subtitle(
      text =  texto
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
      color = PARS$color$primary
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
  
  texto <- "Número diario de pacientes en <b>UCI</b> a nivel nacional."
  
  d %>% 
    mutate(dia = ymd(dia)) %>% 
    hchart(
      hcaes(dia, nro_pascientes_uci),
      type = "line",
      name = "Pacientes UCI",
      showInLegend = TRUE,
      color = PARS$color$primary
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
    hc_subtitle(
      text =  texto
    ) %>% 
    hc_exporting(enabled = TRUE)
  
}

grafico_ventiladores <- function(){
  
  d <- readRDS("data/producto20/NumeroVentiladores_T.rds") %>% 
    mutate(dia = ymd(Ventiladores))
  
  # evento <- tibble(
  #   fecha = ymd("2020-05-15"),
  #   texto = "Inicio cuarentena<br>en la RM"
  # )
  # 
  # data_plotLine <- evento %>% 
  #   transmute(
  #     value = datetime_to_timestamp(fecha),
  #     label = purrr::map(texto, ~ list(text = .x, style = list(fontSize = 13)))
  #   ) %>% 
  #   mutate(color = "gray", width = 1, zIndex = 5)
  
  texto <- "El número de <b>ventiladores disponibles</b> y número de ventiladores ocupados para
      cada día reportado. Se consideran todos los ventiladores presentes en el Sistema Integrado Covid 19."
  
  hchart(
    d,
    hcaes(dia, ocupados),
      type = "line",
      name = "Ocupados",
      showInLegend = TRUE,
      color = PARS$color$primary
    ) %>% 
    hc_add_series(
      data = d,
      hcaes(dia, total),
      type = "line",
      name = "Ventiladores Totales",
      showInLegend = TRUE,
      color = PARS$color$danger
    ) %>% 
    hc_tooltip(
      table = TRUE, 
      valueDecimals = 0,
      sort = TRUE) %>% 
    hc_yAxis(
      title = list(text = "Número de Ventiladores")
    ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) %>%
    hc_subtitle(
      text =  texto
    ) %>% 
    hc_exporting(enabled = TRUE)
  
}

grafico_map <- function(reg = "Tarapacá"){
  
  d <- serie_nro_casos_comuna()
  
  cod_region <- d %>%
    distinct(Region, `Codigo region`)
  
  d <- d %>% 
    filter(Region == reg) %>% 
    group_by(Comuna, `Codigo comuna`) %>% 
    summarise(value = sum(`Casos confirmados`)) %>% 
    ungroup()
  
  cod_reg <- cod_region %>% 
      filter(Region == reg) %>% 
      select(`Codigo region`) %>% 
      pull()
    
  ruta_geojson <-  dir("data/geojson/", full.names = TRUE, pattern = cod_reg)
  
  gjson <- jsonlite::fromJSON(ruta_geojson)
  
  if(reg == "Valparaíso") {
    
    ids <- which(gjson$features$properties$codigo_comuna %in% c("05201", "05104"))
    
    gjson$features <- gjson$features[-ids,]
    
    id <- which(gjson$features$properties$codigo_comuna == "05101")
    
    # str(gjson$features, max.level = 1)
    
    # as_tibble(gjson$features)
    
    gjson$features$geometry$coordinates[[id]] <- gjson$features$geometry$coordinates[[id]][2]
    
    # str(gjson$features$geometry$coordinates[[id]])
    # str(gjson$features$geometry$coordinates[[id]][2])
    # gjson$features$type <- gjson$features$type[-ids]
    # gjson$features$properties <- gjson$features$properties[-ids]
    # gjson$features$geometry   <- gjson$features$geometry  [-ids]
    
  }

  df <- gjson$features$properties
  
  df <- df %>% 
    left_join(d, by = c("codigo_comuna" = "Codigo comuna"))
  
  gjson <- geojsonio::as.json(gjson)
  
  highchart(type = "map") %>%
    hc_add_series(
      mapData = gjson,
      data = list_parse(df),
      joinBy = c("codigo_comuna", "codigo_comuna"),
      showInLegend = FALSE,
      name = "Numero de Fallecidos",
      borderColor = "gray",
      borderWidth = 0.5,
      animation = list(duration = PARS$hc$duration),
      tooltip = list(
        headerFormat = "",
        pointFormat = "<b>{point.Comuna}</b><br>{point.value} casos"
      ),
      dataLabels = list(
        enabled = TRUE, 
        format = "{point.Comuna}",
        color =  "white",
        style = list(fontSize = "12px", textOutline = "2px gray")
        )
    ) %>% 
    hc_colorAxis(
      stops = color_stops(n = 100, colors = covpal(30)),
      startOnTick = TRUE,
      min = 0,
      endOnTick =  FALSE
    ) %>%
    hc_legend(symbolWidth = 400, align = "center", verticalAlign = "top")
  
}

grafico_map_gs <- function() {
  
  file_gs <- "data/geojson/gran_santiago.geojson"
  
  if(!file.exists(file_gs)) {
    
    download.file(
      "https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/87bc72ea23ad19a116ae9af02fa1cb5ae06f29f3/Extras/GRAN_SANTIAGO.geojson",
      file_gs
    )
    
  }
  
  gransantiago <- jsonlite::fromJSON(file_gs, simplifyVector = FALSE)
  
  dcomuna <- gransantiago$features %>% 
    purrr::map_df("properties") %>% 
    rename_all(stringr::str_to_lower) %>% 
    select(comuna, nom_comuna) %>% 
    mutate(
      comuna = as.numeric(comuna),
      nom_comuna = stringr::str_to_title(nom_comuna)
    )
  
  gransantiago_geojson <- geojsonio::as.json(gransantiago)
  
  dcovid <- readRDS("data/producto1/Covid-19.rds")
  
  dcovid_largo <- dcovid %>% 
    filter(`Codigo region` == 13) %>% 
    rename(comuna = `Codigo comuna`) %>% 
    select(comuna, matches("[0-9]{4}")) %>% 
    gather(fecha, casos, -comuna) %>% 
    mutate(fecha = lubridate::ymd(fecha))
  
  dcovid_ultimo <- dcovid_largo %>% 
    group_by(comuna) %>% 
    filter(fecha == max(fecha)) %>% 
    ungroup()
  
  dcovid_largo <- dcovid_largo %>% 
    rename(x = fecha, y = casos) %>% 
    mutate(x = datetime_to_timestamp(x)) %>% 
    group_by(comuna) %>% 
    nest() %>% 
    rename(ttdata = data) %>% 
    mutate(ttdata = purrr::map(ttdata, list_parse))
  
  dcovid <- left_join(
    dcovid_ultimo,
    dcovid_largo,
    by = "comuna"
  ) %>% 
    mutate(comuna = as.numeric(comuna)) %>% 
    inner_join(dcomuna, by = "comuna") %>% 
    rename(value = casos)
  
  dcovid
  
  maxfecha <- dcovid %>% 
    pull(fecha) %>% 
    max() %>% 
    format("%A %e de %B")
  

  highchart(type = "map") %>%
    hc_add_series(
      mapData = gransantiago_geojson,
      data = list_parse(dcovid),
      # "COMUNA" es la key en el geojson, "code" es la key en nuestros datos: dvar
      joinBy = c("COMUNA", "comuna"),
      showInLegend = FALSE,
      name = "Covid",
      borderColor = 'transparent',
      borderWidth = 0.1,
      animation = list(duration = PARS$hc$duration),
      dataLabels = list(
        enabled = TRUE, 
        format = "{point.nom_comuna}",
        color =  "white",
        style = list(fontSize = "12px", textOutline = "2px gray")
        )
      ) %>% 
    hc_colorAxis(
      stops = color_stops(n = 100, colors = covpal(30)),
      startOnTick = FALSE,
      # min = 0,
      endOnTick =  FALSE
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      hideDelay = 500,
      delayForDisplay = 500,
      headerFormat = "{point.key}",
      pointFormatter = tooltip_chart(
        accesor = "ttdata",
        hc_opts = list(
          subtitle = list(text = "point.nom_comuna"),
          chart = list(backgroundColor = "white"),
          xAxis = list(type = "datetime", showLastLabel = TRUE, endOnTick = FALSE),
          yAxis = list(showLastLabel = TRUE, endOnTick = FALSE),
          credits = list(enabled = FALSE)
        ),
        height = 225,
        width = 400
      )
    ) %>% 
    # hc_title(
    #   text = "Casos COVID-19 en el Gran Santiago",
    #   align = "center"
    # ) %>% 
    # hc_subtitle(
    #   text = paste("Datos Ministerio de Ciencia; con última actualización el", maxfecha),
    #   align = "center"
    # ) %>% 
    hc_legend(symbolWidth = 400, align = "center", verticalAlign = "top") # %>% 
    # hc_add_dependency("plugins/tooltip-delay.js")
  
  
}

grafico_fallecidos_por_region <- function(){
  
  d <- serie_consolidado_region()
  
  d <- d %>% 
    select(Region, Fecha, fallecidos)
  
  d <- d %>% 
    mutate(
      Region = fct_reorder(Region, fallecidos, max),
      Region = fct_rev(Region)
      )
  
  hchart(
    d,
    "line",
    hcaes(Fecha, fallecidos, group = Region),
    visible = c(rep(TRUE, 5), rep(FALSE, 16 - 5))
    ) %>% 
    hc_colors(rev(covpal(4, end = 0.8))) %>% 
    hc_tooltip(
      table = TRUE,
      sort = TRUE,
      valueDecimal = 2
    ) %>% 
    hc_legend(
      layout = "proximate",
      verticalAlign = "top",
      align = "right"
    ) %>% 
    hc_yAxis(
      title = list(text = "Número de Fallecidos"),
      type = "logarithmic",
      min = 1,
      plotBands = list(
        list(
          from = 100,
          to = 1000,
          color = hex_to_rgba("#B22222", 0.15),
          zIndex = 1,
          label = list(
            text = "Primeros 100 fallecidos",
            verticalAlign = "bottom",
            y = -10,
            style = list(color = "white", fontSize = "0.8em")
          )
        ),
        list(
          from = 1000,
          to = 10000,
          color = hex_to_rgba("#8B0000", 0.35),
          zIndex = 1,
          label = list(
            text = "Primeros 1000 fallecidos",
            verticalAlign = "bottom",
            y = -10,
            style = list(color = "white", fontSize = "0.8em")
            )
          )
        )
      ) %>%
    hc_xAxis(
      title = list(text = "Fecha")
    ) 

  
}

grafico_tasa_desempleo <- function(){
  
  d <- readRDS("data/tasa_desempleo.rds") %>% 
    mutate(tasa_desempleo = 100 * tasa_desempleo)
  
  d2 <- mindicador::mindicador_importar_datos("tasa_desempleo", 2020) %>% 
    select(fecha, tasa_desempleo = valor)
  
  d <- bind_rows(d, d2) %>% 
    arrange(fecha) %>% 
    filter(year(fecha) >= 2007)
  
  hchart(
    d, 
    "line",
    hcaes(fecha, tasa_desempleo),
    name = "Tasa de desempleo",
    showInLegend = TRUE,
    color = PARS$color$danger,
    tooltip = list(valueDecimals = 2)
    ) %>% 
    hc_yAxis(
      # min = 5,
      # maxStaggerLines = 500,
      # tickamount = 5,
      tickPositions = c(5:12),
      labels = list(format = "{value:.1f}%"),
      title = list(text = "Tasa de desempleo")
    )
  
}

