serie_letalidad <- function(){
  dfallecidos <- serie_nro_fallecidos()
  dcontagiados <- serie_nro_casos()
  
  dfallecidos_contagiados <- dfallecidos %>% 
    full_join(
      dcontagiados,
      by="dia"
    ) %>% 
    mutate(porc=nro_fallecidos/nro_casos) 

  dfallecidos_contagiados %>%
    filter(!is.na(nro_fallecidos)) %>% 
    select(dia, porc) 
  }

serie_nro_casos <- function(){
  
  dcasos_totales_cumulativos <- readRDS("data/producto3/CasosTotalesCumulativo.rds")
  
  dcasos_totales_cumulativos %>% 
    filter(Region == "Total") %>% 
    gather(dia, nro_casos, -Region) %>% 
    select(-Region) %>% 
    mutate(dia = ymd(dia))
  
}

serie_nro_examenes <- function(){
  
  dcasos_examenes <- readRDS('data/producto7/PCR.rds')
  
  dcasos_examenes %>% 
    mutate_if(is.numeric, replace_na, 0) %>% 
    summarise_if(is.numeric, sum) %>% 
    select(-1) %>% 
    gather(dia, nro_examenes) %>% 
    mutate(dia = ymd(dia))
}

serie_nro_examenes_establecimiento <- function(){
  
  dcasos_examenes <- readRDS('data/producto17/PCREstablecimiento.rds')
  
  dcasos_examenes %>% 
    mutate_if(is.numeric, replace_na, 0) %>% 
    mutate(dia = ymd(fecha))
}

serie_nro_fallecidos <- function(){
  
  dfallecidos <- readRDS('data/producto14/FallecidosCumulativo_T.rds')
  dfallecidos %>% 
    select(dia = Region, nro_fallecidos  =Total)
  
}

serie_nro_pascientes_UCI <- function(){
  
  dpascientes_UCI <- readRDS('data/producto8/UCI_T.rds')
  
  dpascientes_UCI %>% 
    filter(!Region %in% c("Codigo region", "Poblacion")) %>% 
    gather(ciudad, valor, -Region) %>% 
    group_by(dia = Region) %>% 
    summarise(nro_pascientes_uci = sum(as.numeric(valor))) %>% 
    mutate(dia = ymd(dia))
  
}

serie_recuperados <- function(){
  
  dtotales_nacionales <- readRDS('data/producto5/TotalesNacionales_T.rds')
  
  dtotales_nacionales %>% 
    select(dia = Fecha, casos_recuperados = `Casos recuperados`) %>% 
    mutate(dia = ymd(dia))
}

serie_consolidado_region <- function(){
  
  examenes <- readRDS("data/producto7/PCR.rds")
  
  examenes_nuevos <- examenes %>% 
    mutate_if(is.numeric, replace_na, 0) %>% 
    gather(Fecha, examenes, -Region, -`Codigo region`, -Poblacion) %>% 
    mutate(Fecha = ymd(Fecha)) %>% 
    group_by(Region, Poblacion) %>% 
    mutate(examenes = cumsum(examenes))
  
  fallecidos <- readRDS("data/producto14/FallecidosCumulativo_T.rds")
  
  fallecidos_nuevos <- fallecidos %>% 
    select(-Total) %>% 
    rename(Fecha = Region) %>% 
    gather(Region, fallecidos, - Fecha)
  
  casos_nuevos <- readRDS("data/producto13/CasosNuevosCumulativo_std.rds")
  
  casos_nuevos <- casos_nuevos %>% rename(casos_nuevos = Total)
  
  dpascientes_UCI <- readRDS("data/producto8/UCI_T.rds")
  
  uci_nuevos <- dpascientes_UCI %>% 
    filter(!Region %in% c("Codigo region", "Poblacion")) %>% 
    rename(Fecha = Region) %>% 
    gather(Region, nro_pacientes_uci, - Fecha) %>% 
    mutate(Fecha = ymd(Fecha)) %>% 
    mutate(nro_pacientes_uci = as.numeric( nro_pacientes_uci))
  
  d <- examenes_nuevos %>% 
    left_join(casos_nuevos, by = c("Region", "Fecha")) %>% 
    left_join(fallecidos_nuevos, by = c("Region", "Fecha")) %>% 
    left_join(uci_nuevos, by = c("Region", "Fecha")) %>% 
    ungroup()
  
  d
  
}

serie_nro_casos_comuna <- function(){
  
  d <- readRDS("data/producto15/FechaInicioSintomas_std.rds")
  
  d %>% 
    mutate(
      `Semana Epidemiologica` = as.numeric(
        str_remove(`Semana Epidemiologica`, "SE"))) %>% 
    mutate(fecha = ymd("2020-01-01") + weeks(`Semana Epidemiologica` - 1))
  
}

serie_ventiladores <- function(){
  
  readRDS("data/producto20/NumeroVentiladores_T.rds") %>% 
    mutate(dia = ymd(Ventiladores)) %>% 
    mutate(porc = disponibles / total) 
  
}

serie_tasa_desempleo <- function() {
  
  d <- readRDS("data/tasa_desempleo.rds") %>% 
    mutate(tasa_desempleo = 100 * tasa_desempleo)
  
  d2 <- mindicador::mindicador_importar_datos("tasa_desempleo", 2020) %>% 
    select(fecha, tasa_desempleo = valor)
  
  d <- bind_rows(d, d2) %>% 
    arrange(fecha) %>% 
    filter(year(fecha) >= 2007)
  
  d
  
}

