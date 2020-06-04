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
    summarise(nro_pascientes_uci = sum(as.numeric(valor)))
}

serie_recuperados <- function(){
  
  dtotales_nacionales <- readRDS('data/producto5/TotalesNacionales_T.rds')
  
  dtotales_nacionales %>% 
    select(dia = Fecha, casos_recuperados = `Casos recuperados`) %>% 
    mutate(dia = ymd(dia))
}



