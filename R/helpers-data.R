descargar_datos <- function(){
  
  suppressWarnings({
    dir.create("data")
    dir.create("data/producto3")
    dir.create("data/producto4")
    dir.create("data/producto5")
    dir.create("data/producto7")
    dir.create("data/producto8")
    dir.create("data/producto14")
    dir.create("data/producto16")
    dir.create("data/producto20")
    dir.create("data/producto32")
  })
  
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv",
      col_types = cols(
        .default = col_double(),
        Region = col_character()
        )
      ),
    "data/producto3/CasosTotalesCumulativo.rds"
  )
  
  suppressWarnings({
    
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-04-08-CasosConfirmados-totalRegional.csv",
      col_types = cols(
        .default = col_double(),
        Region = col_character()
      )
    ),
    "data/producto4/2020-04-08-CasosConfirmados-totalRegional.rds"
  )
  
  })
    
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto7/PCR.csv",
      col_types = cols(
        .default = col_double(),
        Region = col_character(),
        `Codigo region` = col_character()
        )
      ),
    "data/producto7/PCR.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario.csv",
      col_types = cols(
        .default = col_double(),
        `Grupo de edad` = col_character(),
        Sexo = col_character()
        )
      ),
    "data/producto16/CasosGeneroEtario.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto32/Defunciones.csv",
      col_types = cols(
        .default = col_double(),
        Region = col_character(),
        Comuna = col_character()
        )
      ),
    "data/producto32/Defunciones.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo_T.csv",
      col_types = cols(
        Region = col_date(format = ""),
        `Arica y Parinacota` = col_double(),
        Tarapacá = col_double(),
        Antofagasta = col_double(),
        Atacama = col_double(),
        Coquimbo = col_double(),
        Valparaíso = col_double(),
        Metropolitana = col_double(),
        `O’Higgins` = col_double(),
        Maule = col_double(),
        Ñuble = col_double(),
        Biobío = col_double(),
        Araucanía = col_double(),
        `Los Ríos` = col_double(),
        `Los Lagos` = col_double(),
        Aysén = col_double(),
        Magallanes = col_double(),
        Total = col_double()
      )
      ),
    "data/producto14/FallecidosCumulativo_T.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto8/UCI_T.csv",
      col_types = cols(
        Region = col_character(),
        `Arica y Parinacota` = col_double(),
        Tarapacá = col_character(),
        Antofagasta = col_character(),
        Atacama = col_character(),
        Coquimbo = col_character(),
        Valparaíso = col_character(),
        Metropolitana = col_double(),
        `O’Higgins` = col_character(),
        Maule = col_character(),
        Ñuble = col_double(),
        Biobío = col_character(),
        Araucanía = col_character(),
        `Los Ríos` = col_double(),
        `Los Lagos` = col_double(),
        Aysén = col_double(),
        Magallanes = col_double()
        )
      ),
    "data/producto8/UCI_T.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv",
      col_types = cols(
        Fecha = col_date(format = ""),
        `Casos nuevos con sintomas` = col_double(),
        `Casos totales` = col_double(),
        `Casos recuperados` = col_double(),
        Fallecidos = col_double(),
        `Casos activos` = col_double(),
        `Casos nuevos sin sintomas` = col_double(),
        `Casos nuevos totales` = col_double()
        )
      ),
    "data/producto5/TotalesNacionales_T.rds"
  )
  
  saveRDS(
    read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto20/NumeroVentiladores_T.csv",
      col_types = cols(
        Ventiladores = col_date(format = ""),
        total = col_double(),
        disponibles = col_double(),
        ocupados = col_double()
        )
    ),
    "data/producto20/NumeroVentiladores_T.rds"
  )

}

# system.time(descargar_datos())
