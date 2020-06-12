# 
# map_df(2009:2020, mindicador::mindicador_importar_datos, series = "tasa_desempleo")
# mindicador::mindicador_importar_datos("tasa_desempleo", 2009)
# mindicador::mindicador_importar_datos("tasa_desempleo", 2016) %>% glimpse()
# 
# mindicador:::mindicador_importar_datos_interna

readxl::read_excel("D:/git/macroeconomico-3/data/Variables_vs_tpm_desempleo_ipc_20200421.xlsx") %>% 
  select(FECHA, TASA_DESEMPLEO) %>% 
  rename_all(str_to_lower) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  filter(year(fecha) <= 2019) %>% 
  saveRDS("data/tasa_desempleo.rds")
