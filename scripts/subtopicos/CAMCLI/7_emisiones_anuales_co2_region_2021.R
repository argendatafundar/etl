################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Emisiones anuales de co2 por región pporcentaje de cada país sobre world de owid
#'

output_name <- "emisiones_anuales_co2_region_2021"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

evol_anua_co2_reg_2021<-readr::read_csv(argendataR::get_temp_path("R119C0"))
geonomenclador <- argendataR::get_nomenclador_geografico()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

## transformo los datos
evol_anua_co2_reg_2021 <- evol_anua_co2_reg_2021 %>% 
  select (2,7,8,81) %>% 
  rename(anio = "2021")

# armo valor referencia worl
valor_referencia <- evol_anua_co2_reg_2021 %>%
  filter(entities_code == "OWID_WRL") %>%
  select(anio) %>%
  pull()

#  doy formato a porcent
evol_anua_co2_reg_2021 <- evol_anua_co2_reg_2021 %>%
  mutate(valor_en_porcent = sprintf("%.9f", (anio / valor_referencia)))

# final
evol_anua_co2_reg_2021 <- evol_anua_co2_reg_2021 %>%
  inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar")) %>% 
  mutate(anio = 2021)  %>%  
  select(3,10,8,4,5)  %>% 
  rename(
    iso3 = entities_code,
    continente_fundar = continente_fundar,
    iso3_desc_fundar = desc_fundar,
    anio = anio,
    valor_en_porcent = valor_en_porcent
  ) %>% 
  mutate(valor_en_porcent = as.numeric(valor_en_porcent))

view(evol_anua_co2_reg_2021)
  
# elimino na
evol_anua_co2_reg_2021 <- na.omit(evol_anua_co2_reg_2021)

df_output <- evol_anua_co2_reg_2021

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  evol_anua_co2_reg_2021,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("iso3","anio"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R119C0"),
    analista = "",
    pk = c("iso3","anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #    columna_geo_referencia = "",
    #    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_porcent" = "Porcentaje emisiones anuales co2"),
    unidades = list("valor_en_porcent" = "%"),
    directorio = "data/CAMCLI/"
  )