# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/bases_censo2022_RedatamX.zip"

nombre <- "Censo Nacional de Población, Hogares y Viviendas 2022. RedatamX. Viviendas particulares. Microdatos"
institucion <- "Instituto Nacional de Estadística y Censos"

download_filename <- "cpv2022.rxdb"

destfile <- glue::glue("{tempdir()}/{download_filename}")

  
# argendataR::agregar_fuente_raw(
#   url = url,
#   nombre = nombre,
#   institucion = institucion,
#   actualizable = F,
#   fecha_actualizar = fecha_actualizar,
#   path_raw = download_filename,
#   script = code_name
# )


argendataR::actualizar_fuente_raw(
  id_fuente = 499,
  url = url,
  nombre = nombre,
  institucion = institucion,
  fecha_actualizar = fecha_actualizar,
  path_raw = download_filename,
  script = code_name
)
