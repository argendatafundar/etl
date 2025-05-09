# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2022-04-06")
fecha_actualizar <- "Sin informacion"


url <- "https://cdn.produccion.gob.ar/cdn-cep/datos-por-actividad/puestos/puestos_privados_mensual_por_clae6.csv"

download_filename <- "puestos_privados_mensual_por_clae6.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

nombre <- "Puestos de trabajo en el sector privado (CLAE a 6 dígitos)"

institucion = "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )



actualizar_fuente_raw(id_fuente = 316,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name,
                      api = F)
