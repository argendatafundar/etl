# CEPED - Salario real en dólares de paridad de poder adquisitivo de 2017 (PPA de consumo privado)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

path_raw <- "ceped_salario_usd_ppa_internacional.xlsx"

# Es necesario buscar la URL haciendo la consulta con el browser o hacerlo dinámicamente con Selenium
temp_url <- "https://ceped-data.shinyapps.io/ceped-data/_w_b9f27b87bf374662b8a839d5f39b0832/session/e98a6ea3f627ff530c71656b44433e99/download/salarios-download_database?w=b9f27b87bf374662b8a839d5f39b0832"

destfile <- glue::glue("{tempdir()}/{path_raw}")

outfolder <- glue::glue("{tempdir()}")

# Descargar el archivo
download.file(temp_url, destfile, mode = "wb")

nombre_fuente <- "Salario real en dólares de paridad de poder adquisitivo de 2017 (PPA de consumo privado)"

# agregar_fuente_raw(url = "https://ceped-data.shinyapps.io/ceped-data/",
#                    nombre = nombre_fuente,
#                    institucion = "Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED)",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = path_raw,
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 209, dir = tempdir(), fecha_actualizar = "Sin informacion")
