# Censo Nacional 2022 - Resultados provisionales
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

source("scripts/utils/indec_scraper_links.R")


serie_cgi_id <- 49

pattern_vab <- ".*series_cgi_sexo.*\\.xls"


result <- INDEC.cuentas_nacionales.extraer_links(id = serie_cgi_id, pattern = pattern_vab)

url <- result$url

title_raw <- glue::glue("Cuentas Nacionales. Cuenta de Generación del Ingreso. {result$text}")


institucion = "Instituto Nacional de Estadísticas y Censos"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")
download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Cuenta de generación del ingreso (CGI). Remuneración al trabajo asalariado, ingreso mixto e insumo de mano de obra, por sexo y tramos de edad",
#                    institucion = "INDEC",
#                    actualizable = T,
#                    fecha_actualizar = fecha_actualizar,
#                    path_raw = download_filename,
#                    script = code_name
# )


actualizar_fuente_raw(id_fuente = 228,
                      url = url, 
                      nombre = title_raw, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)   
       