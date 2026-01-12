# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ4K97z2gyOqLyjVF2erWss03s5SVIGmeEzQDMhQOHhegghssVBb3_FIeHDR0LiFw/pub?output=xlsx"

nombre <- "Demografía y estadísticas sociales"
institucion <- "Fundación Norte y Sur"

download_filename <- "fnys_demografia_y_estadisticas_sociales.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, mode = "wb",
              destfile = destfile)

# agregar_fuente_raw(
#   url = url,
#   nombre = nombre,
#   institucion = institucion,
#   actualizable = F,
#   fecha_actualizar = fecha_actualizar,
#   path_raw = download_filename,
#   script = code_name
# )

actualizar_fuente_raw(id_fuente = 497,
                       nombre = nombre,
                       institucion = institucion,
                       fecha_actualizar = fecha_actualizar,
                       path_raw = download_filename,
                       script = code_name)