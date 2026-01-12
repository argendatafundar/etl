# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


source("scripts/utils/unesco_api.R")

indicators <- UNESCO.get_indicators()

indicator_code <- "LR.AG15T99"

indicator_selected <- indicators %>% 
dplyr::filter(indicatorCode == indicator_code)


result <- UNESCO.get_indicator_data(indicators = indicator_code, indicator_metadata = TRUE)

url <- result$url_consulta

response <- result$response

title <- paste0(indicator_selected$name, ", vía UNESCO API v2.1")

institucion <- "UNESCO Institute for Statistics (UIS)"

df_raw <- response$records

download_filename <- glue::glue("UNESCO_{indicator_code}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = title,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 496,
                      url = url, 
                      nombre = title, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
