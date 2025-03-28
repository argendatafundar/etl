code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2024-10-15")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/imf_api.R")


dataset <- imf.get_available_datasets() %>% dplyr::filter(ids == "PCPS")

database_id <- dataset$ids

database_title <- paste0(dataset$names, " - Data")

download_filename <- database_title %>% janitor::make_clean_names() %>% paste0("imf_",., ".csv")

destfile <- file.path(tempdir(), download_filename)

result <- imf.get_compact_data("PCPS")

url <- result$url

data <- result$data %>% janitor::clean_names()

data %>% argendataR::write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    nombre = database_title,
#                    institucion = "International Monetary Found",
#                    actualizable = T,
#                    script = code_name,
#                    api = T, 
#                    path_raw = download_filename,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 265,
                      url = url,
                      actualizable = T,
                      script = code_name,
                      api = T, 
                      path_raw = download_filename,
                      fecha_actualizar = fecha_actualizar)




