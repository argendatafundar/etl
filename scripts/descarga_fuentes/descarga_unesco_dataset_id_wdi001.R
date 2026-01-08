# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

source("scripts/utils/unesco_api_v21.R")

datasetId <- "wdi001"
# Ejemplo de uso
indicador_seleccionado <- UNESCO_v21.get_catalog() %>% 
dplyr::filter(dataset_id == datasetId)

title <- indicador_seleccionado$metas$default$title %>%
    glue::glue("{.}, vía UNESCO API v2.1")

institucion <- "UNESCO Institute for Statistics (UIS)"

df_raw <- UNESCO_v21.get_dataset(dataset_id = datasetId)

download_filename <- glue::glue("UNESCO_{datasetId}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

url <- glue::glue("https://data.unesco.org/api/explore/v2.1/catalog/datasets/{datasetId}/exports/csv")

# Aplana (flatten) todas las columnas listas/data.frames anidadas en df_raw,
# dejando solo columnas vectoriales para uso analítico.
df_flat <- df_raw %>%
  tidyr::unnest_wider(where(is.list), names_sep = "_") %>%
  tidyr::unnest_wider(where(is.data.frame), names_sep = "_")

# Garantiza que quede 100% aplanado (repite para cualquier anidación extra)
while(any(purrr::map_lgl(df_raw, ~is.list(.) || is.data.frame(.)))) {
  df_raw <- df_raw %>%
    tidyr::unnest_wider(where(is.list), names_sep = "_") %>%
    tidyr::unnest_wider(where(is.data.frame), names_sep = "_")
}

df_flat %>% write_csv_fundar(., destfile)


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
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
