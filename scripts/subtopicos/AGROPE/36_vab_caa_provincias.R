#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name_old <- "drawing_data.geojson"
output_name <- "vab_caa_provincias.csv"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R305C173" # Subsecretaría de Programación Regional y Sectorial Cadenas Productivas 


df_sprys <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

total_vab <- df_sprys %>% 
  filter(alcance_nombre == "Argentina", categoria_desc == "Agroindustriales") %>% 
  select(cadena_id = cadenas_id, total_cadena = valor)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long) %>% 
  dplyr::filter(grepl("AR-\\w$", geocodigoFundar)) %>% 
  mutate(provincia_join = toupper(stringi::stri_trans_general(geonombreFundar, "Latin-ASCII")))

df_output <- df_sprys %>%
  dplyr::filter(alcance_nombre != "Argentina", categoria_desc == "Agroindustriales") %>% 
  select(provincia_id = alcance_id, provincia = alcance_nombre, cadena_id =  cadenas_id, cadena = cadenas_desc, vab = valor) %>% 
  left_join(total_vab, join_by(cadena_id)) %>% 
  mutate(
    share = round(100 * vab / total_cadena,2)
  ) %>% 
  mutate(provincia = case_when(
    provincia == "CAPITAL FEDERAL" ~ "CABA",
    TRUE ~ provincia
  )
  ) %>% 
  left_join(geo_front, join_by(provincia == provincia_join)) %>% 
  select(geocodigoFundar, geonombreFundar, cadena, share) 




df_anterior <- df_geo %>% 
  mutate(provincia = toupper(nombres_provincia),
         cadena = tolower(cadena) %>% str_replace("^\\w", toupper),
         provincia_id = as.integer(id_provincia)) %>% 
  select(provincia_id, provincia, cadena, share = valor)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('provincia_id','cadena'), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


#-- Exportar Output ----

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name_old), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("provincia_id", 
                      "provincia",
                      "cadena",
                      "share"),
  descripcion = c("Número identificador de provincia",
                  "Provincia",
                  "Cadena agroinudstial",
                  "Participación del VAB de la cadena provincial en el VAB nacional de la cadena")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


aclaracion = c("Para la comparación se tomaron los datos del analista.",
               "Se utilizó una fuente de información distinta: los datos inicialmente fueron tomados de un excel no publico del Laboratorio de Desarrollo Sectorial y Territorial de la FCE-UNLP. En esta versión, se tomaron datos publicados en la web del MECON aunque difieren de los iniciales",
               "La modificación de la fuente trae aparejada una modificación sustancial de los nombres de las cadenas y los valores arrojados, es por ello que la comparación da muy mal",
               "Se modificó el nombre del archivo, de drawing_data.geojson a vab_caa_provincias.csv")

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c('provincia_id', 'cadena'),
    control = comparacion, 
    cambio_nombre_output = list('nombre_nuevo' = output_name, 'nombre_anterior' = output_name_old),
    cambio_nombre_cols = list("share" = "valor"),
    descripcion_columnas = descripcion,
    unidades = list("share" = "porcentaje"),
    aclaraciones = paste0(aclaracion, collapse = ". ")
  )


