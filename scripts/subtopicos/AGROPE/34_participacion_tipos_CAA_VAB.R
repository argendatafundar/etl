################################################################################
##                              Dataset: nombre                               ##
################################################################################
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "participacion_tipos_CAA_VAB"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R341C214"
fuente2 <- "R341C215"

df_caa_constantes <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  mutate(cadena_merge = toupper(stringi::stri_trans_general(cadena, "Latin-ASCII")))

df_caa_implicitos <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) %>% 
  mutate(cadena_merge = toupper(stringi::stri_trans_general(cadena, "Latin-ASCII"))) %>% 
  select(-cadena)


df_output <- df_caa_constantes %>% 
  inner_join(df_caa_implicitos, join_by(cadena_merge, anio)) %>% 
  select(-cadena_merge) %>% 
  mutate(vab_pcorr = vab * indice / 100, 
         unidad_medida = "millones de pesos corrientes") %>% 
  group_by(anio) %>% 
  mutate(
    share = vab_pcorr / sum(vab_pcorr)
  ) %>% 
  ungroup()  %>% 
  dplyr::filter(anio == max(anio)) %>% 
  select(cadena, vab_pcorr, share, unidad_medida)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(share = valor)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("cadena"), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 

# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("vab_pcorr",
                      "share",
                      "unidad_medida"
  ),
  descripcion = c(
    "Valor Agregado Bruto a pesos corrientes, en millones de pesos",
    "Participación de la cadena en el total de las CAA",
    "Unidad de medida de referencia"
  )
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


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("cadena"),
    es_serie_tiempo = F,
    control = comparacion, 
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('share' = 'valor'),
    unidades = list("vab_pcorr" = "millones de pesos corrientes",
                    "share" = "porporción")
  )

