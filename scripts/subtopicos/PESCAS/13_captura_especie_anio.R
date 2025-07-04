# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "13_captura_especie_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)
fuente2 <- 'R330C205' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2013- 2021)
fuente3 <- 'R329C206' # MAGyP Desembarque por especie, anio, mes (1989 - 2012)

df_magyp_especie <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

df_magyp_puerto_flota_especie <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  bind_rows(
    argendataR::get_clean_path(fuente2) %>% 
      arrow::read_parquet(.)
  ) 


df_output <- df_magyp_especie %>% 
  mutate(
    especie_agregada = case_when(
      grepl("Merluza hubbsi.*", especie) ~ "Merluza Hubbsi",
      grepl("Calamar Illex*", especie) ~ "Calamar Illex",
      grepl("Langostino.*", especie) ~ "Langostino",
      TRUE ~ "Otras especies"
    )
  ) %>% 
  group_by(anio, especie_agregada) %>% 
  summarise(
    desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(
    
    df_magyp_puerto_flota_especie %>% 
      mutate(
        especie_agregada = case_when(
          grepl("Merluza hubbsi.*", especie) ~ "Merluza Hubbsi",
          grepl("Calamar Illex*", especie) ~ "Calamar Illex",
          grepl("Langostino.*", especie) ~ "Langostino",
          TRUE ~ "Otras especies"
        )
      ) %>% 
      group_by(anio, especie_agregada) %>% 
      summarise(
        desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
      ) %>% 
      ungroup()
  )


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c("anio", "especie_agregada")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = pk, # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

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



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  pk,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )
