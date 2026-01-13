#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SCROLL"
output_name <- "ranking_le_evol.csv"
analista <- "Daniel Schteingart"
fuente1 <- 'R292C161' # Prados de la Escosura - AHDI


df_prados <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long) 


df_output <- df_prados %>% 
  dplyr::filter(anio >= 1870, variable_name == "Life Expectancy at Birth") %>% 
  rename(geocodigoFundar = iso3) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, le_birth = valor) %>% 
  drop_na(le_birth) %>% 
  group_by(anio) %>% 
  mutate(ranking = rank(-le_birth)) %>% 
  ungroup() 


seleccionados <- c("ARG", "ESP", "HKG", "CHL", "KOR")

df_plot <- df_output %>% 
    dplyr::filter(geocodigoFundar %in% seleccionados)


df_labels <- df_plot %>% 
    dplyr::filter(geocodigoFundar == "ARG", anio %in% c(1870, 1913, 2020))


# Crear el gráfico
p <- ggplot(df_plot, aes(x = anio, y = ranking, color = geonombreFundar)) + 
    geom_point(data = df_labels) +
    geom_line() +
    labs(
        title = "Ranking de la esperanza de vida al nacer de Argentina y países seleccionados, 1870-2020",
        x = "",
        y = "",
        caption = "Fuente de datos: Prados de la Escosura"
    ) + 
    geom_text(data = df_labels, aes(label = paste0("PUESTO ", ranking)), hjust = -0.1, vjust = 0.5, size = 2.5, fontface = "bold") +
    scale_y_reverse() +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        panel.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.title = element_text(color = "#003c6e", size = 12, face = "bold", hjust = 0),
        plot.caption = element_text(color = "#003c6e", size = 8, hjust = 1),
        axis.text.y = element_text(color = "#a4a4a4", size = 8),
        axis.ticks.y = element_line(color = "#a4a4a4"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = scales::alpha("#a4a4a4", 0.3), linewidth = 0.3),
        panel.grid.minor.y = element_blank()
    )


# Exporta el gráfico como SVG en la carpeta 'graficos' 
graficos_path <- "./scripts/subtopicos/SCROLL/graficos"
filename <- paste0(gsub("\\.csv", "", output_name), ".svg")
name_file <- file.path(graficos_path, filename)
ggsave(filename = name_file, plot = p, device = "svg", width = 8, height = 5)


df_anterior <- df_output

pks <- c('anio', 'geocodigoFundar', 'geonombreFundar')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
)


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
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 




# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidades = list("le_birth" = "años", "ranking" = "unidades"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
