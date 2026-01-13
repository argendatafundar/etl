#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SCROLL"
output_name <- "pib_per_capita_largo_plazo.csv"
analista <- "Daniel Schteingart"
fuente1 <- "R219C90" # Maddison - GDP per capita, PPP (constant 2011 international $)


df_madd <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_madd %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  select(anio, pib_per_capita_ppp_2011 = gdppc) %>% 
  drop_na(pib_per_capita_ppp_2011)



# Crear el gráfico
p <- ggplot(df_output, aes(x = anio, y = pib_per_capita_ppp_2011)) + 
    geom_line(color = "#003c6e") +
    labs(
        title = "PIB per cápita de Argentina 1820-2022 \n (en dólares ajustados por paridad de poder adquisitivo 2011)",
        x = "",
        y = "",
        caption = "Fuente de datos: Maddison Project Database (2023)"
    ) +
    scale_y_continuous(
        trans = "log2",
        breaks = c(1000, 2000, 4000, 8000, 16000, 32000),
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) +
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

pks <- c('anio')

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
    unidades = list("pib_per_capita_ppp_2011" = "dólares PPP 2011"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
