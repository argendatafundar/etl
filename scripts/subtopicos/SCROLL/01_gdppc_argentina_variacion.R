#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "SCROLL"
output_name <- "gdppc_argentina_variacion.csv"
analista <- "Daniel Schteingart"
fuente1 <- 'R220C0' # WB - GDP per capita (constant 2015 US$)
fuente2 <- 'R219C90' # Maddison


df_wb <- argendataR::get_raw_path(fuente1) %>% 
  read.csv() %>% 
  select(anio = year, iso3 = iso3c, gdppc_wb = `NY.GDP.PCAP.KD`) %>%
  arrange(anio) %>% 
  group_by(iso3) %>% 
  mutate(gdppc_wb_rate = 100*(gdppc_wb/lag(gdppc_wb)) - 100) %>% 
  ungroup()

df_maddison <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  select(anio, iso3, gdppc_maddison = gdppc) %>%
  arrange(anio) %>% 
  group_by(iso3) %>% 
  mutate(gdppc_maddison_rate = 100*(gdppc_maddison/lag(gdppc_maddison)) - 100) %>% 
  ungroup() 

df_stage <- df_wb %>% 
  dplyr::filter(iso3 == "ARG", !is.na(gdppc_wb_rate)) %>% 
  full_join(df_maddison %>% 
  dplyr::filter(iso3 == "ARG", !is.na(gdppc_maddison_rate)), join_by(anio, iso3)) %>% 
  arrange(anio)


df_output <- df_stage %>% 
  select(anio, gdppc_wb_rate, gdppc_maddison_rate) %>% 
  pivot_longer(-anio, names_to = "fuente", values_to = "valor") %>% 
  mutate(fuente = if_else(fuente == "gdppc_wb_rate", "WB", "MDP"),
         pais_nombre = "Argentina") %>%
  dplyr::filter(anio >= 1900, !(fuente == "MDP" & anio >= 2022), !(fuente == "WB" & anio < 2022))


p <- ggplot(df_output, aes(x = anio, y = valor)) +
  geom_hline(yintercept = 0, color = "white", linewidth = 1) +
  geom_line(color = "#0470a9", linewidth = 0.8) +
  labs(title = "Variación interanual del PIB per cápita en Argentina, 1900-2023",
       x = "", y = "",
       caption = "Fuente de datos: Maddison Project Database (2023) y Banco Mundial") +
  scale_x_continuous(breaks = seq(1900, 2024, 4)) +
  scale_y_continuous(breaks = seq(-20, 20, 5), labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#000d37", color = NA),
    panel.background = element_rect(fill = "#000d37", color = NA),
    plot.title = element_text(color = "white", size = 12, face = "bold", hjust = 0),
    plot.caption = element_text(color = "white", size = 9),
    axis.text = element_text(color = "#6490a5", size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(color = "white"),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = scales::alpha("white", 0.3), linewidth = 0.3),
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
    unidades = list("valor" = "porcentaje"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")


