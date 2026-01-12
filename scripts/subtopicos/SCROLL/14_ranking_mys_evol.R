#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SCROLL"
output_name <- "ranking_mys_evol.csv"
fuente1 <- 'R292C161' # Prados de la Escosura - AHDI


df_prados <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long) 


df_output <- df_prados %>% 
  dplyr::filter(anio >= 1870, variable_name == "Years of Schooling") %>% 
  rename(geocodigoFundar = iso3) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, mys = valor) %>% 
  drop_na(mys) %>% 
  group_by(anio) %>% 
  mutate(ranking = rank(-mys, ties.method = "first")) %>% 
  ungroup() 


seleccionados <- c("ARG", "JPN", "RUS", "TWN", "ARE")

df_plot <- df_output %>% 
    dplyr::filter(geocodigoFundar %in% seleccionados)


df_labels <- df_plot %>% 
    dplyr::filter(geocodigoFundar == "ARG", anio %in% c(1870, 1970, 2000, 2020))


# Crear el gráfico
p <- ggplot(df_plot, aes(x = anio, y = ranking, color = geonombreFundar)) + 
    geom_point(data = df_labels) +
    geom_line() +
    labs(
        title = "Ranking de los años de escolarización de Argentina y países seleccionados, 1870-2020",
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