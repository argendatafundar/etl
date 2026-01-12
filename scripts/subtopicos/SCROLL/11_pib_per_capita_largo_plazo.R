#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SCROLL"
output_name <- "pib_per_capita_largo_plazo.csv"
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
        title = "PIB per cápita de Argentina (en dólares ajustados por paridad de poder adquisitivo 2011), 1820-2022",
        x = "",
        y = "",
        caption = "Fuente de datos: CEPED, INDEC"
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