#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "SCROLL"
output_name <- "tasa_pobreza_monetaria_15_dolares_diarios_ultimo_anio.csv"
analista <- "Daniel Schteingart"
fuente1 <- 'R494C317' # World Bank - Poverty and Inequality Platform - Poverty Line: 15

df_pip <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>%
  mutate(
    prioridad = case_when(
      reporting_level == "National" ~ 1L,
      reporting_level == "Urban"   ~ 2L,
      reporting_level == "Rural"    ~ 3L,
      TRUE                        ~ 99L
    )
  ) %>%
  group_by(anio, iso3) %>%
  slice_min(prioridad, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-prioridad)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_paises <- df_pip %>% 
  dplyr::filter(anio == max(anio)) %>% 
  left_join(geo_front, join_by(iso3 == geocodigoFundar)) %>% 
  mutate(tasa_pobreza_monetaria = headcount * 100) %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar, tasa_pobreza_monetaria, poverty_line)


df_latam <- df_pip %>% 
  dplyr::filter(anio == max(anio), region_code == "LCN") %>% 
  group_by(anio, geocodigoFundar = region_code) %>% 
  summarise(tasa_pobreza_monetaria = stats::weighted.mean(headcount * 100, reporting_pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  mutate(poverty_line = 15)

paises_hic <- c(
  "ABW", "AND", "ARE", "ASM", "ATG", "AUS", "AUT", "BEL",
  "BHR", "BHS", "BMU", "BRB", "BRN", "CAN", "CHE", "CHI",
  "CHL", "CUW", "CYM", "CYP", "CZE", "DEU", "DNK", "ESP",
  "EST", "FIN", "FRA", "FRO", "GBR", "GIB", "GRC", "GRL",
  "GUM", "GUY", "HKG", "HRV", "HUN", "IMN", "IRL", "ISL",
  "ISR", "ITA", "JPN", "KNA", "KOR", "KWT", "LIE", "LTU",
  "LUX", "LVA", "MAC", "MAF", "MCO", "MLT", "MNP", "NCL",
  "NLD", "NOR", "NRU", "NZL", "OMN", "PAN", "POL", "PRI",
  "PRT", "PYF", "QAT", "ROU", "SAU", "SGP", "SMR", "SVK",
  "SVN", "SWE", "SXM", "SYC", "TCA", "TTO", "URY", "USA",
  "VGB", "VIR"
)


df_hic <- df_pip %>% 
  filter(iso3 %in% paises_hic , anio == max(anio)) %>% 
  mutate(region_code = "HIC") %>% 
  group_by(anio, geocodigoFundar = region_code) %>% 
  summarise(tasa_pobreza_monetaria = stats::weighted.mean(headcount * 100, reporting_pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  mutate(poverty_line = 15)
 

df_world <- df_pip %>% 
  filter(anio == max(anio)) %>% 
  mutate(region_code = "WLD") %>% 
  group_by(anio, geocodigoFundar = region_code) %>% 
  summarise(tasa_pobreza_monetaria = stats::weighted.mean(headcount * 100, reporting_pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  mutate(poverty_line = 15)



df_output <- bind_rows(df_paises, df_latam, df_hic, df_world)



# Preparar datos para el gráfico
df_plot <- df_output %>%
    arrange(tasa_pobreza_monetaria) %>%
    mutate(
        orden = row_number(),
        geonombreFundar = factor(geonombreFundar, levels = unique(geonombreFundar)),
        etiqueta_barra = case_when(
            geocodigoFundar == "ARG" ~ "ARGENTINA",
            orden == 1 ~ as.character(geonombreFundar),
            orden == n() ~ as.character(geonombreFundar),
            geocodigoFundar == "HIC" ~ "Países desarrollados",
            geocodigoFundar == "LCN" ~ "América Latina y el Caribe",
            geocodigoFundar == "WLD" ~ "Promedio mundial",
            TRUE ~ "normal"
        )
    )

# Obtener nombres del primer y último país para los colores
primer_pais_nombre <- df_plot %>% filter(orden == 1) %>% pull(geonombreFundar) %>% as.character()
ultimo_pais_nombre <- df_plot %>% filter(orden == n()) %>% pull(geonombreFundar) %>% as.character()

# Crear vector de colores dinámico
valores_unicos <- unique(df_plot$etiqueta_barra)
colores <- setNames(
    rep("#68b7c3", length(valores_unicos)),
    valores_unicos
)
colores["ARGENTINA"] <- "#0072ad"
colores[primer_pais_nombre] <- "#0072ad"
colores[ultimo_pais_nombre] <- "#0072ad"
colores["Países desarrollados"] <- "#003c6e"
colores["América Latina y el Caribe"] <- "#003c6e"
colores["Promedio mundial"] <- "#1576a9"

# Obtener valores de referencia
valores_referencia <- df_output %>%
    dplyr::filter(geocodigoFundar %in% c("LCN", "WLD", "HIC", "ARG")) %>%
    select(geocodigoFundar, tasa_pobreza_monetaria)

valor_lcn <- valores_referencia %>% filter(geocodigoFundar == "LCN") %>% pull(tasa_pobreza_monetaria)
valor_wld <- valores_referencia %>% filter(geocodigoFundar == "WLD") %>% pull(tasa_pobreza_monetaria)
valor_hic <- valores_referencia %>% filter(geocodigoFundar == "HIC") %>% pull(tasa_pobreza_monetaria)
valor_arg <- valores_referencia %>% filter(geocodigoFundar == "ARG") %>% pull(tasa_pobreza_monetaria)

# Encontrar posiciones de las líneas de referencia
pos_lcn <- which(df_plot$tasa_pobreza_monetaria >= valor_lcn)[1]
pos_wld <- which(df_plot$tasa_pobreza_monetaria >= valor_wld)[1]
pos_arg <- which(df_plot$tasa_pobreza_monetaria >= valor_arg)[1]
pos_hic <- which(df_plot$tasa_pobreza_monetaria >= valor_hic)[1]

# Obtener el año del gráfico
anio_grafico <- unique(df_output$anio)[1]

# Calcular posición Y para las etiquetas (usar valor máximo en escala log)
y_max_log <- max(df_plot$tasa_pobreza_monetaria) * 1.5

# Crear el gráfico
p <- ggplot(df_plot, aes(x = orden, y = tasa_pobreza_monetaria)) +
    # Barras
    geom_col(aes(fill = etiqueta_barra), width = 0.8) + 
    # coord_cartesian(ylim = c(400, max(df_plot$anios_educacion_promedio) * 1.8)) +
    scale_fill_manual(values = colores, guide = "none") +
    # Caja de Argentina
    annotate("rect", xmin = pos_arg - (nchar("ARGENTINA") / 2) - 4, xmax = pos_arg + (nchar("ARGENTINA") / 2) + 4, 
             ymin = valor_arg * 1.05, ymax = valor_arg * 1.15,
             fill = "#ffffff", color = "#58a0c6", linewidth = 0.5) +
    annotate("text", x = pos_arg, y = valor_arg * 1.1, 
             label = "ARGENTINA", 
             color = "#0072ad", angle = 0, hjust = 0.5, vjust = 0.5, size = 2,
             fontface = "bold") +
    # Etiquetas de líneas de referencia - AMÉRICA LATINA
    annotate("text", x = pos_lcn, y = valor_lcn * 1.1, 
             label = "AMÉRICA \nLATINA", 
             color = "#003c6e", angle = 90, hjust = 0, vjust = 0.5, size = 2,
             fontface = "bold") +
    # Etiqueta PROMEDIO MUNDIAL
    annotate("text", x = pos_wld, y = valor_wld * 1.1, 
             label = "PROMEDIO \nMUNDIAL", 
             color = "#1576a9", angle = 90, hjust = 0, vjust = 0.5, size = 2,
             fontface = "bold") +
    # Etiquetas PAÍSES DESARROLLADOS
    annotate("text", x = pos_hic, y = valor_hic * 1.1, 
             label = "PAÍSES \nDESARROLLADOS", 
             color = "#003c6e", angle = 90, hjust = 0, vjust = 0.5, size = 2,
             fontface = "bold") +
    geom_text(data = df_plot %>% filter(orden == 1), 
              aes(x = orden , y = tasa_pobreza_monetaria * 1.1, label = geonombreFundar), 
              hjust = 1.1, vjust = 0.5, color = "black", size = 2, fontface = "bold") +
    geom_text(data = df_plot %>% filter(orden == n()), 
              aes(x = orden + 1, y = tasa_pobreza_monetaria * 1.1, label = geonombreFundar), 
              hjust = -0.1, vjust = 0.5, color = "black", size = 2, fontface = "bold") +
    # Título y caption
    labs(
        title = paste0("Tasas de pobreza monetaria en distintos países \n(línea de 13,7 dólares diarios a paridad de poder adquisitivo), ", anio_grafico),
        x = "",
        y = "",
        caption = "Fuente de datos: Banco Mundial, Poverty and Inequality Platform"
    ) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        panel.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.title = element_text(color = "#003c6e", size = 12, face = "bold", hjust = 0),
        plot.caption = element_text(color = "#003c6e", size = 8, hjust = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "#a4a4a4", size = 8),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "#a4a4a4"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = scales::alpha("#a4a4a4", 0.3), linewidth = 0.3),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(color = "#003c6e", size = 10)
    )

# Exporta el gráfico como SVG en la carpeta 'graficos' 
graficos_path <- "./scripts/subtopicos/SCROLL/graficos"
filename <- paste0(gsub("\\.csv", "", output_name), ".svg")
name_file <- file.path(graficos_path, filename)
ggsave(filename = name_file, plot = p, device = "svg", width = 8, height = 5)



df_anterior <- df_output

pks <- c('anio', 'geocodigoFundar', 'poverty_line')

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
    unidades = list("tasa_pobreza_monetaria" = "porcentaje", "poverty_line" = "dólares diarios a paridad de poder adquisitivo en pesos constantes 2021"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
