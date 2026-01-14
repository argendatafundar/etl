#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "SCROLL"
output_name <- "indicadores_resumen_argentina_evolucion.csv"
analista <- "Daniel Schteingart"
fuente1 <- 'R497C318' # Fundación Norte y Sur - Analfabetismo
fuente2 <- 'R292C161' # Prados de la Escosura - AHDI
fuente3 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format
fuente4 <- 'R216C87' # Human Development Report - Mean Years of Schooling.
fuente5 <- 'R498C0' # Gapminder - Child Mortality Rate. 0-5 year olds dying per 1000 born.
fuente6 <- 'R495C0' # Varieties of Democracy (V-Dem) - Index of Liberal Democracy
fuente7 <- 'R496C0' # UNESCO - Adult literacy rate, both sexes

# Analfabetismo 1869 - 2010
df_fnys <- argendataR::get_clean_path(fuente1) %>%    
    arrow::read_parquet(.)

# Life Expectancy y Years of Schooling 1870 - 2020
df_prados <- argendataR::get_clean_path(fuente2) %>% 
    arrow::read_parquet(.)

# Life Expectancy 1950 - 2100
df_wpp <- argendataR::get_clean_path(fuente3) %>% 
    arrow::read_parquet(.)

# Years of Schooling 1990 - 2023
df_hdr <- argendataR::get_clean_path(fuente4) %>% 
    arrow::read_parquet(.)

# Child Mortality Rate 0-5 year olds dying per 1000 born.
df_gapminder <- argendataR::get_raw_path(fuente5) %>% 
    read.csv(.)


# Varieties of Democracy (V-Dem)
ex <- new.env()
df_vdem <- argendataR::get_raw_path(fuente6) %>% 
  load(envir = ex) %>% 
  { ex$vdem }

# Index of Liberal Democracy
df_vdem_libdem <- df_vdem %>% 
  select(geocodigoFundar = country_text_id, anio = year, libdem_index = v2x_libdem) 

# Index of Women's Political Empowerment
df_vdem_women_empowerment <- df_vdem %>% 
  select(geocodigoFundar = country_text_id, anio = year, women_empowerment = v2x_gender) 

# UNESCO - Adult illitirate population 15+ years both sexes
df_unesco <- argendataR::get_raw_path(fuente7) %>% 
  read.csv(.)

#############################################
#  Tasa de alfabetización de adultos 15+ años
#############################################

df_fnys_total <- df_fnys %>% 
  dplyr::filter(genero == "Total") %>%
  mutate(geocodigoFundar = "ARG",
        fuente = "Fundación Norte y Sur", 
        metrica = "Tasa de alfabetización de adultos 15+ años",
        valor = 100 - valor) %>% 
  select(anio, geocodigoFundar, metrica, fuente, valor)


df_unesco_arg <- df_unesco %>% 
  dplyr::filter(geoUnit == "ARG") %>% 
  mutate(fuente = "UNESCO",
         metrica = "Tasa de alfabetización de adultos 15+ años") %>% 
  select(anio = year, geocodigoFundar = geoUnit, metrica, fuente, valor = value) %>% 
  filter(anio > max(df_fnys_total$anio))


df_literacy_rate <- bind_rows(df_unesco_arg, df_fnys_total) %>% 
  arrange(anio)

#############################################
#  Mortalidad infantil 0-5 años
#############################################

df_child_mortality_arg <- df_gapminder %>% 
  dplyr::filter(geo == "arg", time <= (year(Sys.Date())-2)) %>% 
  mutate(fuente = "Gapminder", 
         geocodigoFundar = "ARG", 
         metrica = "Tasa de mortalidad infantil 0-5 años, en porcentaje",
         valor = child_mortality_0_5_year_olds_dying_per_1000_born / 10) %>% 
  select(anio = time, geocodigoFundar, metrica, fuente, valor) %>% 
  arrange(anio)

#############################################
# Esperanza de vida al nacer
#############################################

df_prados_le_arg <- df_prados %>% 
  dplyr::filter(iso3 == "ARG", variable_name == "Life Expectancy at Birth") %>% 
  mutate(fuente = "Prados de la Escosura", 
         metrica = "Esperanza de vida al nacer") %>% 
  select(anio, geocodigoFundar = iso3, metrica, fuente, valor) %>% 
  arrange(anio)

df_wpp_le_arg <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  mutate(fuente = "World Population Prospects (UN)", 
         metrica = "Esperanza de vida al nacer",
         valor = l_ex) %>% 
  select(anio = time, geocodigoFundar = iso3_code, metrica, fuente, valor) %>% 
  arrange(anio)

df_le_arg <- bind_rows(
    df_prados_le_arg, 
    df_wpp_le_arg %>% 
      dplyr::filter(anio >= max(df_prados_le_arg$anio))
    ) %>% 
  arrange(anio) %>% 
  dplyr::filter(anio <= (year(Sys.Date())-1))

#############################################
# Años de escolarización promedio
#############################################

df_prados_mys_arg <- df_prados %>% 
  dplyr::filter(iso3 == "ARG", variable_name == "Years of Schooling") %>% 
  mutate(fuente = "Prados de la Escosura", 
         metrica = "Años de escolarización promedio de la población mayor a 15 años") %>% 
  select(anio, geocodigoFundar = iso3, metrica, fuente, valor) %>% 
  arrange(anio)


df_hdr_mys_arg <- df_hdr %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  mutate(fuente = "Human Development Report", 
         metrica = "Años de escolarización promedio de la población mayor a 15 años") %>% 
  select(anio, geocodigoFundar = iso3, metrica, fuente, valor = mean_years_schoolling) %>% 
  arrange(anio)

df_mys_arg <- bind_rows(df_prados_mys_arg, df_hdr_mys_arg %>% dplyr::filter(anio > max(df_prados_mys_arg$anio))) %>% 
  arrange(anio)

#############################################
# Índice de democracia liberal
#############################################

df_vdem_libdem_arg <- df_vdem_libdem %>% 
  dplyr::filter(geocodigoFundar == "ARG") %>% 
  mutate(fuente = "Varieties of Democracy (V-Dem)", 
         metrica = "Índice de democracia liberal") %>% 
  select(anio, geocodigoFundar, metrica, fuente, valor = libdem_index) %>% 
  arrange(anio)


 ############################################
 # Índice de empoderamiento político de las mujeres
 ############################################

df_vdem_women_empowerment_arg <- df_vdem_women_empowerment %>% 
  dplyr::filter(geocodigoFundar == "ARG") %>% 
  mutate(fuente = "Varieties of Democracy (V-Dem)", 
         metrica = "Índice de empoderamiento político de las mujeres") %>% 
  select(anio, geocodigoFundar, metrica, fuente, valor = women_empowerment) %>% 
  arrange(anio)


df_output <- bind_rows(
  df_literacy_rate, 
  df_child_mortality_arg, 
  df_le_arg, 
  df_mys_arg, 
  df_vdem_libdem_arg, 
  df_vdem_women_empowerment_arg
  ) %>% 
  drop_na(valor)


df_plot <- df_output %>% 
  mutate(metrica = factor(
    case_when(
      metrica == "Esperanza de vida al nacer" ~ "Esperanza de vida al nacer",
      metrica == "Tasa de alfabetización de adultos 15+ años" ~ "Tasa de alfabetismo (15 años y más)",
      metrica == "Tasa de mortalidad infantil 0-5 años, en porcentaje" ~ "Mortalidad infantil (hasta 5 años), en %",
      metrica == "Años de escolarización promedio de la población mayor a 15 años" ~ "Años de escolarización de la población (15 años y más)",
      metrica == "Índice de democracia liberal" ~ "Índice de democracia liberal",
      metrica == "Índice de empoderamiento político de las mujeres" ~ "Índice de empoderamiento político de las mujeres"
    ),
    levels = c(
      "Esperanza de vida al nacer",
      "Mortalidad infantil (hasta 5 años), en %",
      "Años de escolarización de la población (15 años y más)",
      "Tasa de alfabetismo (15 años y más)",
      "Índice de democracia liberal",
      "Índice de empoderamiento político de las mujeres"
    )))



# Definir límites del eje Y para cada faceta
limites_y <- data.frame(
  metrica = factor(
    c(
      "Esperanza de vida al nacer",
      "Mortalidad infantil (hasta 5 años), en %",
      "Años de escolarización de la población (15 años y más)",
      "Tasa de alfabetismo (15 años y más)",
      "Índice de democracia liberal",
      "Índice de empoderamiento político de las mujeres"
    ),
    levels = levels(df_plot$metrica)
  ),
  ymin = c(0, 0, 0, 0, 0, 0),  # Valores inferiores (ajustar según necesidad)
  ymax = c(80, 40, 12, 100, 1, 1)  # Valores superiores (ajustar según necesidad)
)

# Crear el gráfico
p <- ggplot(df_plot, aes(x = anio, y = valor)) + 
    geom_line(color = "#003c6e") +
    facet_wrap(~metrica, ncol = 3, scales = "free", labeller = label_wrap_gen(width = 30)) +
    labs(
        title = "Indicadores resumen de acceso a salud, educación, calidad democrática e igualdad de género",
        x = "",
        y = "",
        caption = "Fuente de datos: Fundación Norte y Sur, Prados de la Escosura, World Population Prospects (UN), Human Development Report, Gapminder, Varieties of Democracy (V-Dem, 2025)"
    ) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        panel.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.title = element_text(color = "#003c6e", size = 12, face = "bold", hjust = 0),
        plot.caption = element_text(color = "#003c6e", size = 8, hjust = 1),
        axis.text.y = element_text(color = "#a4a4a4", size = 8),
        axis.ticks.y = element_line(color = "#a4a4a4"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = scales::alpha("#a4a4a4", 0.3), linewidth = 0.3),
        panel.grid.minor.y = element_blank()
    )

# Aplicar límites por faceta usando ggh4x::facetted_pos_scales
if (requireNamespace("ggh4x", quietly = TRUE)) {
  p <- p + ggh4x::facetted_pos_scales(
    y = lapply(levels(df_plot$metrica), function(met) {
      limites <- c(limites_y$ymin[limites_y$metrica == met], 
                   limites_y$ymax[limites_y$metrica == met])
      scale_y_continuous(limits = limites)
    })
  )
} else {
  warning("ggh4x no está disponible. Instala con: install.packages('ggh4x') para habilitar límites por faceta.")
}

# Exporta el gráfico como SVG en la carpeta 'graficos' 
graficos_path <- "./scripts/subtopicos/SCROLL/graficos"
filename <- paste0(gsub("\\.csv", "", output_name), ".svg")
name_file <- file.path(graficos_path, filename)
ggsave(filename = name_file, plot = p, device = "svg", width = 8, height = 5)



df_anterior <- df_output

pks <- c('anio', 'geocodigoFundar', 'metrica', 'fuente')

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
    unidades = list("valor" = "unidades"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
