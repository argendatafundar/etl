#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SALING"
output_name <- "salario_real_ppa2017_ceped.csv"

fuente1 <- "R209C0" # CEPED
fuente2 <- 'R35C83' # CGI RTA 
fuente3 <- 'R35C84' # CGI Puestos AR 
fuente4 <- 'R35C85' # CGI Puestos ANR 
fuente5 <- 'R127C54' # IPC nacional

# Del dataset descargado nos quedamos únicamente con 
# las filas donde 'nombre.variable' == Salario real en dólares 
# de paridad de poder adquisitivo de 2017 (PPA de consumo privado) 
# y las columnas iso3c, nombre.país, ANO4, valor
ceped.df <- readxl::read_excel(argendataR::get_fuente_path(fuente1)) %>% 
  dplyr::filter(nombre.variable == "Salario real en dólares de paridad de poder adquisitivo de 2017 (PPA de consumo privado)") %>% 
  select(iso3 = iso3c, anio = ANO4, salario_real_ppa_consumo_privado_2017 = valor) 


                
# Tomamos el dato Total general/Total
df_rta <- arrow::read_parquet(argendataR::get_fuente_path(fuente2)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)

# Tomamos el dato Total general/Total 
df_puestos_ar <- arrow::read_parquet(argendataR::get_fuente_path(fuente3)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)

# # Tomamos el dato Total general/Total
df_puestos_anr <- arrow::read_parquet(argendataR::get_fuente_path(fuente4)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)


df_ipc <- arrow::read_parquet(argendataR::get_fuente_path(fuente5)) %>% 
  dplyr::filter(region == "Nacional") %>% 
  dplyr::filter(descripcion == "Nivel general") %>% 
  group_by(anio) %>% 
  summarise(indice_ipc = mean(indice_ipc, na.rm = T)) %>% 
  ungroup()


impute_forward <- function(A, B) {
  
  result <- rep(NA_real_, length(A))
  
  # Calcular las variaciones relativas de B
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el último índice con un valor no nulo en A
  t0 <- max(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia adelante
  for (t in (t0 + 1):length(A)) {
    if (!is.na(VarB[t]) & is.na(A[t])) {
      result[t] <- result[t - 1] * VarB[t]
    }
  }
  
  return(result)
}  

df_indec_cgi <- df_rta %>% 
  left_join(df_puestos_ar, by = join_by(anio)) %>% 
  left_join(df_puestos_anr, by = join_by(anio)) %>%
  left_join(df_ipc, by = join_by(anio)) %>%
  mutate(salario_medio_nominal = valor_agregado_bruto * 1000 / (puestos_ar + puestos_anr),
         salario_medio_real_indec = salario_medio_nominal / indice_ipc) %>% 
  select(anio, salario_medio_real_indec)


df_ceped_arg <- ceped.df %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  select(-iso3)

anio_base <- 1970
valor_base <- df_ceped_arg[(df_ceped_arg$anio == anio_base), c("salario_real_ppa_consumo_privado_2017")][[1]]

df_output <- df_indec_cgi %>% 
  full_join(df_ceped_arg, by = join_by(anio)) %>% 
  arrange(anio) %>% 
  mutate(salario_medio_real_fwd = impute_forward(salario_real_ppa_consumo_privado_2017, salario_medio_real_indec),
         salario_medio_real_ppa_consumo_privado_2017_empalme = ifelse(!is.na(salario_real_ppa_consumo_privado_2017), salario_real_ppa_consumo_privado_2017, salario_medio_real_fwd) ,
         salario_medio_real_ppa_consumo_privado_2017_base1970 = 100 * salario_medio_real_ppa_consumo_privado_2017_empalme / valor_base) %>%
  select(anio, salario_medio_real_ppa_consumo_privado_2017_base1970, salario_medio_real_ppa_consumo_privado_2017_empalme)




# Crear el gráfico
p <- ggplot(df_output, aes(x = anio, y = salario_medio_real_ppa_consumo_privado_2017_base1970)) + 
    geom_line(color = "#003c6e") +
    labs(
        title = "Salario real en dólares 1935-2024 (año base 1970) \n(en PPA de consumo privado a precios de 2017)",
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


anios_etiquetar <- c(1948, 1974, 1984, 2003, 2015, 2024)

df_labels <- df_output |>
  dplyr::filter(anio %in% anios_etiquetar)

p <- ggplot(
  df_output,
  aes(x = anio, y = salario_medio_real_ppa_consumo_privado_2017_base1970)
) +
  geom_line(color = "#003c6e", linewidth = 0.8) +

  # Puntos en los años seleccionados
  geom_point(
    data = df_labels,
    color = "#003c6e",
    size = 2
  ) +

  # Etiquetas con el AÑO
  geom_text(
    data = df_labels,
    aes(label = anio),
    hjust = -0.1,
    vjust = -0.6,
    size = 2.5,
    color = "#0072ad",
    fontface = "bold"
  ) +

  labs(
    title = "Salario real en dólares 1935–2024 (año base 1970)",
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

    # Quitar eje X
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),

    axis.text.y = element_text(color = "#a4a4a4", size = 8),
    axis.ticks.y = element_line(color = "#a4a4a4"),

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      color = scales::alpha("#a4a4a4", 0.3),
      linewidth = 0.3
    ),
    panel.grid.minor.y = element_blank()
  )



# Exporta el gráfico como SVG en la carpeta 'graficos' 
graficos_path <- "./scripts/subtopicos/SCROLL/graficos"
filename <- paste0(gsub("\\.csv", "", output_name), ".svg")
name_file <- file.path(graficos_path, filename)
ggsave(filename = name_file, plot = p, device = "svg", width = 8, height = 5)