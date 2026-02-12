#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SCROLL"
output_name <- "salario_real_ppa2017.csv"
analista <- "Daniel Schteingart" 

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
  dplyr::filter(indicador == "Total general", (trim == "Total" | (trim == "2º trimestre" & anio == 2025))) %>% 
  select(-trim,-indicador)

# Tomamos el dato Total general/Total 
df_puestos_ar <- arrow::read_parquet(argendataR::get_fuente_path(fuente3)) %>% 
  dplyr::filter(indicador == "Total general", (trim == "Total" | (trim == "2º trimestre" & anio == 2025))) %>% 
  select(-trim,-indicador)

# # Tomamos el dato Total general/Total
df_puestos_anr <- arrow::read_parquet(argendataR::get_fuente_path(fuente4)) %>% 
  dplyr::filter(indicador == "Total general", (trim == "Total" | (trim == "2º trimestre" & anio == 2025))) %>% 
  select(-trim,-indicador)


df_ipc <- arrow::read_parquet(argendataR::get_fuente_path(fuente5)) %>% 
  dplyr::filter(region == "Nacional") %>% 
  dplyr::filter(descripcion == "Nivel general") %>% 
  dplyr::filter(anio != 2025 | (anio == 2025 & mes %in% c(7,8,9))) %>% 
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




anios_etiquetar <- c(1935, 1974, 2025)

df_labels <- df_output |>
  dplyr::filter(anio %in% anios_etiquetar)

p <- ggplot(
  df_output,
  aes(x = anio, y = salario_medio_real_ppa_consumo_privado_2017_base1970)
) +
  geom_line(color = "#003c6e", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1935, 2025, 5)) +

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

    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(color = "white"),
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
  ) + 
  ylim(0, 150)



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
    unidades = list("salario_medio_real_ppa_consumo_privado_2017_base1970" = "indice", "salario_medio_real_ppa_consumo_privado_2017_empalme" = "dólares PPP 2017"))



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
