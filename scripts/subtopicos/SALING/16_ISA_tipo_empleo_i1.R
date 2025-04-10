#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '16_ISA_tipo_empleo_i1.R'
subtopico <- 'SALING'
output_name <- 'ISA_tipo_empleo_i1.csv'
id_fuente <- 183
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% 
  rename(ano = años) %>% 
  pivot_longer(-ano, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano','variable'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Ingresos laborales mensuales a precios constantes por tipo de empleo 1992 -2024. La version anterior del dataset era con ingresos laborales horarios",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano','variable'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = "ano",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
