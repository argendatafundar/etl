#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '18_ISA_tipo_empleo_i3.R'
subtopico <- 'SALING'
output_name <- 'ISA_tipo_empleo_i3.csv'
id_fuente <- 185
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% 
  pivot_longer(-pais, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('pais','variable'),
  drop_joined_df = F
)

print(comparacion)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Ingresos laborales mensuales por tipo de trabajo (en dólares a PPA). 2021 o año más cercano",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('pais','variable'),
    control = comparacion, 
    es_serie_tiempo = F,
    nivel_agregacion = "paises",
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
