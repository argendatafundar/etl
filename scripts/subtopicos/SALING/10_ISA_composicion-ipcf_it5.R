#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '10_ISA_composicion-ipcf_it5.R'
subtopico <- 'SALING'
output_name <- 'ISA_composicion-ipcf_it5.csv'
id_fuente <- 167
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semestre','fuente'),
  drop_joined_df = F
)

print(comparacion)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Evolución de las distintas fuentes de ingresos no laborales, 2003-2024",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semestre','fuente'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semestre'),
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('indice' = 'Valor del ingreso real promedio en las distintas fuentes de ingreso (2003-II = 100)'),
    unidades = list('indice' = 'unidades')
  )
