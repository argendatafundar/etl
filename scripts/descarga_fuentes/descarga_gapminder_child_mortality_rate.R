# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "https://raw.githubusercontent.com/open-numbers/ddf--gapminder--child_mortality/refs/heads/master/ddf--datapoints--child_mortality_0_5_year_olds_dying_per_1000_born--by--geo--time.csv"

nombre <- "Child mortality rate, 0-5 year olds dying per 1000 born"
institucion <- "Gapminder (Open Numbers)"

download_filename <- "gapminder_child_mortality_0_5_year_olds_dying_per_1000_born.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

  
# argendataR::agregar_fuente_raw(
#   url = url,
#   nombre = nombre,
#   institucion = institucion,
#   actualizable = F,
#   fecha_actualizar = fecha_actualizar,
#   path_raw = download_filename,
#   script = code_name
# )


argendataR::actualizar_fuente_raw(
  id_fuente = 498,
  url = url,
  nombre = nombre,
  institucion = institucion,
  fecha_actualizar = fecha_actualizar,
  path_raw = download_filename,
  script = code_name
)



