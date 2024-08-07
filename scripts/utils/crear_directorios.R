# Preparacion de la estructura de carpetas
# Se lee la estructura de carpetas de datasets de subtopicos para replicarla


walk(argendataR::subtopicos_dir()$tree$name, function(x) {
  if (!dir.exists(glue::glue("scripts/subtopicos/{x}"))) {
    
    dir.create(glue::glue("scripts/subtopicos/{x}"))
  }
  
  if (!file.exists(glue::glue("scripts/subtopicos/{x}/fuentes_{x}.R"))) {
    
    file.create(glue::glue("scripts/subtopicos/{x}/fuentes_{x}.R"))
    
  }
})

if (!dir.exists("data")) {dir.create("data")}

walk(subtopicos_dir()$tree$name, function(x) {
  
  
  if (!dir.exists(glue::glue("data/{x}"))) {
    
    dir.create(glue::glue("data/{x}"))
  }
})

if (!dir.exists("data/_FUENTES")) {
  dir.create("data/_FUENTES")
  dir.create("data/_FUENTES/clean")
  dir.create("data/_FUENTES/raw")
}

if (!dir.exists("data/_FUENTES/raw")) {
  
  dir.create("data/_FUENTES/raw")
  
}

if (!dir.exists("data/_FUENTES/clean")) {
  
  dir.create("data/_FUENTES/clean")
  
}
