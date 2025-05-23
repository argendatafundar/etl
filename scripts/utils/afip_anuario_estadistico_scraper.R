library(rvest)
library(httr)
library(xml2)
library(openxlsx)


parse_cabextract_list <- function(output_lines) {
  # Buscar el comienzo de la tabla (línea con guiones)
  line_start <- grep("^-{3,}\\+-{3,}\\+-{3,}", output_lines)
  if (length(line_start) == 0) stop("No se pudo identificar la tabla de archivos en la salida.")
  
  data_lines <- output_lines[(line_start + 1):length(output_lines)]
  
  df <- do.call(rbind, lapply(data_lines, function(line) {
    matches <- regmatches(line, regexec("^\\s*(\\d+) \\| ([0-9.]+) ([0-9:]+) \\| (.+)$", line))[[1]]
    if (length(matches) == 5) {
      data.frame(
        size = as.numeric(matches[2]),
        date = matches[3],
        time = matches[4],
        name = matches[5],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  df
}


cabextract_wrapper <- function(
    file,
    directory = NULL,
    list_files = FALSE,
    test_archive = FALSE,
    lowercase = FALSE,
    filter = NULL,
    single = FALSE,
    fix = FALSE,
    verbose = TRUE
) {
  if (!file.exists(file)) stop("El archivo CAB no existe: ", file)
  
  cmd <- "cabextract"
  args <- c()
  
  # Preparar directorio destino
  if (!is.null(directory)) {
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    args <- c(args, "--directory", shQuote(directory))
  }
  
  if (list_files)   args <- c(args, "--list")
  if (test_archive) args <- c(args, "--test")
  if (lowercase)    args <- c(args, "--lowercase")
  if (!is.null(filter)) args <- c(args, "--filter", shQuote(filter))
  if (single)       args <- c(args, "--single")
  if (fix)          args <- c(args, "--fix")
  
  args <- c(args, shQuote(file))
  
  result <- system2(cmd, args = args, stdout = TRUE, stderr = TRUE)
  
  if (list_files) {
    df <- parse_cabextract_list(result)
    if (verbose && !is.null(df)) print(df)
    return(df)
  }
  
  # Si hay filtro, devolver ruta absoluta de archivos extraídos
  if (!is.null(filter)) {
    file_list <- list.files(
      path = directory,
      pattern = glob2rx(filter),
      full.names = TRUE
    )
    return(normalizePath(file_list))
  }
  
  invisible(result)
}









afip_anuario_estadistico.extraer_links_afip = function(){
  
  
  # Define la URL base y la URL de la página a consultar
  url_base <- "https://contenidos.afip.gob.ar/institucional/estudios/archivos/estadisticasTributarias/"
  page_url <- "https://www.afip.gob.ar/estudios/anuario-estadisticas-tributarias/"
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Filtra los links que contienen la URL base
  urls <- links[grepl(url_base, links)]
  
  # Extrae los años de los links
  anios <- as.numeric(sub(".*(\\d{4})\\.zip$", "\\1", urls))
  
  
  # Me quedo con 2014 nomás. 
  datos <- data.frame(anio = anios, url = urls)
  
  return(datos)
}


afip_anuario_estadistico.descargar_zip = function(anio, url){
  
  
  # Armo destfile
  carpeta_unzip <- glue::glue("estadisticasTributarias{anio}")
  archivo_zip <- glue::glue("{carpeta_unzip}.zip")
  destfile <- glue::glue("{tempdir()}/{archivo_zip}")
  
  
  tryCatch(
    {
      # Intentamos ejecutar descarga
      download.file(url = url, destfile = destfile)
      cat_str <- glue::glue("Descargó el archivo del año {anio}\n\n")
      cat(cat_str)
      return(destfile)
    },
    error = function(e) {
      # Si ocurre un error, lo capturamos y mostramos un mensaje
      message("Error: ", conditionMessage(e))
      return(NA)
    }
  )
  
  
}

afip_anuario_estadistico.unzip_to_folder = function(anio, destfile){
  
  carpeta_unzip <- glue::glue("estadisticasTributarias{anio}")
  exdir <- glue::glue("{tempdir()}/{carpeta_unzip}")
  
  tryCatch(
    {
      # Intentamos ejecutar descarga
      unzip(destfile, exdir = exdir)
      cat_str <- glue::glue("Descrompirmió archivo zip del año: {anio}\n\n")
      cat(cat_str)
      return(exdir)
      
    },
    error = function(e) {
      # Si ocurre un error, lo capturamos y mostramos un mensaje
      message("Error: ", conditionMessage(e))
      return(NA)
    }
  )
  
}

afip_anuario_estadistico.search_file = function(anio, unzipped_folder, formatos_archivos =c("2.1.1.4.xls", "2.1.1.4.htm", "2.1.1.4.xlsx")){
  
  list_archivos <- list.files(unzipped_folder, recursive = T, full.names = TRUE)
  
  # resultado_largo <- list_archivos[grepl("^2\\.1\\.1\\.4\\.*", basename(list_archivos))]
  # 
  # cat(anio, basename(resultado_largo), "\n\n")
  
  # Filtra los archivos que coincidan con los nombres definidos
  archivo_path <- list_archivos[basename(list_archivos) %in% formatos_archivos]
  archivo_path <- ifelse(length(archivo_path) == 0, NA, archivo_path)
  cat_str <- ifelse(!is.na(archivo_path), glue::glue("Descrompirmió archivo {anio}\n\n"), glue::glue("No se encontró archivo con los formatos declarados para el año {anio}\n\n"))
  
  cat(cat_str)
  
  return(archivo_path)  
  
}


afip_anuario_estadistico.buscar_sheet_htm = function(zip_path, ruta_archivo){
  
  nombre_sin_extension <- tools::file_path_sans_ext(basename(ruta_archivo))
  
  # Escapar los puntos y generar la string final
  nombre_escapado <- gsub("\\.", "\\\\.", nombre_sin_extension)
  
  # Priorizo sheet002 ante sheet001, porque tiene más desagregacion. 
  patron2 <- sprintf(".*%s.*\\/sheet002\\.htm$", nombre_escapado)
  
  patron1 <- sprintf(".*%s.*\\/sheet001\\.htm$", nombre_escapado)
  
  resultado2 <- unzip(zip_path, list = TRUE) %>% 
    dplyr::filter(Length>0, grepl(patron2, Name)) %>% 
    pull(Name)
  
  resultado1 <- unzip(zip_path, list = TRUE) %>% 
    dplyr::filter(Length>0, grepl(patron1, Name)) %>% 
    pull(Name)
  
  resultados <- c(resultado2, resultado1)
  
  resultado <- resultados[!is.na(resultados)]
  
  coincidencia <- NA_character_
  
  if (length(resultado) > 0) {
    
    coincidencia <- resultado[1]
    
  }
  
  return(coincidencia)
  
}



afip_anuario_estadistico.htm_to_xlsx <- function(anio, sheet001_path, htm_data_file) {
  
  sheet001 <- basename(sheet001_path)
  
  cat_str <- glue::glue("Parseando archivo {sheet001} correspondiente al anio {anio}\n")
  print(cat_str)
  
  outpath <- NA
  
  if (!is.na(sheet001_path)) {
    # Leer el archivo HTM y buscar las tablas en el tbody
    html_data <- read_html(sheet001_path)
    
    # Buscar todas las tablas en el archivo
    tables <- xml2::xml_find_all(html_data, "//table")
    
    # Verificar si hay tablas y seleccionar la más grande
    if (length(tables) >= 2) {
      # Inicializar variables para encontrar la tabla más grande
      max_rows <- 0
      largest_table <- NULL
      
      # Iterar sobre las tablas para encontrar la más grande
      for (table in tables) {
        rows <- xml2::xml_find_all(table, ".//tr")
        num_rows <- length(rows)
        
        # Comparar el número de filas
        if (num_rows > max_rows) {
          max_rows <- num_rows
          largest_table <- table
        }
      }
      
      # Asignar la tabla más grande a la variable 'table'
      table <- largest_table
    } else if (length(tables) == 1) {
      table <- tables[1]
      warning(glue::glue("Advertencia: Solo se encontró una tabla en {sheet001} para el año {anio}."))
    } else {
      warning(glue::glue("No se encontró ninguna tabla en {sheet001} para el año {anio}."))
      return(NA)
    }
    
    # Extraer las filas de la tabla si se ha encontrado una
    rows <- xml2::xml_find_all(table, ".//tr")
    
    # Crear un nuevo archivo de Excel
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet 1")
    
    # Iterar sobre cada fila para escribir en el Excel
    for (i in seq_along(rows)) {
      # Extraer todas las celdas <td> de la fila
      cells <- xml_find_all(rows[[i]], ".//td")
      
      # Iterar sobre cada celda y escribir el texto en la columna correspondiente
      for (j in seq_along(cells)) {
        cell <- cells[[j]]
        
        # Condición para el primer <tr>
        if (i == 1) {
          # En el primer <tr>, solo omitimos <td> con width="0"
          if (xml_attr(cell, "width") != "0") {
            cell_text <- xml_text(cell)
            writeData(wb, "Sheet 1", cell_text, startCol = j, startRow = i)
          }
        } else {
          # Para otros <tr>, omitimos <td> vacíos
          if (xml_text(cell) != "") {
            cell_text <- xml_text(cell)
            writeData(wb, "Sheet 1", cell_text, startCol = j, startRow = i)
          }
        }
      }
    }
    
    f <- basename(htm_data_file)
    f <- tools::file_path_sans_ext(f)
    
    outpath <- glue::glue("{tempdir()}/recuperado_{anio}_{f}.xlsx")
    
    saveWorkbook(wb, outpath, overwrite = TRUE)
    cat("Se escribió en disco el archivo", outpath, "\n\n")
  }
  
  return(outpath)
}










afip_anuario_estadistico.a_fuente_raw <- function(datos, code_name, tematica_archivo, actualizar = FALSE){
  
  anios_scrapeados <- datos %>% dplyr::filter(!is.na(archivo_path)) %>% pull(anio)
  
  
  for (y in anios_scrapeados){
    
    cat("Verificando año ", y, "\n\n")
    
    metadata_completar <- datos %>% dplyr::filter(anio == y) %>% select(anio, url_name, archivo_path)
    
    src <- metadata_completar$archivo_path
    
    download_filename <- basename(src) %>% gsub("_\\d{4}_", "_", .) %>% paste0(y,"_",.)
    
    dest <- glue::glue("{tempdir()}/{download_filename}")
    
    file.copy(from = src, to = dest)
    
    name <- glue::glue("Anuario estadísticas tributarias. {tematica_archivo}. Año {y}.")
    
    fuente_row <- fuentes_raw() %>% dplyr::filter(institucion == "AFIP" & grepl(y, path_raw)) 
    
    existe_fuente <- fuente_row %>% nrow() == 1
    
    if(existe_fuente){
      
      if(actualizar){
      
        id_fuente <- fuente_row$id_fuente
      
      actualizar_fuente_raw(
        id_fuente = id_fuente,
        nombre = name,
        fecha_actualizar = "Sin informacion",
        path_raw = download_filename,
        directorio = tempdir()
        
      )
        
        cat("Se actualizó: ", name,"\n\n")
      
      }  
    }else{
      
      agregar_fuente_raw(
        
        url = metadata_completar$url_name,
        nombre = name,
        institucion = "AFIP",
        actualizable = F,
        fecha_actualizar = "Sin informacion",
        script = code_name,
        path_raw = download_filename,
        directorio = tempdir()
        
      )
      
      cat("Se agregó: ", name, "\n\n")
      
    }
  }
}




