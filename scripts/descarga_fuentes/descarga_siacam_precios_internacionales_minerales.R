code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-07-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://cdn.produccion.gob.ar/cdn-mineria/Datos-Abiertos-SIACAM/precios-internacionales-minerales.xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "precios-internacionales-minerales.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = "Precios Internacionales de Minerales. Precios internacionales de oro, plata, cobre, carbonato de litio, uranio, hierro y aluminio, datos por mes."
institucion = "Ministerio de Economía. Secretaría de Minería. Subsecretaría de Desarrollo Minero"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 272,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)