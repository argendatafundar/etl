code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-12-11")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://cdn.produccion.gob.ar/cdn-mineria/Datos-Abiertos-SIACAM/Proveedores/Compras-por-rama-vendedora-agrupada.csv"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "Compras-por-rama-vendedora-agrupada.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = "Compras por rama vendedora agrupada, 2019. Datos de compras de las empresas mineras por rama vendedora agrupada, en porcentaje."
institucion = "Ministerio de Economía. Secretaría de Minería. Subsecretaría de Desarrollo Minero"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 278,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)