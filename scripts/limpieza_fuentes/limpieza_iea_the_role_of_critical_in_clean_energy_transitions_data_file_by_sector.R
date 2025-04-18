code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 279
fuente_raw <- sprintf("R%sC0",id_fuente)

# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}



clean_scenario_range <- function(sheet_name, scenario_range, scenario_name){

data <- readxl::read_excel(get_raw_path(fuente_raw), 
                                 sheet = sheet_name,
                                 range = scenario_range,
                                 col_names = F)

data <- data[,!white_cols(data)]

names(data) <- c("to_split","2020","2030","2040")

sector <- c("Low-carbon generation", "EV and battery storage", 
            "Electricity networks", "Hydrogen")

sub_sector <- c("Solar PV", "Wind", "Hydro", 
                  "Biomass", "CSP", "Geothermal", 
                  "Nuclear", "EVs", "Battery storage",
                  "Transmission", "Distribution", "Transformer",
                  "Electrolyser", "FCEV")

# cuento cantidad de columnas
num_cols <- length(data)

# saco las filas que tienen (num_cols - 1) nulos
filter_bool <- check_na_threshold(data, num_cols-1)

clean_data <- data %>% 
  dplyr::filter(!filter_bool) %>% 
  mutate(
    sectors = ifelse(to_split %in% sector, to_split, NA),
    clean_energy_technologies = ifelse(to_split %in% sub_sector, to_split, NA),
    mineral = ifelse(!(to_split %in% c(sector, sub_sector)), to_split, NA)
    
  ) %>% 
  fill(sectors, .direction = "down") %>% 
  fill(clean_energy_technologies, .direction = "down") %>% 
  drop_na(mineral) %>% 
  select(-to_split) %>% 
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    names_transform = as.integer, 
    values_to = "demand_thousand_tonnes"
  ) %>% 
  mutate(
    scenario = scenario_name
  )

return(clean_data)

}


sheet_name <- "By sector"

STEPS_RANGE <- "A7:F132"
SUSDV_RANGE <- "H7:M132"

steps_scenario <- "Stated Policies Scenario"
susdv_scenario <- "Sustainable Development Scenario"


steps_clean <- clean_scenario_range(sheet_name = sheet_name, scenario_range = STEPS_RANGE, scenario_name = steps_scenario)
susdv_clean <- clean_scenario_range(sheet_name = sheet_name, scenario_range = SUSDV_RANGE, scenario_name = susdv_scenario)

df_clean <- bind_rows(steps_clean, susdv_clean)

# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Cuadro: {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 150
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("sectors","clean_energy_technologies","mineral", "scenario")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)


