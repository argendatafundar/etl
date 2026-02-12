#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(lubridate)

output_name <- "pobreza_largo_plazo.csv"

df_daniel <- readxl::read_excel("fuentes/pobreza serie empalmada intento2.xlsx", sheet = "limpio")

df_stage <- df_daniel %>% 
    select(1:3) %>% 
    pivot_longer(cols = -c(1), names_to = "metodología", values_to = "tasa_pobreza") %>% 
    rename(anio = `...1`) %>% 
  mutate(
    year = substr(anio, 1, 4),
    periodo = stringr::str_extract(anio, "(?<=-).*"),
    fecha = case_when(
      periodo == "I"  ~ paste0(year, "-06-01"),
      periodo == "II" ~ paste0(year, "-12-01"),
      stringr::str_detect(periodo, "^\\d+$") ~ 
        paste0(year, "-", 
               stringr::str_pad(periodo, 2, side = "left", pad = "0"),
               "-01"),
      TRUE ~ NA_character_
    ),
    fecha = as.Date(fecha)
  ) %>%
  drop_na(tasa_pobreza)

df_output <- df_stage %>%
  select(fecha, metodología, tasa_pobreza)

df_plot <- df_stage %>%
    mutate(label_date = case_when(
      periodo == "I" ~ paste0("ENE-JUN ", year),
      periodo == "II" ~ paste0("JUL-DIC ", year),
      TRUE ~ lubridate::month(fecha, label = TRUE, abbr = TRUE, locale = "es_ES.UTF-8") %>% 
                toupper(.) %>%
                paste0(., " ", year)),
      label_complete = paste0(label_date, "\n", round(tasa_pobreza, 1), "%")
                )


fechas_etiquetar <- c(
  as.Date("1993-10-01"), 
  as.Date("2002-10-01"), 
  as.Date("2017-12-01"),
  as.Date("2025-06-01")
  )

df_labels <- df_plot |>
  dplyr::filter(fecha %in% fechas_etiquetar) 

p <- ggplot(
  df_plot,
  aes(x = fecha, y = tasa_pobreza, color = metodología)
) +
  geom_line(linewidth = 0.8) +
  geom_point(data = df_labels, aes(x = fecha, y = tasa_pobreza), color = "#0072ad", size = 2) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +

  geom_text(
    data = df_labels,
    aes(label = label_complete),
    hjust = 0.8,
    vjust = -0.6,
    size = 2.5,
    color = "#0072ad",
    fontface = "bold"
  ) +

  labs(
    title = "Porcentaje de personas pobres por ingresos en Argentina\n (metodología tradicional y actual, serie empalmada), 1974-2025",
    x = "",
    y = "",
    caption = "Fuente de datos: CEDLAS, INDEC y Logares (2008)"
  ) +

  guides(color = guide_legend(ncol = 2)) +

  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.box = "horizontal",

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
  ylim(0, 70)


output_name <- "outputs/pobreza_largo_plazo.csv"

df_output %>% 
  readr::write_csv(., file = output_name)

# Exporta el gráfico como SVG en la carpeta 'graficos' 
graficos_path <- "graficos"
filename <- paste0(gsub("\\.csv", "", basename(output_name)), ".svg")
name_file <- file.path(graficos_path, filename)
ggplot2::ggsave(filename = name_file, plot = p, device = "svg", width = 8, height = 5)
