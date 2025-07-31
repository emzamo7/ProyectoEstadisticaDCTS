library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(tibble)
library(leaflet)
library(tidyverse)
# Load the Mexican states GeoJSON file
mx_states <- st_read("C:/Users/emman/Documents/StatisticsPRoject/mx.json")

# Define economic regions
economic_regions <- tribble(
  ~region,             ~state_name,
  "Noroeste",         c("Baja California", "Sonora", "Sinaloa", "Nayarit", "Baja California Sur"),
  "Norte",             c("Coahuila", "Durango", "Chihuahua", "Zacatecas", "San Luis Potosí"),
  "Noreste",         c("Nuevo León", "Tamaulipas"),
  "Centro-Occidente",              c("Colima","Jalisco", "Aguascalientes", "Michoacán", "Guanajuato"),
  "Centro-Sur",      c("Querétaro", "México", "Hidalgo", "Tlaxcala", "Puebla", "Ciudad de México", "Puebla", "Morelos" ),
  "Golfo de Mexico",            c("Veracruz", "Tabasco"),
  "Pacifico sur",             c("Guerrero", "Oaxaca", "Chiapas"),
  "Peninsula de Yucatán",         c("Campeche", "Yucatán", "Quintana Roo" )
)

# Flatten to a long format for joining
region_df <- economic_regions %>%
  unnest(state_name) %>%
  rename(name = state_name)

# Join economic region labels to GeoJSON data
mx_states_region <- mx_states %>%
  left_join(region_df, by = "name")

ggplot(mx_states_region) +
  geom_sf(aes(fill = region), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Set2", na.value = "gray80") +
  labs(title = "Regiones Economicas de Mexico", fill = "Region") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# 3. Convertir la lista a formato largo para hacer un join más sencillo
region_df <- economic_regions %>%
  unnest(state_name) %>%
  rename(name = state_name)

# 4. Añadir la columna de región al dataframe de estados
mx_states_region <- mx_states %>%
  left_join(region_df, by = "name")

# 5. Filtrar únicamente la región Noroeste
mx_noroeste <- mx_states_region %>%
  filter(region == "Noroeste")

# 6. Plotear el mapa
ggplot(mx_noroeste) +
  geom_sf(fill = "#3ebb00", color = "white", size = 0.3) +
  labs(
    title = "Región Noroeste de México",
    subtitle = "Estados: Baja California, Sonora, Sinaloa, Nayarit, Baja California Sur",
    caption = "Fuente: simplemaps.com"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
################################################################
########################SURESTE#################################
################################################################
# 5. Filtrar únicamente la región Sureste
mx_sureste <- mx_states_region %>%
  filter(region == "Peninsula de Yucatán")

# 6. Plotear el mapa
ggplot(mx_sureste) +
  geom_sf(fill = "#9c9483", color = "white", size = 0.3) +
  labs(
    title = "Región Sureste de México",
    subtitle = "Estados: Yucatán, Campeche, Quintana Roo",
    caption = "Fuente: simplemaps.com"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
################################################################
######################## GOLFO DE MEXICO #################################
################################################################
# 5. Filtrar únicamente la región Golfo de Mexico
mx_Golfo <- mx_states_region %>%
  filter(region == "Golfo de Mexico")

# 6. Plotear el mapa
ggplot(mx_Golfo) +
  geom_sf(fill = "#42a8ff", color = "white", size = 0.3) +
  labs(
    title = "Región Golfo de México",
    subtitle = "Estados: Veracruz, Tabasco",
    caption = "Fuente: simplemaps.com"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

################################################################
######################## NORESTE #################################
################################################################
# 5. Filtrar únicamente la región Golfo de Mexico
mx_NE <- mx_states_region %>%
  filter(region == "Noreste")

# 6. Plotear el mapa
ggplot(mx_NE) +
  geom_sf(fill = "#bc3fde", color = "white", size = 0.3) +
  labs(
    title = "Noreste",
    subtitle = "Estados: Tamaulipas y Nuevo León",
    caption = "Fuente: simplemaps.com"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
