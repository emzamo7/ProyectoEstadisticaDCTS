# Region Noroeste
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(tibble)
library(leaflet)
library(tidyverse)
# Load the Mexican states GeoJSON file
mx_states <- st_read("D:/mx.json")

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
  geom_sf(fill = "steelblue", color = "white", size = 0.3) +
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


df <- read.csv("D:/StatisticsPRoject/IDM_NM_jun25.csv", fileEncoding = "latin1")
df$Entidad <- df$Entidad %>%
  str_trim() %>%                
  str_to_upper() %>%            
  str_replace_all("COAHUILA DE ZARAGOZA", "COAHUILA")

df$Entidad <- df$Entidad %>%
  str_trim() %>%                
  str_to_upper() %>%           
  str_replace_all("VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ")
df$Entidad <- df$Entidad %>%
  str_trim() %>%                
  str_to_upper() %>%            
  str_replace_all("MICHOACÁN DE OCAMPO", "MICHOACÁN")

str(df)

df2 <- df[c("Año", "Entidad", "Tipo.de.delito", "Subtipo.de.delito","Modalidad" , "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio","Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")]
str(df2)
years_to_include <- 2015:2024  # creates a vector of years from 2015 to 2024


################################################################################
############################### BAJA CALIFORNIA 2015-2024 ######################
################################################################################
filtered_df_BC <- df2[df2$Año %in% years_to_include & df2$Entidad == "BAJA CALIFORNIA", ]
filtered_df_BC
View(filtered_df_BC)

# Create a vector of the month columns
months_BC <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Sum all month values per year
crime_totals_BC <- aggregate(. ~ Año, data = filtered_df_BC[, c("Año", months_BC)], sum)
crime_totals_BC
# Optional: Add a "Total" column summing across all months
#rime_totals_BC$Total_BC <- rowSums(crime_totals_BC[, months])


#crime_totals_BC$Total_BC #2015 - 2014

# Define the month columns
months_BC <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
years_to_include <- 2015:2024
filtered_df_BC <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "BAJA CALIFORNIA")
# Group by crime type and sum each month
crime_monthly_by_year_type_BC <- filtered_df_BC %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_BC), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_BC)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_BC <- crime_monthly_by_year_type_BC %>%
  mutate(Total_BC = rowSums(across(all_of(months_BC))))

# Group by crime type and sum all years to get overall totals
top_crimes_BC <- crime_monthly_by_year_type_BC %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_BC = sum(Total_BC)) %>%
  arrange(desc(Total_BC)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_BC)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_BC <- crime_monthly_by_year_type_BC %>%
  mutate(Total_BC = rowSums(across(all_of(months_BC))))

# Find top 3 crimes per year
top_crimes_per_year_BC <- crime_monthly_by_year_type_BC %>%
  group_by(Año) %>%
  slice_max(order_by = Total_BC, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_BC))
write.csv(top_crimes_per_year_BC, "top_crimes_per_year_BC.csv", row.names = FALSE)
# View the result
print(top_crimes_per_year_BC, n = 30)
View(top_crimes_per_year_BC)
#unique(df$Entidad)
# Filter for 'Violencia Familiar' only
violencia_familiar_BC <- crime_monthly_by_year_type_BC %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_BC) %>%
  arrange(Año)

# View the result
print(violencia_familiar_BC)

bc_vfBC <- tibble::tibble(
  Año = 2015:2024,
  Total_BC = c(8892, 8315, 8554, 9904, 10455, 10781, 12568, 13262, 14626, 14729)
)

# Población 2020 según tus datos
Poblacion_BC_2024 <- 3193663 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenBC <- bc_vfBC %>%
  summarise(
    anios = n(),
    suma = sum(Total_BC),
    media = mean(Total_BC),
    mediana = median(Total_BC),
    minimo = min(Total_BC),
    maximo = max(Total_BC),
    desviacion = sd(Total_BC)
  )

print(resumenBC)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenBC$media / Poblacion_BC_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en BC:", round(tasa_100k, 2), "\n")




############################### BAJA CALIFORNIA SUR 2015-2024 ######################
months_BCS <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_BCS <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "BAJA CALIFORNIA SUR")
# Group by crime type and sum each month
crime_monthly_by_year_type_BCS <- filtered_df_BCS %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_BCS), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_BCS)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_BCS <- crime_monthly_by_year_type_BCS %>%
  mutate(Total_BCS = rowSums(across(all_of(months_BCS))))

# Group by crime type and sum all years to get overall totals
top_crimes_BCS <- crime_monthly_by_year_type_BCS %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_BCS = sum(Total_BCS)) %>%
  arrange(desc(Total_BCS)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_BCS)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_BCS <- crime_monthly_by_year_type_BCS %>%
  mutate(Total_BCS = rowSums(across(all_of(months_BCS))))

# Find top 3 crimes per year
top_crimes_per_year_BCS <- crime_monthly_by_year_type_BCS %>%
  group_by(Año) %>%
  slice_max(order_by = Total_BCS, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_BCS))

# View the result
print(top_crimes_per_year_BCS, n = 30)
View(top_crimes_per_year_BCS)
# Filter for 'Violencia Familiar' only
violencia_familiar_BCS <- crime_monthly_by_year_type_BCS %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_BCS) %>%
  arrange(Año)

# View the result
print(violencia_familiar_BCS)
bc_vfBCS <- tibble::tibble(
  Año = 2015:2024,
  Total_BCS = c(1691, 2099, 1982, 2110, 2612, 2490, 2493, 2620, 2946, 3344)
)

# Población 2020 según tus datos
Poblacion_BCS_2024 <- 670296 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenBCS <- bc_vfBCS %>%
  summarise(
    anios = n(),
    suma = sum(Total_BCS),
    media = mean(Total_BCS),
    mediana = median(Total_BCS),
    minimo = min(Total_BCS),
    maximo = max(Total_BCS),
    desviacion = sd(Total_BCS)
  )

print(resumenBCS)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenBCS$media / Poblacion_BCS_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en BC:", round(tasa_100k, 2), "\n")

############################### NAYARIT 2015-2024 ######################
months_Nayarit <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Nayarit <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "NAYARIT")
# Group by crime type and sum each month
crime_monthly_by_year_type_Nayarit <- filtered_df_Nayarit %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Nayarit), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Nayarit)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Nayarit <- crime_monthly_by_year_type_Nayarit %>%
  mutate(Total_Nayarit = rowSums(across(all_of(months_Nayarit))))

# Group by crime type and sum all years to get overall totals
top_crimes_Nayarit <- crime_monthly_by_year_type_Nayarit %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Nayarit = sum(Total_Nayarit)) %>%
  arrange(desc(Total_Nayarit)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Nayarit)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Nayarit <- crime_monthly_by_year_type_Nayarit %>%
  mutate(Total_Nayarit = rowSums(across(all_of(months_Nayarit))))

# Find top 3 crimes per year
top_crimes_per_year_Nayarit <- crime_monthly_by_year_type_Nayarit %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Nayarit, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Nayarit))

# View the result
print(top_crimes_per_year_Nayarit, n = 30)
View(top_crimes_per_year_Nayarit)

# Filter for 'Violencia Familiar' only
violencia_familiar_Nay <- crime_monthly_by_year_type_Nayarit %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Nayarit) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Nay)

bc_vfNay <- tibble::tibble(
  Año = 2015:2024,
  Total_Nay = c(298, 476, 432, 404, 768, 864, 1063, 1947, 2310, 2513)
)

# Población 2020 según tus datos
Poblacion_Nay_2024 <- 1599104 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenNay <- bc_vfNay %>%
  summarise(
    anios = n(),
    suma = sum(Total_Nay),
    media = mean(Total_Nay),
    mediana = median(Total_Nay),
    minimo = min(Total_Nay),
    maximo = max(Total_Nay),
    desviacion = sd(Total_Nay)
  )

print(resumenNay)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenNay$media / Poblacion_Nay_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Nayarit:", round(tasa_100k, 2), "\n")



############################### SINALOA 2015-2024 ######################
months_Sinaloa <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Sinaloa <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "SINALOA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Sinaloa <- filtered_df_Sinaloa %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Sinaloa), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Sinaloa)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Sinaloa <- crime_monthly_by_year_type_Sinaloa %>%
  mutate(Total_Sinaloa = rowSums(across(all_of(months_Sinaloa))))

# Group by crime type and sum all years to get overall totals
top_crimes_Sinaloa <- crime_monthly_by_year_type_Sinaloa %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Sinaloa= sum(Total_Sinaloa)) %>%
  arrange(desc(Total_Sinaloa)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Sinaloa)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Sinaloa <- crime_monthly_by_year_type_Sinaloa %>%
  mutate(Total_Sinaloa = rowSums(across(all_of(months_Sinaloa))))

# Find top 3 crimes per year
top_crimes_per_year_Sinaloa <- crime_monthly_by_year_type_Sinaloa %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Sinaloa, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Sinaloa))

# View the result
print(top_crimes_per_year_Sinaloa, n = 30)
View(top_crimes_per_year_Sinaloa)
violencia_familiar_Sin <- crime_monthly_by_year_type_Sinaloa %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Sinaloa) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Sin)

bc_vfSin <- tibble::tibble(
  Año = 2015:2024,
  Total_Sin = c(2387,2371,2612,3800,4460,5138,5733,6746,7601,7336)
)

# Población 2020 según tus datos
Poblacion_Sin_2024 <- 2412311 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenSin <- bc_vfSin %>%
  summarise(
    anios = n(),
    suma = sum(Total_Sin),
    media = mean(Total_Sin),
    mediana = median(Total_Sin),
    minimo = min(Total_Sin),
    maximo = max(Total_Sin),
    desviacion = sd(Total_Sin)
  )

print(resumenSin)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenSin$media / Poblacion_Sin_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Sinaloa:", round(tasa_100k, 2), "\n")


############################### SONORA 2015-2024 ######################
months_Sonora <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Sonora <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "SONORA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Sonora <- filtered_df_Sonora %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Sonora), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Sonora)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Sonora <- crime_monthly_by_year_type_Sonora %>%
  mutate(Total_Sonora = rowSums(across(all_of(months_Sonora))))

# Group by crime type and sum all years to get overall totals
top_crimes_Sonora <- crime_monthly_by_year_type_Sonora %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Sonora = sum(Total_Sonora)) %>%
  arrange(desc(Total_Sonora)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Sonora)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Sonora <- crime_monthly_by_year_type_Sonora %>%
  mutate(Total_Sonora = rowSums(across(all_of(months_Sonora))))

# Find top 3 crimes per year
top_crimes_per_year_Sonora <- crime_monthly_by_year_type_Sonora %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Sonora, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Sonora))

# View the result
print(top_crimes_per_year_Sonora, n = 30)
violencia_familiar_Son <- crime_monthly_by_year_type_Sonora %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Sonora) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Son)


View(top_crimes_per_year_Sonora)
bc_vfSon <- tibble::tibble(
  Año = 2015:2024,
  Total_Son = c(2231, 3595, 2428, 2195, 3587, 5450, 7225, 6345, 8101, 8366)
)

# Población 2020 según tus datos
Poblacion_Son_2024 <- 2366747 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenSon <- bc_vfSon %>%
  summarise(
    anios = n(),
    suma = sum(Total_Son),
    media = mean(Total_Son),
    mediana = median(Total_Son),
    minimo = min(Total_Son),
    maximo = max(Total_Son),
    desviacion = sd(Total_Son)
  )

print(resumenSon)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenSon$media / Poblacion_Son_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Sonora:", round(tasa_100k, 2), "\n")

##########################################################################################################################################
####################################################### NOROESTE: BC, BCS, SONORA, SINALOA y NAYARIT #####################################
##########################################################################################################################################


############################### BAJA CALIFORNIA 2015-2024 ######################




############################### BAJA CALIFORNIA SUR 2015-2024 ######################
desc_stats_BCS <- crime_monthly_by_year_type_BCS %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_BCS, na.rm = TRUE),  # Total across all years
    media = mean(Total_BCS, na.rm = TRUE),  # Average per year
    mediana = median(Total_BCS, na.rm = TRUE),
    minimo = min(Total_BCS, na.rm = TRUE),
    maximo = max(Total_BCS, na.rm = TRUE),
    desviacion = sd(Total_BCS, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_BCS)

#####¿Cómo interpretar estos resultados?
#  Robo
# Delito más frecuente, con un promedio anual de 7,782 casos.

#Alta variabilidad (desviación: 2,516), que sugiere que ha habido años especialmente problemáticos (pico de 11,365),
# mientras que otros años han sido mucho más bajos.

# Violencia familiar
# El segundo delito en volumen, con poco más de 2,400 casos anuales y una desviación baja,
# lo que señala relativa estabilidad en el número de casos por año.

# Lesiones, Daño a la propiedad, Amenazas
# Todos superan los 1,400 casos anuales en promedio, pero muestran dispersiones menores que el robo, 
# lo que indica que sus cifras son más consistentes año con año.

# Narcomenudeo y Fraude
# Presentan cifras anuales moderadas pero cierta variabilidad, probablemente ligadas a acciones legales específicas o 
# cambios en la dinámica criminal.

# Despojo
# Es el menos reportado del top 10, con un promedio anual de 358 casos y muy poca variabilidad (desviación: 41.2), 
# lo que indica estabilidad.

# Claves para tu análisis:
#  Si la media y la mediana son similares, el comportamiento es estable; 
# si difieren mucho, algunos años atípicos o cambios de tendencia recientes pueden estar afectando la estadística.

# Una desviación estándar alta señala que el fenómeno cambia mucho de un año a otro; 
# requiere atención para identificar factores coyunturales o estructurales que expliquen los cambios.

# Un máximo muy lejos de la media o la mediana puede señalar un año atípico que vale la pena investigar con más detalle.

# Create the tibble with the second set of data
# Note: Truncated names from the input have been completed for clarity.
# Create the tibble with the second set of data
crime_data_2 <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                         10, 77823, 7782, 7174, 5068, 11365, 2516,
  "Violencia familiar",           10, 24387, 2439, 2492, 1691, 3344, 487,
  "Lesiones",                     10, 19715, 1972, 1944, 1565, 2492, 252,
  "Daño a la propiedad",          10, 17220, 1722, 1758, 1215, 2080, 268,
  "Amenazas",                     10, 14267, 1427, 1336, 953, 2250, 396,
  "Otros delitos del Fuero Común",10, 11105, 1110, 976,  562, 2877, 648,
  "Fraude",                       10, 10999, 1100, 1018, 646, 1716, 361,
  "Incumplimiento de obligaciones",10, 7781, 778,  790,  526,  958, 141,
  "Narcomenudeo",                 10,  5982,  598,  562,  435, 1054, 187,
  "Despojo",                      10,  3585,  358,  360,  262,  413, 41.2
)

# Create the bar chart with text labels on top
ggplot(crime_data_2, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # This new line adds the text labels above the bars
  geom_text(aes(label = media), vjust = -0.3, size = 3) +
  labs(
    title = "Promedio anual de conteo de delitos en BCS",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################### SONORA 2015-2024 ######################
desc_stats_Sonora <- crime_monthly_by_year_type_Sonora %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Sonora, na.rm = TRUE),  # Total across all years
    media = mean(Total_Sonora, na.rm = TRUE),  # Average per year
    mediana = median(Total_Sonora, na.rm = TRUE),
    minimo = min(Total_Sonora, na.rm = TRUE),
    maximo = max(Total_Sonora, na.rm = TRUE),
    desviacion = sd(Total_Sonora, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_Sonora)

# Claves de interpretación
# Robo
# Es el delito más frecuente, con un promedio anual de 9,197 casos y un rango muy amplio (de 6,397 a 16,021).

# La desviación estándar elevada (2,705) indica que el número de robos varía considerablemente de un año a otro, 
# probablemente ligado a cambios coyunturales o políticas de seguridad.

# Violencia familiar
# Segundo en volumen, con más de 4,900 casos anuales en promedio.

# Gran variabilidad (desviación de 2,450), lo que podría reflejar un aumento reciente significativo o la influencia de factores sociales, culturales o de denuncias.

# # Lesiones y Narcomenudeo
# Ambos con más de 2,000 casos anuales en promedio.

# Narcomenudeo presenta una mediana mayor a la media, lo que sugiere una disminución reciente en el delito o 
# # años excepcionales con cifras bajas.

# Lesiones es más estable pero aún muestra fluctuaciones considerables entre años.

# Delitos patrimoniales y familiares
# Daño a la propiedad y incumplimiento de obligaciones de asistencia familiar mantienen medias superiores
# a 2,000 casos anuales.

# Ambos presentan desviaciones estándar moderadas—indican cierta regularidad, aunque con tendencia a cambiar en algunos años.

# Otros delitos relevantes
# Homicidio alcanza promedios de más de 1,400 casos anuales y una desviación baja en comparación con otros delitos graves,
# lo que señala un comportamiento relativamente constante.

# Amenazas y fraude poseen cifras inferiores, pero siguen siendo relevantes para el análisis y prevención.
# Create the tibble with the data for Sonora
# Note: The truncated crime type name has been completed for clarity.
crime_data_sonora <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                      10, 91969, 9197, 8538, 6397, 16021, 2705,
  "Violencia familiar",                        10, 49523, 4952, 4522, 2195, 8366,  2450,
  "Lesiones",                                  10, 24572, 2457, 2418, 1310, 3405,  644,
  "Narcomenudeo",                              10, 22532, 2253, 2464, 698,  3999,  999,
  "Daño a la propiedad",                       10, 21149, 2115, 2286, 1215, 2780,  551,
  "Incumplimiento de obligaciones",            10, 20905, 2090, 2089, 1327, 3013,  570,
  "Otros delitos del Fuero Común",             10, 17802, 1780, 2004, 460,  2627,  737,
  "Homicidio",                                 10, 14020, 1402, 1404, 983,  1968,  325,
  "Amenazas",                                  10, 10602, 1060, 1134, 247,  1817,  654,
  "Fraude",                                    10,  6423,  642,  636, 360,   863,  150
)

# Create the bar chart with text labels
ggplot(crime_data_sonora, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "coral") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Sonora",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
############################### SINALOA 2015-2024 ######################
desc_stats_Sinaloa <- crime_monthly_by_year_type_Sinaloa %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Sinaloa, na.rm = TRUE),  # Total across all years
    media = mean(Total_Sinaloa, na.rm = TRUE),  # Average per year
    mediana = median(Total_Sinaloa, na.rm = TRUE),
    minimo = min(Total_Sinaloa, na.rm = TRUE),
    maximo = max(Total_Sinaloa, na.rm = TRUE),
    desviacion = sd(Total_Sinaloa, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_Sinaloa)

# Robo
# Más común, con cerca de 8,456 casos anuales y 84,559 en la década.

# La variabilidad anual no es tan alta para el nivel absoluto: fue más bajo (6,660) y más alto (10,115).

# La mediana muy parecida a la media sugiere comportamiento estable, aunque con algo de incremento o descenso en ciertos 
# años.

# Violencia familiar
# Segundo más frecuente.

# Desviación estándar alta (2,019): mucho crecimiento en algunos años, probablemente tendencia en aumento o algunos picos concretos.

# Mínimo (2,371) y máximo (7,601) muestran que puede haber habido cambios relevantes (como mejores registros o cambios sociales) en algunos años.

# Lesiones, Daño a la propiedad y Homicidio
# Todos tienen medias y medianas bastante similares (comportamiento estable).

# Lesiones: alrededor de 3,200 al año, desviación moderada.

# Daño a la propiedad y Homicidio: promedios menores, poca fluctuación.

# Amenazas y Narcomenudeo
# Amenazas tiene mediana muy cerca de la media pero con un máximo alto (2,007) y un mínimo bajo (307): posibles picos específicos a investigar.

# Narcomenudeo: promedio bajo pero desviación considerable, lo que indica años con fenómenos específicos o picos aislados.

# Otros delitos
# Otros delitos contra la libertad personal y contra vida e integridad muestran cifras bajas y bastante estables.

# Conclusiones rápidas
# Sinaloa tiene como delito principal el robo, seguido de violencia familiar (que muestra tendencia a aumentar o variabilidad considerable).

# Los delitos contra la integridad física (lesiones, homicidio) y propiedad han sido relativamente estables.

# Delitos como narcomenudeo y amenazas pueden tener años atípicos o estar relacionados con fenómenos coyunturales.
# Create the tibble with the data for Sinaloa
# Note: The truncated crime type names have been completed for clarity.
crime_data_sinaloa <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                      10, 84559, 8456, 8618, 6660, 10115, 1146,
  "Violencia familiar",                        10, 48184, 4818, 4799, 2371, 7601,  2019,
  "Lesiones",                                  10, 32081, 3208, 3100, 2132, 4360,  862,
  "Daño a la propiedad",                       10, 20635, 2064, 2046, 1287, 2777,  574,
  "Homicidio",                                 10, 13990, 1399, 1406, 1106, 1839,  220,
  "Otros delitos contra la libertad",          10, 11334, 1133, 1168, 560,  1599,  310,
  "Amenazas",                                  10, 11027, 1103, 1076, 307,  2007,  624,
  "Fraude",                                    10,  6369,  637,  592, 424,  1017,  205,
  "Otros delitos contra la vida",              10,  5500,  550,  558, 401,   681,  107,
  "Narcomenudeo",                              10,  4624,  462,  328, 145,   939,  309
)

# Create the bar chart with text labels
ggplot(crime_data_sinaloa, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkseagreen") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Sinaloa",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
############################### NAYARIT 2015-2024 ######################
# 1. Descriptive statistics *per crime type* across all years
desc_stats_nayarit <- crime_monthly_by_year_type_Nayarit %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Nayarit, na.rm = TRUE),  # Total across all years
    media = mean(Total_Nayarit, na.rm = TRUE),  # Average per year
    mediana = median(Total_Nayarit, na.rm = TRUE),
    minimo = min(Total_Nayarit, na.rm = TRUE),
    maximo = max(Total_Nayarit, na.rm = TRUE),
    desviacion = sd(Total_Nayarit, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_nayarit)
# Esto sugiere que "Violencia familiar" ha crecido mucho en años recientes, lo que explica una media y desviación alta.

#Robo
#media de 1,077, mediana de 1,011, desviación de 383:

#  Hay cierta variabilidad, pero menos que en violencia familiar.

#La diferencia entre media y mediana no es tan grande, por lo tanto no hay años tan extremos.

#El número de robos ha sido relativamente estable, aunque sí hay fluctuaciones.

#Lesiones
#media de 510, mediana de 285, máximo de 1,243, desviación de 397:

#  Aunque en promedio parecen muchos casos (510), la mediana es baja (285), lo que indica que hubo algunos años con picos muy altos (hasta 1,243), jalando la media hacia arriba.

#La desviación es alta, lo que confirma mucha variabilidad.

# Create the tibble with the data for Nayarit
# Note: The truncated crime type name has been completed for clarity.
crime_data_nayarit <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Violencia familiar",                        10, 11075, 1108,  816,  298, 2513, 837,
  "Robo",                                      10, 10766, 1077, 1011,  584, 1713, 383,
  "Otros delitos del Fuero Común",             10,  8517,  852,  836,  290, 1331, 352,
  "Incumplimiento de obligaciones",            10,  5972,  597,  552,  300,  951, 234,
  "Lesiones",                                  10,  5105,  510,  285,  159, 1243, 397,
  "Narcomenudeo",                              10,  3810,  381,  334,  129,  750, 206,
  "Homicidio",                                 10,  3023,  302,  330,   84,  446, 112,
  "Amenazas",                                  10,  3003,  300,  140,   53,  958, 323,
  "Daño a la propiedad",                       10,  2972,  297,  144,   77,  839, 293,
  "Fraude",                                    10,  2933,  293,  228,   97,  617, 185
)

# Create the bar chart with text labels
ggplot(crime_data_nayarit, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "mediumpurple") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Nayarit",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Define years and states of interest
years_to_include <- 2015:2024
states_of_interest <- c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "SINALOA", "NAYARIT")

# Filter data: Only records for selected states, years, and crime
filtered_data <- df2 %>%
  filter(Año %in% years_to_include,
         Entidad %in% states_of_interest,
         Tipo.de.delito == "Violencia familiar")

# Sum over all months for each year and state
filtered_summary <- filtered_data %>%
  mutate(Total = Enero + Febrero + Marzo + Abril + Mayo + Junio + 
           Julio + Agosto + Septiembre + Octubre + Noviembre + Diciembre) %>%
  group_by(Año, Entidad) %>%
  summarise(Annual_Count = sum(Total, na.rm = TRUE), .groups = "drop")

# Plot the annual trend as a line graph
ggplot(filtered_summary, aes(x = Año, y = Annual_Count, color = Entidad)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 2015:2024) +
  geom_point(size = 2) +
  labs(title = "Evolución anual de Violencia Familiar (2015-2024)",
       x = "Año",
       y = "Casos anuales",
       color = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

aov_result <- aov(Total ~ Estado, data = df_long)
summary(aov_result)

##############################################################################3
#####################################ANOVA###################################
##############################################################################
# Datos ejemplo ajustados a tu caso
df_long <- tibble::tibble(
  Año = rep(2015:2024, 5),
  Estado = rep(c("Baja California","Baja California Sur","Nayarit","Sinaloa","Sonora"), each = 10),
  Total = c(
    8892, 8315, 8554, 9904, 10455, 10781, 12568, 13262, 14626, 14729,        # BC
    1691, 2099, 1982, 2110, 2612, 2490, 2493, 2620, 2946, 3344,              # BCS
    298, 476, 432, 404, 768, 864, 1063, 1947, 2310, 2513,                    # Nayarit
    2387,2371,2612,3800,4460,5138,5733,6746,7601,7336,                        # Sinaloa
    2231,3595,2428,2195,3587,5450,7225,6345,8101,8366                         # Sonora
  )
)
# ANOVA de 1 factor
anova_result <- aov(Total ~ Estado, data = df_long)
summary(anova_result)
# TUKEY
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Visual (opcional)
plot(tukey_result)

###################################IC#####################

# BC:
media <- 11209      # Media anual de BC
sd <- 2435          # Desviación estándar anual de BC
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para BC: [%.1f, %.1f]\n", lower, upper))


# # BCS:
media <- 2439      # Media anual de BCS
sd <- 487          # Desviación estándar anual de BCS
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para BCS: [%.1f, %.1f]\n", lower, upper))

# # Nayarit:
media <- 1108      # Media anual de BCS
sd <- 837          # Desviación estándar anual de BCS
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Nayarit: [%.1f, %.1f]\n", lower, upper))
# # Sinaloa:
media <- 4818      # Media anual de Sin
sd <- 2019          # Desviación estándar anual de Sin
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Sinaloa: [%.1f, %.1f]\n", lower, upper))

# # Sonora
media <- 4952      # Media anual de Son
sd <- 2450          # Desviación estándar anual de Son
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Sonora: [%.1f, %.1f]\n", lower, upper))
