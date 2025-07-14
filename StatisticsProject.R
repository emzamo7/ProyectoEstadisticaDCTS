library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
# Instalar el paquete si no lo tienes
install.packages("readxl")

# Cargar el paquete
library(readxl)# Leer el archivo .xlsx
#ruta_archivo <- "ruta/del/archivo.xlsx"
# datos <- read_excel(ruta_archivo)





# Leer el archivo CSV
df <- read_excel("C:/Users/DIEM estudiantes/Downloads/2020.xlsx")
head(df)
df$Entidad <- df$Entidad %>%
  str_trim() %>%                # Elimina espacios extra
  str_to_upper() %>%            # Convierte a mayúsculas para uniformidad
  str_replace_all("COAHUILA DE ZARAGOZA", "COAHUILA")
df$Entidad <- df$Entidad %>%
  str_trim() %>%                # Elimina espacios extra
  str_to_upper() %>%            # Convierte a mayúsculas para uniformidad
  str_replace_all("VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ")
df$Entidad <- df$Entidad %>%
  str_trim() %>%                # Elimina espacios extra
  str_to_upper() %>%            # Convierte a mayúsculas para uniformidad
  str_replace_all("MICHOACÁN DE OCAMPO", "MICHOACÁN")


# Sumar delitos por mes
months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
df$Total_Crimes <- rowSums(df[, months], na.rm = TRUE)

# Suposición de población
population <- 100000

# Total de delitos por municipio
crime_totals <- df %>%
  group_by(Entidad) %>%
  summarise(Total_Crimes = sum(Total_Crimes, na.rm = TRUE))

# Unir totales al dataframe original
df <- df %>%
  left_join(crime_totals, by = "Entidad", suffix = c("", "_Total"))

# Delitos por tipo y tasa por cada 100k habitantes
crime_type_totals <- df %>%
  group_by(Entidad, `Tipo de delito`) %>%
  summarise(Crimes_by_type = sum(Total_Crimes, na.rm = TRUE)) %>%
  left_join(crime_totals, by = "Entidad") %>%
  mutate(Rate_per_100k = Crimes_by_type / population * 100000)

# Top 3 delitos por municipio
top_crimes <- crime_type_totals %>%
  group_by(Entidad) %>%
  slice_max(Rate_per_100k, n = 3) %>%
  ungroup()

# Gráfico general
ggplot(top_crimes, aes(x = Rate_per_100k, y = `Tipo de delito`, fill = Entidad)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Top 3 Crimes per 100,000 inhabitants by Municipality",
       x = "Crimes per 100,000 inhabitants",
       y = "Type of Crime") +
  theme_minimal() +
  theme(legend.position = "right")

# Gráfico por municipio
ggplot(top_crimes, aes(x = Rate_per_100k, y = reorder(`Tipo de delito`, Rate_per_100k), fill = Entidad)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Entidad, scales = "free_y") +
  labs(title = "Top 3 delitos por cada 100,000 habitantes en cada municipio",
       x = "Tasa por cada 100,000 habitantes",
       y = "Tipo de delito") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# Leer shapefile (asegúrate de usar el archivo .shp directamente)
mapa <- st_read("C:/Users/DIEM estudiantes/Downloads/mx.json")

library(stringr)

# Normaliza nombres en ambos dataframes
crime_totals$Entidad <- crime_totals$Entidad %>%
  str_trim() %>%
  str_to_upper()

mapa$Entidad <- mapa$name %>%
  str_trim() %>%
  str_to_upper()

# Unir datos de crimen al mapa
mapa <- mapa %>%
  left_join(crime_totals, by = "Entidad") %>%
  mutate(Rate_per_100k = Total_Crimes / population * 100000)

# Mapa choropleth
ggplot(mapa) +
  geom_sf(aes(fill = Rate_per_100k), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Delitos por 100k") +
  labs(title = "Tasa de delitos por municipio en México") +
  theme_minimal()

library(dplyr)

# Encuentra el delito más frecuente por estado
crimen_mas_comun <- crime_type_totals %>%
  group_by(Entidad, `Tipo de delito`) %>%
  summarise(Casos = sum(Crimes_by_type, na.rm = TRUE)) %>%
  slice_max(Casos, n = 1, with_ties = FALSE) %>%
  ungroup()

# Normaliza nombres si no lo hiciste antes
crimen_mas_comun$Entidad <- crimen_mas_comun$Entidad %>%
  str_trim() %>%
  str_to_upper()

mapa$Entidad <- mapa$name %>%
  str_trim() %>%
  str_to_upper()

# Unir al shapefile
mapa_comun <- mapa %>%
  left_join(crimen_mas_comun, by = "Entidad")

ggplot(mapa_comun) +
  geom_sf(aes(fill = `Tipo de delito`), color = "white") +
  labs(title = "Delito más común por estado en México",
       fill = "Tipo de delito") +
  theme_minimal()

install.packages("DT")
library(DT)

datatable(crimen_mas_comun)

crimen_mas_comun_tasa <- crime_type_totals %>%
  group_by(Entidad, `Tipo de delito`) %>%
  summarise(Tasa = sum(Rate_per_100k, na.rm = TRUE)) %>%
  slice_max(Tasa, n = 1, with_ties = FALSE) %>%
  ungroup()

crimen_mas_comun_tasa$Entidad <- crimen_mas_comun_tasa$Entidad %>%
  str_trim() %>%
  str_to_upper()

mapa$Entidad <- mapa$name %>%
  str_trim() %>%
  str_to_upper()

mapa_comun_tasa <- mapa %>%
  left_join(crimen_mas_comun_tasa, by = "Entidad")

ggplot(mapa_comun_tasa) +
  geom_sf(aes(fill = `Tipo de delito`), color = "white") +
  labs(title = "Delito más común por cada 100,000 habitantes en cada estado",
       fill = "Tipo de delito") +
  theme_minimal()
