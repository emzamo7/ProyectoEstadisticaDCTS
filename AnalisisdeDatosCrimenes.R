# Preprocesamiento de datos
install.packages("tidyverse") # En este caso, voy a usar la biblioteca tidyverse
library(tidyverse)

# Cargando el dataset

data <- read.csv("C:/Users/emman/Downloads/archive/mexico_crime.csv")

# Explorando
dim(data) # dim nos sirve para conocer las dimensiones del dataset: 332416 filas, 9 columnas
head(data)

# Las columnas son: year, entity_code, entity, affected_legal_good, type_of_crime,
# subtype_of_crim, modality, momth y count.


summary(data)

library(lubridate)  
library(janitor)  
library(scales)  
library(patchwork)
library(readr)

crime <- read_csv("C:/Users/emman/Downloads/archive/mexico_crime.csv") %>%  
  clean_names()  #Limpiar y estandarizar la columnas (nombres)
glimpse(crime)  # Un vistazo a nuestro dataset

year_tot <- crime %>%  
  group_by(year) %>%  
  summarise(total = sum(count, na.rm = TRUE))  # Aqui estoy usando este operador. Lo estoy probando, es para encadenar 
# funciones pasandose el resultado de una funcion a la siguiente como argumento principal.


# Graficamos
ggplot(year_tot, aes(year, total)) +  
  geom_col(fill = "steelblue") +  
  scale_y_continuous(labels = comma) +  
  labs(title = "Total reported crimes, nationwide",  
       x = NULL, y = "Incidents") +  
  theme_minimal()  


state_tot <- crime %>%  
  group_by(entity) %>%  
  summarise(total = sum(count, na.rm = TRUE)) %>%  
  arrange(desc(total))  

top10_states <- state_tot %>% slice_head(n = 10)

ggplot(top10_states,  
       aes(reorder(entity, total), total)) +  
  geom_col(fill = "firebrick") +  
  coord_flip() +  
  scale_y_continuous(labels = comma) +  
  labs(title = "Top-10 states by total crime counts (2015--)",  
       x = NULL, y = "Incidents") +  
  theme_minimal()  

type_tot <- crime %>%  
  group_by(type_of_crime) %>%  
  summarise(total = sum(count, na.rm = TRUE)) %>%  
  arrange(desc(total))  

ggplot(type_tot,  
       aes(reorder(type_of_crime, total), total)) +  
  geom_col(fill = "darkorchid") +  
  coord_flip() +  
  scale_y_continuous(labels = comma) +  
  labs(title = "Crime counts by major type",  
       x = NULL, y = "Incidents") +  
  theme_minimal()  

#Ahora solo me enfocare a la CDMX
target_state <- "Ciudad de México" 

state_mix <- crime %>%  
  filter(entity == target_state) %>%  
  group_by(type_of_crime) %>%  
  summarise(total = sum(count, na.rm = TRUE)) %>%  
  mutate(pct = total / sum(total)) #  mutate me ayuda a crear una nueva columna llamada pct 
# Aqui estoy usando filter, para seleccionar filas del dataset. entity == target_state es mi condicion logica: solo se conservaran las filas donde el valor de la columna entity sea igual al valor de la variable target_state.
# ya con summarise, veo un nuevo resumen del DF. En sum, calculo la suma total de la columna count, y na.rm = TRUE indico que los valores NA (faltantes) se ignoren al sumar.
ggplot(state_mix,  
       aes(x = "", y = pct, fill = type_of_crime)) +  
  geom_col(width = 1, color = "white") +  
  coord_polar(theta = "y") +  
  scale_y_continuous(labels = percent) +  
  labs(title = paste("Crime-type composition in", target_state),  
       fill = "Crime type") +  
  theme_void()  
#Viendo que onda de manera grafica en un pastel.
#Viendo que rollo ahora de manera nacional
nation_mix <- crime %>%  
  group_by(year, type_of_crime) %>%  
  summarise(total = sum(count, na.rm = TRUE)) %>%  
  group_by(year) %>%  
  mutate(pct = total / sum(total))
#Graficamos de manera de barras y mapa de calor (con los colores) 
ggplot(nation_mix,  
       aes(year, pct, fill = type_of_crime)) +  
  geom_col() +  
  scale_y_continuous(labels = percent) +  
  labs(title = "Changing crime-type composition nationwide",  
       x = NULL, y = "Share of incidents",  
       fill = "Crime type") +  
  theme_minimal()

crime <- crime %>%  
  mutate(month_num = match(month, month.name))  

heat <- crime %>%  
  group_by(year, month_num) %>%  
  summarise(total = sum(count, na.rm = TRUE))  
#Ahora, con un mapa de calor veo el comportamiento por meses , de cada anio, de los incidentes.
ggplot(heat, aes(month_num, year, fill = total)) +  
  geom_tile(color = "grey70") +  
  scale_x_continuous(breaks = 1:12, labels = substr(month.name,1,3)) +  
  scale_fill_viridis_c(option = "plasma", labels = comma) +  
  labs(title = "Seasonality heat-map (all crimes)",  
       x = "Month", y = "Year", fill = "Incidents") +  
  theme_minimal()  
# ya descargado el dataset de INEGI, se limpio.
pop <- read_csv("C:/Users/emman/Downloads/INEGI_cleaned.csv") %>%
  clean_names()
##############################################################  
# 3.  tasa de criminalidad por estado y comparacion 
##############################################################  


# Required columns: de este ultimo csv, nos interesa la entidad y el total de poblacion de cada estado  


head(pop)
pop
str(pop)
# Si solo tienes datos de 2020, agrega la columna de año:
pop <- pop %>%
  mutate(year = 2020) %>%
  select(entity, year, total) 
crime_2020
crime_2020 <- crime %>%
  filter(year == 2020) %>%
  group_by(entity, year) %>%
  summarise(crime_total = sum(count, na.rm = TRUE))

crime_rate_2020 <- crime_2020 %>%
  left_join(pop, by = c("entity", "year")) %>%
  mutate(rate = crime_total / total * 1e5)

# Para ver el resultado
head(crime_rate_2020)
crime_rate_2020

# Ordenamos los estados por tasa de criminalidad (de mayor a menor)
rank_tbl <- crime_rate_2020 %>%
  arrange(desc(rate))

# Grafica la tasa de criminalidad por estado
ggplot(rank_tbl, aes(x = reorder(entity, rate), y = rate)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Tasa de criminalidad por cada 100,000 habitantes (2020)",
    x = NULL,
    y = "Tasa de crímenes"
  ) +
  theme_minimal()

#ESTADISTICA DESCRIPTIVA

summary(crime_rate_2020$rate)
#Medidas de tendencia central y dispersion

# Media
mean_rate <- mean(crime_rate_2020$rate, na.rm = TRUE)

# Mediana
median_rate <- median(crime_rate_2020$rate, na.rm = TRUE)

# Moda (requiere paquete DescTools)
if (!require(DescTools)) install.packages("DescTools")
library(DescTools)
mode_rate <- Mode(crime_rate_2020$rate)

# Desviación estándar
sd_rate <- sd(crime_rate_2020$rate, na.rm = TRUE)

# Varianza
var_rate <- var(crime_rate_2020$rate, na.rm = TRUE)

# Rango
range_rate <- range(crime_rate_2020$rate, na.rm = TRUE)

# Cuartiles
quantiles_rate <- quantile(crime_rate_2020$rate, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
# Asimetría y curtosis (paquete moments)
if (!require(moments)) install.packages("moments")
library(moments)
skew_rate <- skewness(crime_rate_2020$rate, na.rm = TRUE)
kurt_rate <- kurtosis(crime_rate_2020$rate, na.rm = TRUE)

# Puedes crear intervalos de tasas y contar frecuencias
crime_rate_2020 %>%
  mutate(rate_group = cut(rate, breaks = 5)) %>%
  count(rate_group)

# Histograma
ggplot(crime_rate_2020, aes(x = rate)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Histograma de tasas de criminalidad (por 100,000 habitantes)",
       x = "Tasa", y = "Frecuencia") +
  theme_minimal()

# Boxplot
ggplot(crime_rate_2020, aes(y = rate)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot de tasas de criminalidad", y = "Tasa") +
  theme_minimal()

# Estados con tasas atípicamente altas o bajas
Q1 <- quantile(crime_rate_2020$rate, 0.25, na.rm = TRUE)
Q3 <- quantile(crime_rate_2020$rate, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outliers <- crime_rate_2020 %>%
  filter(rate < (Q1 - 1.5 * IQR) | rate > (Q3 + 1.5 * IQR))

resumen <- tibble(
  Media = mean_rate,
  Mediana = median_rate,
  Moda = mode_rate,
  Desviación_Estándar = sd_rate,
  Varianza = var_rate,
  Rango = diff(range_rate),
  Q1 = quantiles_rate[1],
  Q3 = quantiles_rate[3],
  Asimetría = skew_rate,
  Curtosis = kurt_rate
)
print(resumen)

Q1 <- quantile(crime_rate_2020$rate, 0.25, na.rm = TRUE)
Q3 <- quantile(crime_rate_2020$rate, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
IQR


