#Region Sur
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(tibble)
library(leaflet)

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

############################### CHIAPAS 2015-2024 ######################
months_Chiapas <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Chiapas <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "CHIAPAS")
# Group by crime type and sum each month
crime_monthly_by_year_type_Chiapas <- filtered_df_Chiapas %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Chiapas), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Chiapas)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Chiapas <- crime_monthly_by_year_type_Chiapas %>%
  mutate(Total_Chiapas = rowSums(across(all_of(months_Chiapas))))

# Group by crime type and sum all years to get overall totals
top_crimes_Chiapas <- crime_monthly_by_year_type_Chiapas %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Chiapas = sum(Total_Chiapas)) %>%
  arrange(desc(Total_Chiapas)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Chiapas)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Chiapas <- crime_monthly_by_year_type_Chiapas %>%
  mutate(Total_Chiapas = rowSums(across(all_of(months_Chiapas))))

# Find top 3 crimes per year
top_crimes_per_year_Chiapas <- crime_monthly_by_year_type_Chiapas %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Chiapas, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Chiapas))

# View the result
print(top_crimes_per_year_Chiapas, n = 30)
View(top_crimes_per_year_Chiapas)





############################### GUERRERO 2015-2024 ######################
months_Guerrero <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                     "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Guerrero <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "GUERRERO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Guerrero <- filtered_df_Guerrero %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Guerrero), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Guerrero)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Guerrero <- crime_monthly_by_year_type_Guerrero %>%
  mutate(Total_Guerrero = rowSums(across(all_of(months_Guerrero))))

# Group by crime type and sum all years to get overall totals
top_crimes_Guerrero <- crime_monthly_by_year_type_Guerrero %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Guerrero = sum(Total_Guerrero)) %>%
  arrange(desc(Total_Guerrero)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Guerrero)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Guerrero <- crime_monthly_by_year_type_Guerrero %>%
  mutate(Total_Guerrero = rowSums(across(all_of(months_Guerrero))))

# Find top 3 crimes per year
top_crimes_per_year_Guerrero <- crime_monthly_by_year_type_Guerrero %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Guerrero, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Guerrero))

# View the result
print(top_crimes_per_year_Guerrero, n = 30)
View(top_crimes_per_year_Guerrero)
############################### OAXACA 2015-2024 ######################
months_Oaxaca <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Oaxaca <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "OAXACA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Oaxaca <- filtered_df_Oaxaca %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Oaxaca), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Oaxaca)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Oaxaca <- crime_monthly_by_year_type_Oaxaca %>%
  mutate(Total_Oaxaca = rowSums(across(all_of(months_Oaxaca))))

# Group by crime type and sum all years to get overall totals
top_crimes_Oaxaca <- crime_monthly_by_year_type_Oaxaca %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Oaxaca = sum(Total_Oaxaca)) %>%
  arrange(desc(Total_Oaxaca)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Oaxaca)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Oaxaca<- crime_monthly_by_year_type_Oaxaca %>%
  mutate(Total_Oaxaca= rowSums(across(all_of(months_Oaxaca))))

# Find top 3 crimes per year
top_crimes_per_year_Oaxaca <- crime_monthly_by_year_type_Oaxaca %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Oaxaca, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Oaxaca))

# View the result
print(top_crimes_per_year_Oaxaca, n = 30)
View(top_crimes_per_year_Oaxaca)
#### VOLVER A CORRER OAXACA 


