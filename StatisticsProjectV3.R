
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(tibble)
library(leaflet)

df <- read.csv("C:/Users/emman/OneDrive/Documents/EstadisticaGerardo/StatisticsPRoject/IDM_NM_jun25.csv", fileEncoding = "latin1")
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
# REDUJE EL DATASET A 17 variables
############################### AGUASCALIENTES 2015-2024 ######################
filtered_df_Aguascalientes <- df2[df2$Año %in% years_to_include & df2$Entidad == "AGUASCALIENTES", ]
filtered_df_Aguascalientes
View(filtered_df_Aguascalientes)

# Create a vector of the month columns
months_Aguascalientes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Sum all month values per year
crime_totals_Aguascalientes <- aggregate(. ~ Año, data = filtered_df_Aguascalientes[, c("Año", months_Aguascalientes)], sum)
crime_totals_Aguascalientes
# Optional: Add a "Total" column summing across all months
#crime_totals_Aguascalientes$Total_Aguascalientes <- rowSums(crime_totals_Aguascalientes[, months_Aguascalientes])

#crime_totals_Aguascalientes$Total_Aguascalientes #2015 - 2014

# Group by crime type and sum each month
crime_monthly_by_year_type_Aguascalientes <- filtered_df_Aguascalientes %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Aguascalientes), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Aguascalientes)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  mutate(Total_Aguascalientes = rowSums(across(all_of(months_Aguascalientes))))

# Group by crime type and sum all years to get overall totals
top_crimes_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Aguascalientes = sum(Total_Aguascalientes)) %>%
  arrange(desc(Total_Aguascalientes)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Aguascalientes)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  mutate(Total_Aguascalientes = rowSums(across(all_of(months_Aguascalientes))))

# Find top 3 crimes per year
top_crimes_per_year_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Aguascalientes, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Aguascalientes))

# View the result
print(top_crimes_per_year_Aguascalientes, n = 30)
View(top_crimes_per_year_Aguascalientes)
#unique(df$Entidad)
############################### BAJA CALIFORNIA 2015-2024 ######################
#years_to_include <- 2015:2024  # creates a vector of years from 2015 to 2024


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

# View the result
print(top_crimes_per_year_BC, n = 30)
View(top_crimes_per_year_BC)
#unique(df$Entidad)

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
############################### CAMPECHE 2015-2024 ######################
months_Campeche <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                     "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Campeche <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "CAMPECHE")
# Group by crime type and sum each month
crime_monthly_by_year_type_Campeche <- filtered_df_Campeche %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Campeche), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Campeche)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Campeche <- crime_monthly_by_year_type_Campeche %>%
  mutate(Total_Campeche = rowSums(across(all_of(months_Campeche))))

# Group by crime type and sum all years to get overall totals
top_crimes_Campeche <- crime_monthly_by_year_type_Campeche %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Campeche = sum(Total_Campeche)) %>%
  arrange(desc(Total_Campeche)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Campeche)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Campeche <- crime_monthly_by_year_type_Campeche %>%
  mutate(Total_Campeche = rowSums(across(all_of(months_Campeche))))

# Find top 3 crimes per year
top_crimes_per_year_Campeche <- crime_monthly_by_year_type_Campeche %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Campeche, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Campeche))

# View the result
print(top_crimes_per_year_Campeche, n = 30)
View(top_crimes_per_year_Campeche) #CHECAR LOS PRIMEROS AÑOS DE CAMPECHE 
############################### COAHUILA 2015-2024 ######################
months_Coahuila <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                     "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Coahuila <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "COAHUILA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Coahuila <- filtered_df_Coahuila %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Coahuila), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Coahuila)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Coahuila <- crime_monthly_by_year_type_Coahuila %>%
  mutate(Total_Coahuila = rowSums(across(all_of(months_Coahuila))))

# Group by crime type and sum all years to get overall totals
top_crimes_Coahuila <- crime_monthly_by_year_type_Coahuila %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Coahuila = sum(Total_Coahuila)) %>%
  arrange(desc(Total_Coahuila)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Coahuila)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Coahuila <- crime_monthly_by_year_type_Coahuila %>%
  mutate(Total_Coahuila = rowSums(across(all_of(months_Coahuila))))

# Find top 3 crimes per year
top_crimes_per_year_Coahuila <- crime_monthly_by_year_type_Coahuila %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Coahuila, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Coahuila))

# View the result
print(top_crimes_per_year_Coahuila, n = 30)
View(top_crimes_per_year_Coahuila)
############################### COLIMA 2015-2024 ######################
months_Colima <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Colima <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "COLIMA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Colima <- filtered_df_Colima %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Colima), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Colima)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Colima <- crime_monthly_by_year_type_Colima %>%
  mutate(Total_Colima = rowSums(across(all_of(months_Colima))))

# Group by crime type and sum all years to get overall totals
top_crimes_Colima <- crime_monthly_by_year_type_Colima %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Colima = sum(Total_Colima)) %>%
  arrange(desc(Total_Colima)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Colima)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Colima <- crime_monthly_by_year_type_Colima %>%
  mutate(Total_Colima = rowSums(across(all_of(months_Colima))))

# Find top 3 crimes per year
top_crimes_per_year_Colima <- crime_monthly_by_year_type_Colima %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Colima, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Colima))

# View the result
print(top_crimes_per_year_Colima, n = 30)
View(top_crimes_per_year_Colima)
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
############################### CHIHUAHUA 2015-2024 ######################
months_Chihuahua <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Chihuahua <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "CHIHUAHUA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Chihuahua <- filtered_df_Chihuahua %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Chihuahua), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Chihuahua)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Chihuahua <- crime_monthly_by_year_type_Chihuahua %>%
  mutate(Total_Chihuahua = rowSums(across(all_of(months_Chihuahua))))

# Group by crime type and sum all years to get overall totals
top_crimes_Chihuahua <- crime_monthly_by_year_type_Chihuahua %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Chihuahua = sum(Total_Chihuahua)) %>%
  arrange(desc(Total_Chihuahua)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Chihuahua)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Chihuahua <- crime_monthly_by_year_type_Chihuahua %>%
  mutate(Total_Chihuahua = rowSums(across(all_of(months_Chihuahua))))

# Find top 3 crimes per year
top_crimes_per_year_Chihuahua <- crime_monthly_by_year_type_Chihuahua %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Chihuahua, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Chihuahua))

# View the result
print(top_crimes_per_year_Chihuahua, n = 30)
View(top_crimes_per_year_Chihuahua)
############################### CIUDAD DE MÉXICO 2015-2024 ######################
months_CDMX <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_CDMX <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "CIUDAD DE MÉXICO")
# Group by crime type and sum each month
crime_monthly_by_year_type_CDMX <- filtered_df_CDMX %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_CDMX), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_CDMX)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_CDMX <- crime_monthly_by_year_type_CDMX %>%
  mutate(Total_CDMX = rowSums(across(all_of(months_CDMX))))

# Group by crime type and sum all years to get overall totals
top_crimes_CDMX <- crime_monthly_by_year_type_CDMX %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_CDMX = sum(Total_CDMX)) %>%
  arrange(desc(Total_CDMX)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_CDMX)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_CDMX <- crime_monthly_by_year_type_CDMX %>%
  mutate(Total_CDMX = rowSums(across(all_of(months_CDMX))))

# Find top 3 crimes per year
top_crimes_per_year_CDMX <- crime_monthly_by_year_type_CDMX %>%
  group_by(Año) %>%
  slice_max(order_by = Total_CDMX, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_CDMX))

# View the result
print(top_crimes_per_year_CDMX, n = 30)
View(top_crimes_per_year_CDMX)
############################### DURANGO 2015-2024 ######################
months_Durango <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Durango <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "DURANGO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Durango <- filtered_df_Durango %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Durango), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Durango)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Durango <- crime_monthly_by_year_type_Durango %>%
  mutate(Total_Durango = rowSums(across(all_of(months_Durango))))

# Group by crime type and sum all years to get overall totals
top_crimes_Durango <- crime_monthly_by_year_type_Durango %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Durango = sum(Total_Durango)) %>%
  arrange(desc(Total_Durango)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Durango)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Durango <- crime_monthly_by_year_type_Durango %>%
  mutate(Total_Durango = rowSums(across(all_of(months_Durango))))

# Find top 3 crimes per year
top_crimes_per_year_Durango <- crime_monthly_by_year_type_Durango %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Durango, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Durango))

# View the result
print(top_crimes_per_year_Durango, n = 30)
View(top_crimes_per_year_Durango)
############################### GUANAJUATO 2015-2024 ######################
months_Guanajuato <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                       "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Guanajuato <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "GUANAJUATO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Guanajuato <- filtered_df_Guanajuato %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Guanajuato), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Guanajuato)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Guanajuato <- crime_monthly_by_year_type_Guanajuato %>%
  mutate(Total_Guanajuato = rowSums(across(all_of(months_Guanajuato))))

# Group by crime type and sum all years to get overall totals
top_crimes_Guanajuato <- crime_monthly_by_year_type_Guanajuato %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Guanajuato = sum(Total_Guanajuato)) %>%
  arrange(desc(Total_Guanajuato)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Guanajuato)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Guanajuato <- crime_monthly_by_year_type_Guanajuato %>%
  mutate(Total_Guanajuato = rowSums(across(all_of(months_Guanajuato))))

# Find top 3 crimes per year
top_crimes_per_year_Guanajuato <- crime_monthly_by_year_type_Guanajuato %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Guanajuato, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Guanajuato))

# View the result
print(top_crimes_per_year_Guanajuato, n = 30)
View(top_crimes_per_year_Guanajuato)
#### VER A QUE SE REFIERE CON ' OTROS DELITOS DEL FUERO COMUN'
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
#### VER A QUE SE REFIERE CON ' OTROS DELITOS DEL FUERO COMUN'
############################### HIDALGO 2015-2024 ######################
months_Hidalgo <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Hidalgo <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "HIDALGO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Hidalgo <- filtered_df_Hidalgo %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Hidalgo), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Hidalgo)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Hidalgo <- crime_monthly_by_year_type_Hidalgo %>%
  mutate(Total_Hidalgo = rowSums(across(all_of(months_Hidalgo))))

# Group by crime type and sum all years to get overall totals
top_crimes_Hidalgo <- crime_monthly_by_year_type_Hidalgo %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Hidalgo = sum(Total_Hidalgo)) %>%
  arrange(desc(Total_Hidalgo)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Hidalgo)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Hidalgo <- crime_monthly_by_year_type_Hidalgo %>%
  mutate(Total_Hidalgo = rowSums(across(all_of(months_Hidalgo))))

# Find top 3 crimes per year
top_crimes_per_year_Hidalgo <- crime_monthly_by_year_type_Hidalgo %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Hidalgo, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Hidalgo))

# View the result
print(top_crimes_per_year_Hidalgo, n = 30)
View(top_crimes_per_year_Hidalgo)
############################### JALISCO 2015-2024 ######################
months_Jalisco <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Jalisco <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "JALISCO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Jalisco <- filtered_df_Jalisco %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Jalisco), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Jalisco)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Jalisco <- crime_monthly_by_year_type_Jalisco %>%
  mutate(Total_Jalisco = rowSums(across(all_of(months_Jalisco))))

# Group by crime type and sum all years to get overall totals
top_crimes_Jalisco <- crime_monthly_by_year_type_Jalisco %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Jalisco = sum(Total_Jalisco)) %>%
  arrange(desc(Total_Jalisco)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Jalisco)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Jalisco <- crime_monthly_by_year_type_Jalisco %>%
  mutate(Total_Jalisco = rowSums(across(all_of(months_Jalisco))))

# Find top 3 crimes per year
top_crimes_per_year_Jalisco <- crime_monthly_by_year_type_Jalisco %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Jalisco, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Jalisco))

# View the result
print(top_crimes_per_year_Jalisco, n = 30)
View(top_crimes_per_year_Jalisco)
############################### MÉXICO 2015-2024 ######################
months_EdoMex <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_EdoMex <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "MÉXICO")
# Group by crime type and sum each month
crime_monthly_by_year_type_EdoMex <- filtered_df_EdoMex %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_EdoMex), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_EdoMex)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_EdoMex <- crime_monthly_by_year_type_EdoMex %>%
  mutate(Total_EdoMex = rowSums(across(all_of(months_EdoMex))))

# Group by crime type and sum all years to get overall totals
top_crimes_EdoMex <- crime_monthly_by_year_type_EdoMex %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_EdoMex = sum(Total_EdoMex)) %>%
  arrange(desc(Total_EdoMex)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_EdoMex)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_EdoMex <- crime_monthly_by_year_type_EdoMex %>%
  mutate(Total_EdoMex = rowSums(across(all_of(months_EdoMex))))

# Find top 3 crimes per year
top_crimes_per_year_EdoMex <- crime_monthly_by_year_type_EdoMex %>%
  group_by(Año) %>%
  slice_max(order_by = Total_EdoMex, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_EdoMex))

# View the result
print(top_crimes_per_year_EdoMex, n = 30)
View(top_crimes_per_year_EdoMex)
############################### MICHOACÁN 2015-2024 ######################
months_Michoacan <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Michoacan <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "MICHOACÁN")
# Group by crime type and sum each month
crime_monthly_by_year_type_Michoacan <- filtered_df_Michoacan %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Michoacan), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Michoacan)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Michoacan <- crime_monthly_by_year_type_Michoacan %>%
  mutate(Total_Michoacan = rowSums(across(all_of(months_Michoacan))))

# Group by crime type and sum all years to get overall totals
top_crimes_Michoacan <- crime_monthly_by_year_type_Michoacan %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Michoacan = sum(Total_Michoacan)) %>%
  arrange(desc(Total_Michoacan)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Michoacan)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Michoacan <- crime_monthly_by_year_type_Michoacan %>%
  mutate(Total_Michoacan = rowSums(across(all_of(months_Michoacan))))

# Find top 3 crimes per year
top_crimes_per_year_Michoacan <- crime_monthly_by_year_type_Michoacan %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Michoacan, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Michoacan))

# View the result
print(top_crimes_per_year_Michoacan, n = 30)
View(top_crimes_per_year_Michoacan)

############################### MORELOS 2015-2024 ######################
months_Morelos <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Morelos <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "MORELOS")
# Group by crime type and sum each month
crime_monthly_by_year_type_Morelos <- filtered_df_Morelos %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Morelos), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Morelos)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Morelos <- crime_monthly_by_year_type_Morelos %>%
  mutate(Total_Morelos = rowSums(across(all_of(months_Morelos))))

# Group by crime type and sum all years to get overall totals
top_crimes_Morelos <- crime_monthly_by_year_type_Morelos %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Morelos = sum(Total_Morelos)) %>%
  arrange(desc(Total_Morelos)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Morelos)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Morelos <- crime_monthly_by_year_type_Morelos %>%
  mutate(Total_Morelos = rowSums(across(all_of(months_Morelos))))

# Find top 3 crimes per year
top_crimes_per_year_Morelos <- crime_monthly_by_year_type_Morelos %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Morelos, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Morelos))

# View the result
print(top_crimes_per_year_Morelos, n = 30)
View(top_crimes_per_year_Morelos)


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
############################### NUEVO LEÓN 2015-2024 ######################
months_NL <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_NL <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "NUEVO LEÓN")
# Group by crime type and sum each month
crime_monthly_by_year_type_NL <- filtered_df_NL %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_NL), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_NL)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_NL <- crime_monthly_by_year_type_NL %>%
  mutate(Total_NL = rowSums(across(all_of(months_NL))))

# Group by crime type and sum all years to get overall totals
top_crimes_NL <- crime_monthly_by_year_type_NL %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_NL = sum(Total_NL)) %>%
  arrange(desc(Total_NL)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_NL)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_NL<- crime_monthly_by_year_type_NL %>%
  mutate(Total_NL= rowSums(across(all_of(months_NL))))

# Find top 3 crimes per year
top_crimes_per_year_NL <- crime_monthly_by_year_type_NL %>%
  group_by(Año) %>%
  slice_max(order_by = Total_NL, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_NL))

# View the result
print(top_crimes_per_year_NL, n = 30)
View(top_crimes_per_year_NL)
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
############################### PUEBLA 2015-2024 ######################
months_Puebla <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Puebla <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "PUEBLA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Puebla <- filtered_df_Puebla%>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Puebla), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Puebla)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Puebla <- crime_monthly_by_year_type_Puebla %>%
  mutate(Total_Puebla = rowSums(across(all_of(months_Puebla))))

# Group by crime type and sum all years to get overall totals
top_crimes_Puebla <- crime_monthly_by_year_type_Puebla %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Puebla= sum(Total_Puebla)) %>%
  arrange(desc(Total_Puebla)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Puebla)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Puebla<- crime_monthly_by_year_type_Puebla %>%
  mutate(Total_Puebla= rowSums(across(all_of(months_Puebla))))

# Find top 3 crimes per year
top_crimes_per_year_Puebla <- crime_monthly_by_year_type_Puebla %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Puebla, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Puebla))

# View the result
print(top_crimes_per_year_Puebla, n = 30)
View(top_crimes_per_year_Puebla)
############################### QUERÉTARO 2015-2024 ######################
months_Queretaro <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Queretaro <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "QUERÉTARO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Queretaro <- filtered_df_Queretaro %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Queretaro), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Queretaro)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Queretaro <- crime_monthly_by_year_type_Queretaro %>%
  mutate(Total_Queretaro = rowSums(across(all_of(months_Queretaro))))

# Group by crime type and sum all years to get overall totals
top_crimes_Queretaro <- crime_monthly_by_year_type_Queretaro %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Queretaro= sum(Total_Queretaro)) %>%
  arrange(desc(Total_Queretaro)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Queretaro)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Queretaro<- crime_monthly_by_year_type_Queretaro %>%
  mutate(Total_Queretaro = rowSums(across(all_of(months_Queretaro))))

# Find top 3 crimes per year
top_crimes_per_year_Queretaro <- crime_monthly_by_year_type_Queretaro %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Queretaro, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Queretaro))

# View the result
print(top_crimes_per_year_Queretaro, n = 30)
View(top_crimes_per_year_Queretaro)
############################### QUINTANA ROO 2015-2024 ######################
months_QRoo <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_QRoo <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "QUINTANA ROO")
# Group by crime type and sum each month
crime_monthly_by_year_type_QRoo <- filtered_df_QRoo %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_QRoo), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_QRoo)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_QRoo <- crime_monthly_by_year_type_QRoo %>%
  mutate(Total_QRoo = rowSums(across(all_of(months_QRoo))))

# Group by crime type and sum all years to get overall totals
top_crimes_QRoo <- crime_monthly_by_year_type_QRoo %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_QRoo= sum(Total_QRoo)) %>%
  arrange(desc(Total_QRoo)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_QRoo)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_QRoo<- crime_monthly_by_year_type_QRoo %>%
  mutate(Total_QRoo = rowSums(across(all_of(months_QRoo))))

# Find top 3 crimes per year
top_crimes_per_year_QRoo <- crime_monthly_by_year_type_QRoo %>%
  group_by(Año) %>%
  slice_max(order_by = Total_QRoo, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_QRoo))

# View the result
print(top_crimes_per_year_QRoo, n = 30)
View(top_crimes_per_year_QRoo)
############################### SAN LUIS POTOSÍ 2015-2024 ######################
months_SLP <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_SLP <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "SAN LUIS POTOSÍ")
# Group by crime type and sum each month
crime_monthly_by_year_type_SLP <- filtered_df_SLP %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_SLP), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_SLP)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_SLP <- crime_monthly_by_year_type_SLP %>%
  mutate(Total_SLP = rowSums(across(all_of(months_SLP))))

# Group by crime type and sum all years to get overall totals
top_crimes_SLP <- crime_monthly_by_year_type_SLP %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_SLP= sum(Total_SLP)) %>%
  arrange(desc(Total_SLP)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_SLP)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_SLP <- crime_monthly_by_year_type_SLP %>%
  mutate(Total_SLP = rowSums(across(all_of(months_SLP))))

# Find top 3 crimes per year
top_crimes_per_year_SLP <- crime_monthly_by_year_type_SLP %>%
  group_by(Año) %>%
  slice_max(order_by = Total_SLP, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_SLP))

# View the result
print(top_crimes_per_year_SLP, n = 30)
View(top_crimes_per_year_SLP)
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
View(top_crimes_per_year_Sonora)
############################### TABASCO 2015-2024 ######################
months_Tabasco <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Tabasco <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "TABASCO")
# Group by crime type and sum each month
crime_monthly_by_year_type_Tabasco <- filtered_df_Tabasco %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Tabasco), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Tabasco)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Tabasco <- crime_monthly_by_year_type_Tabasco %>%
  mutate(Total_Tabasco = rowSums(across(all_of(months_Tabasco))))

# Group by crime type and sum all years to get overall totals
top_crimes_Tabasco <- crime_monthly_by_year_type_Tabasco %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Tabasco = sum(Total_Tabasco)) %>%
  arrange(desc(Total_Tabasco)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Tabasco)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Tabasco <- crime_monthly_by_year_type_Tabasco %>%
  mutate(Total_Tabasco = rowSums(across(all_of(months_Tabasco))))

# Find top 3 crimes per year
top_crimes_per_year_Tabasco <- crime_monthly_by_year_type_Tabasco %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Tabasco, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Tabasco))

# View the result
print(top_crimes_per_year_Tabasco, n = 30)
View(top_crimes_per_year_Tabasco)
############################### TAMAULIPAS 2015-2024 ######################
months_Tamaulipas <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Tamaulipas <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "TAMAULIPAS")
# Group by crime type and sum each month
crime_monthly_by_year_type_Tamaulipas <- filtered_df_Tamaulipas %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Tamaulipas), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Tamaulipas)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Tamaulipas <- crime_monthly_by_year_type_Tamaulipas %>%
  mutate(Total_Tamaulipas = rowSums(across(all_of(months_Tamaulipas))))

# Group by crime type and sum all years to get overall totals
top_crimes_Tamaulipas <- crime_monthly_by_year_type_Tamaulipas %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Tamaulipas = sum(Total_Tamaulipas)) %>%
  arrange(desc(Total_Tamaulipas)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Tamaulipas)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Tamaulipas <- crime_monthly_by_year_type_Tamaulipas %>%
  mutate(Total_Tamaulipas = rowSums(across(all_of(months_Tamaulipas))))

# Find top 3 crimes per year
top_crimes_per_year_Tamaulipas <- crime_monthly_by_year_type_Tamaulipas %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Tamaulipas, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Tamaulipas))

# View the result
print(top_crimes_per_year_Tamaulipas, n = 30)
View(top_crimes_per_year_Tamaulipas)
############################### TLAXCALA 2015-2024 ######################
months_Tlaxcala <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Tlaxcala <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "TLAXCALA")
# Group by crime type and sum each month
crime_monthly_by_year_type_Tlaxcala <- filtered_df_Tlaxcala %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Tlaxcala), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Tlaxcala)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Tlaxcala <- crime_monthly_by_year_type_Tlaxcala %>%
  mutate(Total_Tlaxcala = rowSums(across(all_of(months_Tlaxcala))))

# Group by crime type and sum all years to get overall totals
top_crimes_Tlaxcala <- crime_monthly_by_year_type_Tlaxcala %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Tlaxcala = sum(Total_Tlaxcala)) %>%
  arrange(desc(Total_Tlaxcala)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Tlaxcala)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Tlaxcala <- crime_monthly_by_year_type_Tlaxcala %>%
  mutate(Total_Tlaxcala = rowSums(across(all_of(months_Tlaxcala))))

# Find top 3 crimes per year
top_crimes_per_year_Tlaxcala <- crime_monthly_by_year_type_Tlaxcala %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Tlaxcala, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Tlaxcala))

# View the result
print(top_crimes_per_year_Tlaxcala, n = 30)
View(top_crimes_per_year_Tlaxcala)
############################### VERACRUZ 2015-2024 ######################
months_Veracruz <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Veracruz <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "VERACRUZ")
# Group by crime type and sum each month
crime_monthly_by_year_type_Veracruz <- filtered_df_Veracruz %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Veracruz), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Veracruz)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Veracruz <- crime_monthly_by_year_type_Veracruz %>%
  mutate(Total_Veracruz = rowSums(across(all_of(months_Veracruz))))

# Group by crime type and sum all years to get overall totals
top_crimes_Veracruz <- crime_monthly_by_year_type_Veracruz %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Veracruz = sum(Total_Veracruz)) %>%
  arrange(desc(Total_Veracruz)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Veracruz)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Veracruz <- crime_monthly_by_year_type_Veracruz %>%
  mutate(Total_Veracruz = rowSums(across(all_of(months_Veracruz))))

# Find top 3 crimes per year
top_crimes_per_year_Veracruz <- crime_monthly_by_year_type_Veracruz %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Veracruz, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Veracruz))

# View the result
print(top_crimes_per_year_Veracruz, n = 30)
View(top_crimes_per_year_Veracruz)
############################### YUCATÁN 2015-2024 ######################
months_Yucatan <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
years_to_include <- 2015:2024
filtered_df_Yucatan <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "YUCATÁN")
# Group by crime type and sum each month
crime_monthly_by_year_type_Yucatan <- filtered_df_Yucatan %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Yucatan), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Yucatan)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Yucatan <- crime_monthly_by_year_type_Yucatan %>%
  mutate(Total_Yucatan = rowSums(across(all_of(months_Yucatan))))

# Group by crime type and sum all years to get overall totals
top_crimes_Yucatan <- crime_monthly_by_year_type_Yucatan %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Yucatan = sum(Total_Yucatan)) %>%
  arrange(desc(Total_Yucatan)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Yucatan)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Yucatan <- crime_monthly_by_year_type_Yucatan %>%
  mutate(Total_Yucatan = rowSums(across(all_of(months_Yucatan))))

# Find top 3 crimes per year
top_crimes_per_year_Yucatan <- crime_monthly_by_year_type_Yucatan %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Yucatan, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Yucatan))

# View the result
print(top_crimes_per_year_Yucatan, n = 30)
View(top_crimes_per_year_Yucatan)
############################### ZACATECAS 2015-2024 ######################
months_Zacatecas <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Zacatecas <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "ZACATECAS")
# Group by crime type and sum each month
crime_monthly_by_year_type_Zacatecas <- filtered_df_Zacatecas %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Zacatecas), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Zacatecas)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Zacatecas <- crime_monthly_by_year_type_Zacatecas %>%
  mutate(Total_Zacatecas = rowSums(across(all_of(months_Zacatecas))))

# Group by crime type and sum all years to get overall totals
top_crimes_Zacatecas <- crime_monthly_by_year_type_Zacatecas %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Zacatecas = sum(Total_Zacatecas)) %>%
  arrange(desc(Total_Zacatecas)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Zacatecas)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Zacatecas <- crime_monthly_by_year_type_Zacatecas %>%
  mutate(Total_Zacatecas = rowSums(across(all_of(months_Zacatecas))))

# Find top 3 crimes per year
top_crimes_per_year_Zacatecas <- crime_monthly_by_year_type_Zacatecas %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Zacatecas, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Zacatecas))

# View the result
print(top_crimes_per_year_Zacatecas, n = 30)
View(top_crimes_per_year_Zacatecas)

################################################################################################
################################################################################################
################################################################################################
#La tasa se calcula dividiendo el número total de delitos ocurridos entre la población de 18 años y más multiplicado por 100 000 habitantes.


#Fuente:  Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE)

# Fórmula de cálculo
# La tasa de criminalidad se calcula utilizando la siguiente fórmula:
  
# CR =  (N/P) X 100,000

# donde: CR es la tasa de criminalidad por cada 100.000 habitantes,

# N es el número de delitos específicos cometidos,

# P es la población total.

# jemplo de cálculo
#Si se cometieron 500 delitos en un año en una ciudad con una población de 50.000 habitantes, la tasa de criminalidad se calcularía como:


# CR = (500/50,000) x 100,000 = 1,000

# Esto significa que hay 1.000 delitos por cada 100.000 habitantes en la ciudad.

# ¿Por qué se calcula la tasa de criminalidad por cada 100.000 habitantes?

# Esto estandariza la tasa de criminalidad en áreas con diferentes tamaños de población, lo que hace que las comparaciones sean más significativas.
#
install.packages("readxl")

# Load the package
library(readxl)

# Read the Excel file
data <- read_excel("C:/Users/emman/Documents/ProyectoEstadistica/Poblacion_01.xlsx", sheet = 1)  # Specify the sheet name or index


View(data)
data$...6
### PENDIENTE hacerlo por cada 100,000 habitantes


###############################################################################################
################################################################################################
#################################################################################################


# Región Noroeste o Pacífico Norte: Comprende Baja California, Baja California Sur, Sonora, Sinaloa y Nayarit. Esta región se caracteriza por su fuerte actividad en la agricultura, la pesca y el turismo, además de contar con una creciente industria manufacturera.

# 2nd Region : Región Norte: Incluye estados como Chihuahua, Coahuila, Durango, Nuevo León, San Luis Potosí, Sinaloa, Sonora y Tamaulipas. Es la región más rica del país, generando aproximadamente el 30% del PIB nacional y concentrando una gran parte de las exportaciones. Se destaca por su industrialización y empleo bien remunerado.

#3rd Region : Región Centro-Occidente: Compuesta por Aguascalientes, Colima, Guanajuato, Jalisco, Michoacán, Nayarit, Querétaro y Zacatecas. Esta región tiene una menor participación en la generación de riqueza, pero es importante en la producción agrícola y la industria

# 4th Region: Región Centro-Sur: Incluye la Ciudad de México, Guerrero, Hidalgo, Estado de México, Morelos, Puebla y Tlaxcala. Es una de las regiones más importantes en términos de población y actividad económica, aunque enfrenta desafíos en términos de pobreza y marginación. 

# 5th Region : Región del Golfo de México: Comprende estados como Veracruz y Tabasco, con una economía basada en la agricultura, la pesca y la industria petrolera.
#6th Region : Región Península de Yucatán: Incluye Yucatán, Campeche y Quintana Roo, destacándose por su turismo y agricultura, especialmente en la producción de hortalizas y frutas.


##########################################################################################################################################
####################################################### NOROESTE: BC, BCS, SONORA, SINALOA y NAYARIT #####################################
##########################################################################################################################################


############################### BAJA CALIFORNIA 2015-2024 ######################
desc_stats_BC <- crime_monthly_by_year_type_BC %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_BC, na.rm = TRUE),  # Total across all years
    media = mean(Total_BC, na.rm = TRUE),  # Average per year
    mediana = median(Total_BC, na.rm = TRUE),
    minimo = min(Total_BC, na.rm = TRUE),
    maximo = max(Total_BC, na.rm = TRUE),
    desviacion = sd(Total_BC, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_BC)


######## Lectura de los datos
# Robo es, por mucho, el delito más frecuente en Baja California, 
# con más de 38,000 casos anuales en promedio y una alta variabilidad
# (desviación estándar de 8,747): hubo años con tan solo 27,993 casos y otros con hasta 51,385.
# Esto sugiere que el fenómeno ha experimentado subidas y bajadas notables, posiblemente por cambios económicos, 
# sociales o en la política criminal.

# Violencia familiar ha mantenido niveles elevados y relativamente estables, 
# con una desviación estándar menor (2,435): cada año se reportan entre 8,315 y 14,729 casos, 
# con la mayoría de los años acercándose al promedio.

# Lesiones y Daño a la propiedad siguen patrones similares de alta incidencia, aunque con menos variabilidad que el robo.

# Narcomenudeo resalta por tener un mínimo de 0 casos en algún año, pero en otros supera los 10,000,
# mostrando gran inestabilidad (desviación de 3,584).

# Homicidio tiene una media de poco más de 2,500 casos al año, pero con un máximo de 3,225, lo que sugiere que aunque 
# es grave, su volumen anual es más predecible que delitos como robo.

# Amenazas, allanamiento, y fraude se encuentran en un segundo plano, pero sus cifras siguen siendo relevantes para 
# la política pública local.

# ¿Cómo interpretar estos resultados?
#  Rangos amplios y varianza alta (desviación estándar elevada, diferencia grande entre mínimo y máximo)
# indican delitos cuyas cifras pueden cambiar de un año a otro por factores específicos (coyunturas, operativos, 
# reformas legales, etc.).

# Mediana muy por debajo de la media sugiere un aumento reciente en la incidencia (por ejemplo, si la media es
# alta pero la mediana baja, probablemente los casos aumentaron en la parte final del periodo).

# Estabilidad (desviación baja, mediana cercana al promedio) muestra que el delito tiene un comportamiento
# constante, lo que facilita la planeación y evaluación de políticas.

# Casos como Narcomenudeo con un año en cero pueden deberse a cambios administrativos o interrupciones en el registro 
# de datos.

# Create the tibble with your data
crime_data <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo", 10, 381625, 38162, 35319, 27993, 51385, 8747,
  "Violencia familiar", 10, 112086, 11209, 10618, 8315, 14729, 2435,
  "Lesiones", 10, 93265, 9326, 8370, 6810, 15325, 2872,
  "Daño a la propiedad", 10, 84282, 8428, 8072, 7105, 10975, 1321,
  "Otros delitos del Fuero Común", 10, 83792, 8379, 8321, 5774, 15238, 2713,
  "Narcomenudeo", 10, 71311, 7131, 8116, 0, 10307, 3584,
  "Amenazas", 10, 45061, 4506, 4148, 3328, 6545, 1137,
  "Allanamiento de morada", 10, 25905, 2590, 2496, 1906, 3440, 509,
  "Homicidio", 10, 25832, 2583, 2806, 1219, 3225, 703,
  "Fraude", 10, 22236, 2224, 1924, 1384, 3753, 779
)

# Create the bar chart
ggplot(crime_data_2, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # This new line adds the text labels above the bars
  geom_text(aes(label = media), vjust = -0.3, size = 3) +
  labs(
    title = "Promedio anual de conteo de delitos en BC",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

##########################################################################################################################################
####################################################### NORTE: COAHUILA, CHIHUAHUA, DURANGO, ZACATECAS Y SLP #####################################
##########################################################################################################################################
#################################### COAHUILA ################################
desc_stats_coahuila <- crime_monthly_by_year_type_Coahuila %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Coahuila, na.rm = TRUE),  # Total across all years
    media = mean(Total_Coahuila, na.rm = TRUE),  # Average per year
    mediana = median(Total_Coahuila, na.rm = TRUE),
    minimo = min(Total_Coahuila, na.rm = TRUE),
    maximo = max(Total_Coahuila, na.rm = TRUE),
    desviacion = sd(Total_Coahuila, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_coahuila)
# Note: Truncated crime type names have been completed for clarity.
crime_data_coahuila <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Violencia familiar",              10, 97617, 9762, 10219, 2154, 13022,
  "Robo",                            10, 82584, 8258, 7229,  5587, 13140,
  "Daño a la propiedad",             10, 74825, 7482, 7644,  5835, 8330,
  "Narcomenudeo",                    10, 71788, 7179, 8670,  2331, 11032,
  "Lesiones",                        10, 52056, 5206, 5366,  4141, 5832,
  "Amenazas",                        10, 51844, 5184, 4622,  2097, 8851,
  "Otros delitos del Fuero Común",   10, 46297, 4630, 2810,  1365, 11223,
  "Fraude",                          10, 13415, 1342, 1114,  824,  2457,
  "Otros delitos contra la vida",    10, 13103, 1310, 1173,  973,  1920,
  "Abuso sexual",                    10,  6668,  667,  576,  311,  1046
)

# Create the bar chart with text labels
ggplot(crime_data_coahuila, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "blue") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Coahuila",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### CHIHUAHUA ################################
desc_stats_chihuahua <- crime_monthly_by_year_type_Chihuahua %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Chihuahua, na.rm = TRUE),  # Total across all years
    media = mean(Total_Chihuahua, na.rm = TRUE),  # Average per year
    mediana = median(Total_Chihuahua, na.rm = TRUE),
    minimo = min(Total_Chihuahua, na.rm = TRUE),
    maximo = max(Total_Chihuahua, na.rm = TRUE),
    desviacion = sd(Total_Chihuahua, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_chihuahua)
# Note: Truncated crime type names have been completed for clarity.
# Create the tibble with the new data
# Note: Truncated crime type names have been completed for clarity.
crime_data_chihuahua <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                             10, 150736, 15074, 14741, 12914, 17366,
  "Violencia familiar",               10, 124185, 12418, 12116, 10968, 14235,
  "Daño a la propiedad",              10,  77533,  7753,  7776,  6390,  9816,
  "Narcomenudeo",                     10,  55901,  5590,  5081,  3064,  8019,
  "Lesiones",                         10,  54253,  5425,  5343,  4362,  6753,
  "Fraude",                           10,  38987,  3899,  2914,  2248,  7326,
  "Amenazas",                         10,  31170,  3117,  3082,  2392,  3864,
  "Otros delitos del Fuero Común",    10,  27084,  2708,  2534,  1640,  3902,
  "Homicidio",                        10,  20388,  2039,  2070,  1286,  2567,
  "Incumplimiento de obligaciones",   10,  20144,  2014,  1918,  1559,  2594
)


# Create the bar chart with text labels
ggplot(crime_data_chihuahua, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "olivedrab") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Chihuahua",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### DURANGO ################################
desc_stats_durango <- crime_monthly_by_year_type_Durango %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Durango, na.rm = TRUE),  # Total across all years
    media = mean(Total_Durango, na.rm = TRUE),  # Average per year
    mediana = median(Total_Durango, na.rm = TRUE),
    minimo = min(Total_Durango, na.rm = TRUE),
    maximo = max(Total_Durango, na.rm = TRUE),
    desviacion = sd(Total_Durango, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_durango)
# Note: Truncated crime type names have been completed for clarity.
# Create the tibble with the data for Durango
# Note: Truncated crime type names have been completed for clarity.
crime_data_durango <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                            10, 81445, 8144, 9274, 2847, 11158,
  "Violencia familiar",              10, 51755, 5176, 5208, 3521, 6320,
  "Lesiones",                        10, 37182, 3718, 3818, 2742, 4894,
  "Daño a la propiedad",             10, 24875, 2488, 2474, 1977, 3154,
  "Fraude",                          10, 17406, 1741, 1640, 1242, 2227,
  "Amenazas",                        10, 14298, 1430, 1268, 970,  2001,
  "Otros delitos del Fuero Común",   10, 10810, 1081, 934,  653,  1901,
  "Narcomenudeo",                    10,  7935,  794,  819,  314,  1079,
  "Abuso de confianza",              10,  6078,  608,  618,  352,   812,
  "Abuso sexual",                    10,  4211,  421,  442,  182,   570
)

# Create the bar chart with text labels
ggplot(crime_data_durango, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Durango",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### ZACATECAS ################################
desc_stats_zacatecas <- crime_monthly_by_year_type_Zacatecas %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Zacatecas, na.rm = TRUE),  # Total across all years
    media = mean(Total_Zacatecas, na.rm = TRUE),  # Average per year
    mediana = median(Total_Zacatecas, na.rm = TRUE),
    minimo = min(Total_Zacatecas, na.rm = TRUE),
    maximo = max(Total_Zacatecas, na.rm = TRUE),
    desviacion = sd(Total_Zacatecas, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_zacatecas)
# Create the tibble with the data for Zacatecas
# Note: Truncated crime type names have been completed for clarity.
crime_data_zacatecas <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                                      10, 66995, 6700, 6681, 5501, 7733,
  "Violencia familiar",                        10, 25839, 2584, 3055, 735,  3673,
  "Lesiones",                                  10, 22940, 2294, 2390, 1735, 3141,
  "Daño a la propiedad",                       10, 20852, 2085, 2082, 1406, 2717,
  "Otros delitos del Fuero Común",             10, 17406, 1741, 1725, 1279, 2184,
  "Fraude",                                    10, 11220, 1122, 1061, 744,  1660,
  "Amenazas",                                  10,  9825,  982, 1179, 344,  1412,
  "Homicidio",                                 10,  8612,  861,  746, 357,  1467,
  "Incumplimiento de obligaciones",            10,  4913,  491,  510, 189,   796,
  "Otros delitos que atentan contra la vida",  10,  4141,  414,  330, 151,   781
)

# Create the bar chart with text labels
ggplot(crime_data_zacatecas, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "saddlebrown") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Zacatecas",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### SAN LUIS POTOSI ################################
desc_stats_slp <- crime_monthly_by_year_type_SLP %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_SLP, na.rm = TRUE),  # Total across all years
    media = mean(Total_SLP, na.rm = TRUE),  # Average per year
    mediana = median(Total_SLP, na.rm = TRUE),
    minimo = min(Total_SLP, na.rm = TRUE),
    maximo = max(Total_SLP, na.rm = TRUE),
    desviacion = sd(Total_SLP, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_slp)
# Create the tibble with the data for San Luis Potosí
# Note: Truncated crime type names have been completed for clarity.
crime_data_slp <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                             10, 128372, 12837, 13870, 6033, 16495,
  "Violencia familiar",               10,  71416,  7142,  8007, 2924,  9562,
  "Daño a la propiedad",              10,  46954,  4695,  4959, 2465,  6059,
  "Lesiones",                         10,  43359,  4336,  4421, 2723,  5324,
  "Amenazas",                         10,  25103,  2510,  2857, 1274,  3429,
  "Otros delitos del Fuero Común",    10,  23874,  2387,  2218, 1055,  4057,
  "Narcomenudeo",                     10,  20845,  2084,   900,  142,  7426,
  "Fraude",                           10,  19758,  1976,  2089, 1011,  3230,
  "Otros delitos contra la vida",     10,   8376,   838,   752,    3,  1966,
  "Homicidio",                        10,   7372,   737,   750,  392,   967
)

# Create the bar chart with text labels
ggplot(crime_data_slp, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en San Luis Potosí",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################################################################################
####################################################### NORESTE: NUEVO LEON Y TAMAULIPAS #####################################
##########################################################################################################################################
#################################### NUEVO LEON ################################

desc_stats_nl <- crime_monthly_by_year_type_NL %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_NL, na.rm = TRUE),  # Total across all years
    media = mean(Total_NL, na.rm = TRUE),  # Average per year
    mediana = median(Total_NL, na.rm = TRUE),
    minimo = min(Total_NL, na.rm = TRUE),
    maximo = max(Total_NL, na.rm = TRUE),
    desviacion = sd(Total_NL, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_nl)

# Create the tibble with the data for Nuevo León
# Note: Truncated crime type names have been completed for clarity.
crime_data_nuevo_leon <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Violencia familiar",                        10, 185682, 18568, 17856, 16339, 22480,
  "Robo",                                      10, 148014, 14801, 15164,  9734, 19000,
  "Daño a la propiedad",                       10,  76784,  7678,  8004,  5332,  9330,
  "Lesiones",                                  10,  73734,  7373,  7556,  5243,  8826,
  "Amenazas",                                  10,  50767,  5077,  5064,  3561,  7354,
  "Otros delitos contra la vida",              10,  49501,  4950,  5010,  3345,  6951,
  "Fraude",                                    10,  45474,  4547,  4484,  2502,  6070,
  "Otros delitos del Fuero Común",             10,  42009,  4201,  3252,  2306,  6750,
  "Narcomenudeo",                              10,  41525,  4152,  3677,  1611,  8158,
  "Otros delitos que atentan contra la vida",  10,  22313,  2231,  2244,  1978,  2553
)

# Create the bar chart with text labels
ggplot(crime_data_nuevo_leon, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Nuevo León",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### TAMAULIPAS ################################

desc_stats_tamaulipas <- crime_monthly_by_year_type_Tamaulipas %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Tamaulipas, na.rm = TRUE),  # Total across all years
    media = mean(Total_Tamaulipas, na.rm = TRUE),  # Average per year
    mediana = median(Total_Tamaulipas, na.rm = TRUE),
    minimo = min(Total_Tamaulipas, na.rm = TRUE),
    maximo = max(Total_Tamaulipas, na.rm = TRUE),
    desviacion = sd(Total_Tamaulipas, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_tamaulipas)

# Create the tibble with the data for Tamaulipas
# Note: Truncated crime type names have been completed for clarity.
crime_data_tamaulipas <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                             10, 123029, 12303, 11330, 8440, 19273,
  "Violencia familiar",               10,  69630,  6963,  7270, 2394,  8785,
  "Lesiones",                         10,  37974,  3797,  3722, 2865,  5029,
  "Daño a la propiedad",              10,  36841,  3684,  3936, 2107,  4180,
  "Amenazas",                         10,  21242,  2124,  2196, 1500,  2561,
  "Otros delitos del Fuero Común",    10,  20853,  2085,  1713,  982,  4722,
  "Incumplimiento de obligaciones",   10,  18792,  1879,  1860, 1175,  2785,
  "Fraude",                           10,  14559,  1456,  1458,  967,  1802,
  "Homicidio",                        10,  13405,  1340,  1324, 1018,  1618,
  "Otros delitos contra la vida",     10,   7579,   758,   802,  416,   876
)

# Create the bar chart with text labels
ggplot(crime_data_tamaulipas, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Tamaulipas",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################################################################################
####################################################### OCCIDENTE: JALISCO, AGUASCALIENTES, COLIMA, MICHOACAN Y GUANAJUATO #####################################
##########################################################################################################################################
#################################### JALISCO ################################

desc_stats_jalisco <- crime_monthly_by_year_type_Jalisco %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Jalisco, na.rm = TRUE),  # Total across all years
    media = mean(Total_Jalisco, na.rm = TRUE),  # Average per year
    mediana = median(Total_Jalisco, na.rm = TRUE),
    minimo = min(Total_Jalisco, na.rm = TRUE),
    maximo = max(Total_Jalisco, na.rm = TRUE),
    desviacion = sd(Total_Jalisco, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_jalisco)
# Create the tibble with the data for Jalisco
# Note: Truncated crime type names have been completed for clarity.
crime_data_jalisco <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                            10, 568530, 56853, 52767, 27501, 88606,
  "Lesiones",                        10, 120177, 12018, 12113, 10037, 13707,
  "Violencia familiar",              10, 119148, 11915, 11662,  8543, 16904,
  "Otros delitos del Fuero Común",   10, 118731, 11873, 11047,  8494, 16308,
  "Amenazas",                        10,  97297,  9730,  9868,  7696, 11039,
  "Fraude",                          10,  82172,  8217,  7550,  6587, 11884,
  "Daño a la propiedad",             10,  70354,  7035,  7540,  2251,  9526,
  "Abuso sexual",                    10,  28955,  2896,  2362,  1572,  5323,
  "Homicidio",                       10,  24299,  2430,  2560,  1736,  2762,
  "Narcomenudeo",                    10,  20520,  2052,  1802,  1067,  5045
)

# Create the bar chart with text labels
ggplot(crime_data_jalisco, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "gold") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Jalisco",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### AGUASCALIENTES ################################

desc_stats_aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Aguascalientes, na.rm = TRUE),  # Total across all years
    media = mean(Total_Aguascalientes, na.rm = TRUE),  # Average per year
    mediana = median(Total_Aguascalientes, na.rm = TRUE),
    minimo = min(Total_Aguascalientes, na.rm = TRUE),
    maximo = max(Total_Aguascalientes, na.rm = TRUE),
    desviacion = sd(Total_Aguascalientes, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_aguascalientes)
# Create the tibble with the data for Aguascalientes
# Note: Truncated crime type names have been completed for clarity.
crime_data_aguascalientes <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                            10, 118614, 11861, 10884, 10103, 15697,
  "Lesiones",                        10,  43582,  4358,  4586,  2771,  5019,
  "Daño a la propiedad",             10,  37650,  3765,  4124,  1577,  5136,
  "Amenazas",                        10,  28926,  2893,  3229,  1041,  3819,
  "Otros delitos del Fuero Común",   10,  25432,  2543,  2252,  1349,  4453,
  "Violencia familiar",              10,  19537,  1954,  2152,   481,  3358,
  "Fraude",                          10,  18715,  1872,  1708,   557,  3124,
  "Narcomenudeo",                    10,  15749,  1575,  1898,   614,  2206,
  "Abuso de confianza",              10,   6774,   677,   710,   394,   833,
  "Falsificación",                   10,   5070,   507,   404,   195,   848
)

# Create the bar chart with text labels
ggplot(crime_data_aguascalientes, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Aguascalientes",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### MICHOACAN ################################

desc_stats_michoacan <- crime_monthly_by_year_type_Michoacan %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Michoacan, na.rm = TRUE),  # Total across all years
    media = mean(Total_Michoacan, na.rm = TRUE),  # Average per year
    mediana = median(Total_Michoacan, na.rm = TRUE),
    minimo = min(Total_Michoacan, na.rm = TRUE),
    maximo = max(Total_Michoacan, na.rm = TRUE),
    desviacion = sd(Total_Michoacan, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_michoacan)
# Create the tibble with the data for Michoacán
# Note: Truncated crime type names have been completed for clarity.
crime_data_michoacan <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo,
  "Robo",                            10, 147864, 14786, 14970, 11304, 18611,
  "Lesiones",                        10,  69829,  6983,  7480,  4330,  8305,
  "Amenazas",                        10,  33043,  3304,  3513,   447,  5269,
  "Daño a la propiedad",             10,  29699,  2970,  3098,  1217,  4291,
  "Otros delitos del Fuero Común",   10,  28708,  2871,  2806,  1454,  4024,
  "Homicidio",                       10,  25995,  2600,  2600,  2041,  3237,
  "Narcomenudeo",                    10,  19315,  1932,  2024,  1101,  2514,
  "Fraude",                          10,  17395,  1740,  1796,   512,  2780,
  "Violencia familiar",              10,  11828,  1183,  1184,   787,  1479,
  "Despojo",                         10,   7121,   712,   789,   328,   936
)

# Create the bar chart with text labels
ggplot(crime_data_michoacan, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "purple4") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Michoacán",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### GUANAJUATO ################################

desc_stats_guanajuato <- crime_monthly_by_year_type_Guanajuato %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Guanajuato, na.rm = TRUE),  # Total across all years
    media = mean(Total_Guanajuato, na.rm = TRUE),  # Average per year
    mediana = median(Total_Guanajuato, na.rm = TRUE),
    minimo = min(Total_Guanajuato, na.rm = TRUE),
    maximo = max(Total_Guanajuato, na.rm = TRUE),
    desviacion = sd(Total_Guanajuato, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_guanajuato)
# Create the tibble with the data for Guanajuato
# Note: Truncated crime type names have been completed for clarity.
crime_data_guanajuato <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 350791, 35079, 33028, 30424, 42982, 4964,
  "Otros delitos del Fuero Común",         10, 195004, 19500, 20076, 16827, 21826, 1816,
  "Narcomenudeo",                          10, 132844, 13284, 12186,  3060, 23976, 7945,
  "Lesiones",                              10, 130152, 13015, 13064, 10657, 16488, 1964,
  "Violencia familiar",                    10, 120218, 12022, 11198, 10035, 15958, 2042,
  "Daño a la propiedad",                   10, 109836, 10984, 11523,  8414, 13209, 1907,
  "Amenazas",                              10,  90608,  9061,  9497,  3752, 13045, 2956,
  "Homicidio",                             10,  36666,  3667,  3656,  2398,  4940,  791,
  "Fraude",                                10,  33258,  3326,  2968,  1981,  5166, 1184,
  "Incumplimiento de obligaciones",        10,  18430,  1843,  1926,  1045,  2761,  632
)

# Create the bar chart with text labels
ggplot(crime_data_guanajuato, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Guanajuato",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################################################################################
####################################################### ESTE: QUERETARO, EDOMEX, CDMX, MORELOS, HIDALGO, TLAXCALA Y PUEBLA #####################################
##########################################################################################################################################
#################################### QUERETARO ################################

desc_stats_queretaro <- crime_monthly_by_year_type_Queretaro %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Queretaro, na.rm = TRUE),  # Total across all years
    media = mean(Total_Queretaro, na.rm = TRUE),  # Average per year
    mediana = median(Total_Queretaro, na.rm = TRUE),
    minimo = min(Total_Queretaro, na.rm = TRUE),
    maximo = max(Total_Queretaro, na.rm = TRUE),
    desviacion = sd(Total_Queretaro, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_queretaro)

# Create the tibble with the data for Querétaro
# Note: Truncated crime type names have been completed for clarity.
crime_data_queretaro <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 233965, 23396, 22872, 17633, 27836, 3079,
  "Lesiones",                              10,  54845,  5484,  5718,  3345,  6662,  959,
  "Otros delitos del Fuero Común",         10,  39222,  3922,  4162,  1513,  5070, 1128,
  "Amenazas",                              10,  34379,  3438,  3858,  1108,  5072, 1249,
  "Violencia familiar",                    10,  31888,  3189,  3344,   942,  5865, 1898,
  "Fraude",                                10,  29263,  2926,  2622,  1486,  4684, 1175,
  "Daño a la propiedad",                   10,  27914,  2791,  1838,  1360,  5421, 1597,
  "Narcomenudeo",                          10,  11981,  1198,  1152,   224,  1783,  469,
  "Otros delitos que atentan contra la vida", 10,   9376,   938,   962,   626,  1333,  231,
  "Despojo",                               10,   7563,   756,   808,   483,   942,  172
)

# Create the bar chart with text labels
ggplot(crime_data_queretaro, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "slategray") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Querétaro",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### MORELOS ################################

desc_stats_morelos <- crime_monthly_by_year_type_Morelos %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Morelos, na.rm = TRUE),  # Total across all years
    media = mean(Total_Morelos, na.rm = TRUE),  # Average per year
    mediana = median(Total_Morelos, na.rm = TRUE),
    minimo = min(Total_Morelos, na.rm = TRUE),
    maximo = max(Total_Morelos, na.rm = TRUE),
    desviacion = sd(Total_Morelos, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_morelos)

# Create the tibble with the data for Querétaro
# Note: Truncated crime type names have been completed for clarity.
crime_data_morelos <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 166570, 166657, 15843, 14554, 20564, 2101,
  "Violencia familiar",                              10,  50403,  5043,  5011,  4034,  5877,  534,
  "Amenazas",         10,  44822,  4482,   4471 ,   3182  , 5620  ,    786,
  "Lesiones",                              10,  41594,  4159,   3947 ,   3451 ,  4962   ,   511,
  "Daño a la propiedad",                    10,  28138,  2814,   2725 ,   2018,   3561  ,    516,
  "Otros delitos del Fuero Común",                                10,  21719,  2172,   1896 ,   1609  , 5019  ,  1019,
  "Fraude",                   10,  18097 , 1810,   1766,   1521 , 2192  ,    205,
  "Homicidio",                          10,  11601 , 1160,   1098,   828  , 1627   ,   302,
  "Despojo", 10,   11198 , 1120,   1140,   957 ,  1243   ,    98.6,
  "Narcomenudeo",                               10,   7319 ,  732,    750,   493 ,   894   ,   136
)

# Create the bar chart with text labels
ggplot(crime_data_morelos, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "slategray") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Morelos",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### EDOMEX ################################

desc_stats_edomex <- crime_monthly_by_year_type_EdoMex %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_EdoMex, na.rm = TRUE),  # Total across all years
    media = mean(Total_EdoMex, na.rm = TRUE),  # Average per year
    mediana = median(Total_EdoMex, na.rm = TRUE),
    minimo = min(Total_EdoMex, na.rm = TRUE),
    maximo = max(Total_EdoMex, na.rm = TRUE),
    desviacion = sd(Total_EdoMex, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_edomex)

# Create the tibble with the data for Querétaro
# Note: Truncated crime type names have been completed for clarity.
# Create the tibble with the data for Estado de México
# Note: Truncated crime type names have been completed for clarity.
crime_data_estado_mexico <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                      10, 1470000, 147000, 144024, 122206, 168652, 16453,
  "Otros delitos del Fuero Común",             10,  684000,  68400,  68424,  40802,  90573, 13118,
  "Lesiones",                                  10,  585000,  58500,  58742,  52686,  63716,  3899,
  "Violencia familiar",                        10,  156000,  15600,  13082,   4722,  28650, 10159,
  "Daño a la propiedad",                       10,  135000,  13500,  12638,  10891,  16348,  1954,
  "Fraude",                                    10,  103000,  10300,  10018,   4693,  17753,  4938,
  "Despojo",                                   10,   41000,   4100,   4128,   2994,   4936,   780,
  "Otros delitos que atentan contra la vida",  10,   37000,   3700,   3005,   1048,   6743,  2025,
  "Abuso de confianza",                        10,   35400,   3540,   3447,   2425,   4712,   692,
  "Delitos cometidos por servidores públicos", 10,   34500,   3450,   3602,   2769,   4163,   553
)

# Create the bar chart with text labels
ggplot(crime_data_estado_mexico, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "maroon") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Estado de México",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### CDMX ################################
desc_stats_cdmx <- crime_monthly_by_year_type_CDMX %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_CDMX, na.rm = TRUE),  # Total across all years
    media = mean(Total_CDMX, na.rm = TRUE),  # Average per year
    mediana = median(Total_CDMX, na.rm = TRUE),
    minimo = min(Total_CDMX, na.rm = TRUE),
    maximo = max(Total_CDMX, na.rm = TRUE),
    desviacion = sd(Total_CDMX, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_cdmx)

# Create the tibble with the data for CDMX
# Note: Truncated crime type names have been completed for clarity.
crime_data_cdmx <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                      10, 872171, 87217, 78764, 67903, 123514, 18099,
  "Violencia familiar",                        10, 272065, 27206, 26770, 16103, 37578,  8681,
  "Fraude",                                    10, 165252, 16525, 15057, 11472, 23891,  4678,
  "Amenazas",                                  10, 143797, 14380, 14104,  9304, 20132,  4274,
  "Daño a la propiedad",                       10, 105909, 10591, 10874,  8796, 11585,   858,
  "Lesiones",                                  10, 103969, 10397, 10683,  7907, 11385,  1113,
  "Otros delitos del Fuero Común",             10,  84103,  8410,  9444,  4825, 10860,  2336,
  "Falsificación",                             10,  47514,  4751,  4172,  2324,  9033,  2442,
  "Delitos cometidos por servidores públicos", 10,  45128,  4513,  4976,  2916,  6161,  1063,
  "Narcomenudeo",                              10,  41623,  4162,  4647,  1405,  6097,  1723
)

# Create the bar chart with text labels
ggplot(crime_data_cdmx, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "mediumorchid") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en CDMX",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### HIDALGO ################################
desc_stats_hidalgo <- crime_monthly_by_year_type_Hidalgo %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Hidalgo, na.rm = TRUE),  # Total across all years
    media = mean(Total_Hidalgo, na.rm = TRUE),  # Average per year
    mediana = median(Total_Hidalgo, na.rm = TRUE),
    minimo = min(Total_Hidalgo, na.rm = TRUE),
    maximo = max(Total_Hidalgo, na.rm = TRUE),
    desviacion = sd(Total_Hidalgo, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_hidalgo)

# Create the tibble with the data for Hidalgo
# Note: Truncated crime type names have been completed for clarity.
crime_data_hidalgo <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                     10, 123956, 12396, 12022,  9866, 14873, 1732,
  "Otros delitos del Fuero Común",            10,  69238,  6924,  4930,  2150, 14771, 4775,
  "Lesiones",                                 10,  56394,  5639,  5888,  3693,  7138, 1009,
  "Violencia familiar",                       10,  53351,  5335,  5672,  2294,  7535, 1677,
  "Amenazas",                                 10,  30239,  3024,  3126,  1928,  4087,  668,
  "Daño a la propiedad",                      10,  27720,  2772,  2714,  1782,  3744,  591,
  "Otros delitos que atentan contra la vida", 10,  16426,  1643,  1758,   798,  1941,  332,
  "Fraude",                                   10,  14719,  1472,  1409,   898,  2003,  346,
  "Despojo",                                  10,   8806,   881,   838,   640,  1175,  189,
  "Incumplimiento de obligaciones",           10,   7975,   798,   824,   607,   914,  113
)

# Create the bar chart with text labels
ggplot(crime_data_hidalgo, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkkhaki") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Hidalgo",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### TLAXCALA ################################
desc_stats_tlaxcala <- crime_monthly_by_year_type_Tlaxcala %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Tlaxcala, na.rm = TRUE),  # Total across all years
    media = mean(Total_Tlaxcala, na.rm = TRUE),  # Average per year
    mediana = median(Total_Tlaxcala, na.rm = TRUE),
    minimo = min(Total_Tlaxcala, na.rm = TRUE),
    maximo = max(Total_Tlaxcala, na.rm = TRUE),
    desviacion = sd(Total_Tlaxcala, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_tlaxcala)

# Create the tibble with the data for Tlaxcala
# Note: Truncated crime type names have been completed for clarity.
crime_data_tlaxcala <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 36197, 3620,   3015,   2615, 5360, 1033,
  "Lesiones",                              10,  3384,  338,    309,    128,  952,  226,
  "Otros delitos del Fuero Común",         10,  3116,  312,    275,    141,  638,  147,
  "Homicidio",                             10,  2132,  213,    167,    141,  362,   85.4,
  "Daño a la propiedad",                   10,  1606,  161,    150,     36,  356,   93.0,
  "Narcomenudeo",                          10,  1163,  116,    104,     48,  218,   59.5,
  "Fraude",                                10,  1130,  113,     76,     11,  346,  107,
  "Incumplimiento de obligaciones",        10,   748,   74.8,   41.5,    0,  286,   91.4,
  "Violencia familiar",                    10,   618,   61.8,   24,      6,  275,   83.9,
  "Allanamiento de morada",                10,   485,   48.5,   51,     24,   71,   16.5
)

# Create the bar chart with text labels
ggplot(crime_data_tlaxcala, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "salmon") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Tlaxcala",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### PUEBLA ################################
desc_stats_puebla <- crime_monthly_by_year_type_Puebla %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Puebla, na.rm = TRUE),  # Total across all years
    media = mean(Total_Puebla, na.rm = TRUE),  # Average per year
    mediana = median(Total_Puebla, na.rm = TRUE),
    minimo = min(Total_Puebla, na.rm = TRUE),
    maximo = max(Total_Puebla, na.rm = TRUE),
    desviacion = sd(Total_Puebla, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_puebla)

# Create the tibble with the data for Puebla
# Note: Truncated crime type names have been completed for clarity.
crime_data_puebla <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 291254, 29125, 29890, 21691, 35887, 4470,
  "Violencia familiar",                    10,  78607,  7861,  8454,  5024, 10461, 1747,
  "Lesiones",                              10,  57791,  5779,  6272,  2643,  9068, 1903,
  "Amenazas",                              10,  39569,  3957,  4367,   549,  6029, 2093,
  "Fraude",                                10,  36100,  3610,  3690,  2302,  4815, 1012,
  "Daño a la propiedad",                   10,  30680,  3068,  3475,   796,  4366, 1333,
  "Otros delitos del Fuero Común",         10,  28310,  2831,  2488,  1809,  5195, 1131,
  "Homicidio",                             10,  14587,  1459,  1318,  1171,  1983,  300,
  "Narcomenudeo",                          10,  14269,  1427,  1504,   253,  3133,  946,
  "Despojo",                               10,  12872,  1287,  1339,   853,  1822,  319
)

# Create the bar chart with text labels
ggplot(crime_data_puebla, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Puebla",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##########################################################################################################################################
####################################################### ORIENTE: VERACRUZ Y TABASCO #####################################
##########################################################################################################################################
#################################### VERACRUZ ################################

desc_stats_veracruz <- crime_monthly_by_year_type_Veracruz %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Veracruz, na.rm = TRUE),  # Total across all years
    media = mean(Total_Veracruz, na.rm = TRUE),  # Average per year
    mediana = median(Total_Veracruz, na.rm = TRUE),
    minimo = min(Total_Veracruz, na.rm = TRUE),
    maximo = max(Total_Veracruz, na.rm = TRUE),
    desviacion = sd(Total_Veracruz, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_veracruz)

# Create the tibble with the data for Veracruz
# Note: Truncated crime type names have been completed for clarity.
crime_data_veracruz <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 216609, 21661, 21354, 16213, 29887, 4660,
  "Violencia familiar",                    10,  88168,  8817, 10355,  3572, 11796, 3226,
  "Lesiones",                              10,  76703,  7670,  8438,  4241,  9703, 1961,
  "Daño a la propiedad",                   10,  57758,  5776,  6510,  2811,  7374, 1675,
  "Amenazas",                              10,  53105,  5310,  6336,  1874,  7929, 2383,
  "Otros delitos del Fuero Común",         10,  40765,  4076,  4196,  1717,  6859, 1956,
  "Fraude",                                10,  30809,  3081,  3406,  1532,  4171,  954,
  "Homicidio",                             10,  20024,  2002,  1956,  1008,  2688,  470,
  "Despojo",                               10,  18013,  1801,  2072,   835,  2581,  609,
  "Incumplimiento de obligaciones",        10,  13099,  1310,  1302,   988,  1639,  194
)

# Create the bar chart with text labels
ggplot(crime_data_veracruz, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkslategray") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Veracruz",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################### TABASCO ################################

desc_stats_tabasco <- crime_monthly_by_year_type_Tabasco %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Tabasco, na.rm = TRUE),  # Total across all years
    media = mean(Total_Tabasco, na.rm = TRUE),  # Average per year
    mediana = median(Total_Tabasco, na.rm = TRUE),
    minimo = min(Total_Tabasco, na.rm = TRUE),
    maximo = max(Total_Tabasco, na.rm = TRUE),
    desviacion = sd(Total_Tabasco, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_tabasco)
# Create the tibble with the data for Tabasco
crime_data_tabasco <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                            10, 166658, 16666, 15526,  9375, 25469, 6477,
  "Otros delitos del Fuero Común",   10,  88127,  8813,  7511,  6690, 16665, 3060,
  "Violencia familiar",              10,  66359,  6636,  6906,  4230,  7791, 1099,
  "Lesiones",                       10,  53387,  5339,  5466,  4549,  6031,  520,
  "Amenazas",                       10,  40700,  4070,  4086,  3169,  4834,  497,
  "Daño a la propiedad",            10,  24514,  2451,  2509,  1969,  2975,  380,
  "Incumplimiento de obligaciones", 10,  13580,  1358,  1220,   855,  2060,  430,
  "Fraude",                         10,  11380,  1138,  1050,   900,  1411,  186,
  "Homicidio",                      10,   8563,   856,   834,   685,  1180,  163,
  "Abuso de confianza",             10,   6025,   602,   609,   353,   804,  143
)

# Create the bar chart with text labels
ggplot(crime_data_tabasco, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "chartreuse3") +
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Tabasco",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################################################################################
####################################################### SUR: GUERRERO, OAXACA Y CHIAPAS #####################################
##########################################################################################################################################
#################################### GUERRERO ################################
desc_stats_guerrero <- crime_monthly_by_year_type_Guerrero %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Guerrero, na.rm = TRUE),  # Total across all years
    media = mean(Total_Guerrero, na.rm = TRUE),  # Average per year
    mediana = median(Total_Guerrero, na.rm = TRUE),
    minimo = min(Total_Guerrero, na.rm = TRUE),
    maximo = max(Total_Guerrero, na.rm = TRUE),
    desviacion = sd(Total_Guerrero, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_guerrero)

# Create the tibble with the data for Guerrero
# Note: Truncated crime type names have been completed for clarity.
crime_data_guerrero <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 80140, 8014, 6882, 5672, 12600, 2607,
  "Otros delitos del Fuero Común",         10, 34758, 3476, 2278,  989,  8851, 3028,
  "Lesiones",                              10, 32087, 3209, 3362, 2362,  3674,  395,
  "Violencia familiar",                    10, 28755, 2876, 2915, 2022,  3472,  489,
  "Homicidio",                             10, 21392, 2139, 1927, 1469,  2868,  582,
  "Amenazas",                              10, 21263, 2126, 2154, 1226,  3196,  718,
  "Daño a la propiedad",                   10, 19679, 1968, 1973, 1703,  2290,  180,
  "Fraude",                                10,  7365,  736,  708,  603,   940,  120,
  "Narcomenudeo",                          10,  6198,  620,  680,  281,  1073,  254,
  "Incumplimiento de obligaciones",        10,  5763,  576,  532,  358,  1069,  204
)

# Create the bar chart with text labels
ggplot(crime_data_guerrero, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "indianred") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Guerrero",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### OAXACA ################################
desc_stats_oaxaca <- crime_monthly_by_year_type_Oaxaca %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Oaxaca, na.rm = TRUE),  # Total across all years
    media = mean(Total_Oaxaca, na.rm = TRUE),  # Average per year
    mediana = median(Total_Oaxaca, na.rm = TRUE),
    minimo = min(Total_Oaxaca, na.rm = TRUE),
    maximo = max(Total_Oaxaca, na.rm = TRUE),
    desviacion = sd(Total_Oaxaca, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_oaxaca)

# Create the tibble with the data for Oaxaca
# Note: Truncated crime type names have been completed for clarity.
crime_data_oaxaca <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 97243, 9724, 10497,  1737, 13153, 3288,
  "Violencia familiar",                    10, 53878, 5388,  6066,   618,  7428, 2109,
  "Lesiones",                              10, 45543, 4554,  4778,   744,  5924, 1432,
  "Amenazas",                              10, 31825, 3182,  4116,   197,  4906, 1664,
  "Daño a la propiedad",                   10, 23496, 2350,  2628,   442,  2823,  723,
  "Homicidio",                             10, 16725, 1672,  1732,   416,  2203,  484,
  "Otros delitos del Fuero Común",         10, 16068, 1607,  1684,   705,  2327,  464,
  "Fraude",                                10, 13503, 1350,  1506,   150,  1941,  610,
  "Otros delitos contra el patrimonio",    10,  8580,  858,   614,   283,  1932,  576,
  "Despojo",                               10,  6736,  674,   758,   116,  1039,  273
)

# Create the bar chart with text labels
ggplot(crime_data_oaxaca, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "chocolate") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Oaxaca",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### CHIAPAS ################################
desc_stats_chiapas <- crime_monthly_by_year_type_Chiapas %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Chiapas, na.rm = TRUE),  # Total across all years
    media = mean(Total_Chiapas, na.rm = TRUE),  # Average per year
    mediana = median(Total_Chiapas, na.rm = TRUE),
    minimo = min(Total_Chiapas, na.rm = TRUE),
    maximo = max(Total_Chiapas, na.rm = TRUE),
    desviacion = sd(Total_Chiapas, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_chiapas)
# Create the tibble with the data for Chiapas
# Note: Truncated crime type names have been completed for clarity.
crime_data_chiapas <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                     10, 55326, 5533, 4920, 2316, 9336, 3111,
  "Violencia familiar",                       10, 29899, 2990, 2735,  845, 6032, 1868,
  "Lesiones",                                 10, 16642, 1664, 1554, 1186, 2263,  336,
  "Otros delitos del Fuero Común",            10, 15377, 1538, 1256,  885, 3362,  741,
  "Daño a la propiedad",                      10, 13324, 1332, 1332,  835, 1702,  278,
  "Homicidio",                                10, 12906, 1291, 1278, 1065, 1552,  161,
  "Amenazas",                                 10,  9180,  918,  530,   77, 2472,  807,
  "Narcomenudeo",                             10,  8769,  877,  788,  255, 1893,  508,
  "Violación simple",                         10,  4866,  487,  527,  220,  717,  149,
  "Otros delitos que atentan contra la vida", 10,  4745,  474,  534,   52,  754,  257
)

# Create the bar chart with text labels
ggplot(crime_data_chiapas, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Chiapas",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##########################################################################################################################################
####################################################### SURESTE: YUCATAN, CAMPECHE Y QUINTANA ROO #####################################
##########################################################################################################################################
#################################### YUCATAN ################################
desc_stats_yucatan <- crime_monthly_by_year_type_Yucatan %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Yucatan, na.rm = TRUE),  # Total across all years
    media = mean(Total_Yucatan, na.rm = TRUE),  # Average per year
    mediana = median(Total_Yucatan, na.rm = TRUE),
    minimo = min(Total_Yucatan, na.rm = TRUE),
    maximo = max(Total_Yucatan, na.rm = TRUE),
    desviacion = sd(Total_Yucatan, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_yucatan)

# Create the tibble with the data for Yucatán
# Note: Truncated crime type names have been completed for clarity.
crime_data_yucatan <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Amenazas",                                10, 28001, 2800, 2016,  668, 6619, 2085,
  "Otros delitos del Fuero Común",           10, 22615, 2262, 1888,  367, 6569, 2031,
  "Daño a la propiedad",                     10, 17703, 1770, 1362,  637, 4690, 1317,
  "Robo",                                    10, 15893, 1589, 1400,  352, 3625, 1228,
  "Violencia familiar",                      10, 12557, 1256, 1090,  125, 2670,  945,
  "Lesiones",                                10, 11617, 1162,  362,  205, 4048, 1504,
  "Fraude",                                  10,  6733,  673,  519,   86, 1742,  590,
  "Otros delitos contra el patrimonio",    10,  5750,  575,  479,   13, 1492,  549,
  "Abuso de confianza",                      10,  5242,  524,  405,   77, 1408,  454,
  "Incumplimiento de obligaciones",        10,  4820,  482,  286,  110, 1212,  445
)

# Create the bar chart with text labels
ggplot(crime_data_yucatan, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Yucatán",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### CAMPECHE ################################
desc_stats_campeche <- crime_monthly_by_year_type_Campeche %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_Campeche, na.rm = TRUE),  # Total across all years
    media = mean(Total_Campeche, na.rm = TRUE),  # Average per year
    mediana = median(Total_Campeche, na.rm = TRUE),
    minimo = min(Total_Campeche, na.rm = TRUE),
    maximo = max(Total_Campeche, na.rm = TRUE),
    desviacion = sd(Total_Campeche, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_campeche)

# Create the tibble with the data for Campeche
# Note: Truncated crime type names have been completed for clarity.
crime_data_campeche <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 22336, 2234, 1077,    858, 5999, 2061,
  "Lesiones",                              10, 13361, 1336,  187,    137, 4514, 1824,
  "Daño a la propiedad",                   10,  8696,  870,  225,    121, 3049, 1163,
  "Amenazas",                              10,  7893,  789,   65.5,   20, 3220, 1249,
  "Violencia familiar",                    10,  5893,  589,   63.5,   18, 2151,  850,
  "Otros delitos del Fuero Común",         10,  4071,  407,   76,     52, 1479,  552,
  "Fraude",                                10,  2608,  261,   10.5,    5, 1074,  415,
  "Otros delitos contra el patrimonio",    10,  1745,  174,   42,     16,  628,  226,
  "Homicidio",                             10,  1626,  163,  161,    118,  216,   34.5,
  "Narcomenudeo",                          10,  1584,  158,  130,     58,  350,   95.1
)

# Create the bar chart with text labels
ggplot(crime_data_campeche, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Campeche",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################### QUINTANA ROO ################################

desc_stats_qroo <- crime_monthly_by_year_type_QRoo %>%
  group_by(Tipo.de.delito) %>%
  summarise(
    count_years = n(),
    suma = sum(Total_QRoo, na.rm = TRUE),  # Total across all years
    media = mean(Total_QRoo, na.rm = TRUE),  # Average per year
    mediana = median(Total_QRoo, na.rm = TRUE),
    minimo = min(Total_QRoo, na.rm = TRUE),
    maximo = max(Total_QRoo, na.rm = TRUE),
    desviacion = sd(Total_QRoo, na.rm = TRUE)
  ) %>%
  arrange(desc(suma))

print(desc_stats_qroo)
# Create the tibble with the data for Quintana Roo
# Note: Truncated crime type names have been completed for clarity.
crime_data_quintana_roo <- tribble(
  ~Tipo.de.delito, ~count_years, ~suma, ~media, ~mediana, ~minimo, ~maximo, ~desviacion,
  "Robo",                                  10, 138530, 13853, 14376,  7102, 20050, 3339,
  "Violencia familiar",                    10,  47510,  4751,  5160,  1524,  7195, 1886,
  "Daño a la propiedad",                   10,  41722,  4172,  3676,  2839,  6046, 1149,
  "Lesiones",                              10,  35608,  3561,  3460,  1866,  5139, 1117,
  "Amenazas",                              10,  21014,  2101,  2204,   549,  3636, 1138,
  "Abuso de confianza",                    10,  18503,  1850,  1339,   322,  4165, 1525,
  "Otros delitos del Fuero Común",         10,  14291,  1429,  1338,   664,  2174,  552,
  "Narcomenudeo",                          10,  11866,  1187,  1028,   333,  2476,  760,
  "Homicidio",                             10,  11195,  1120,  1418,   278,  1596,  501,
  "Fraude",                                10,   7659,   766,   562,   181,  1741,  560
)

# Create the bar chart with text labels
ggplot(crime_data_quintana_roo, aes(x = reorder(Tipo.de.delito, -media), y = media)) +
  geom_bar(stat = "identity", fill = "mediumseagreen") +
  # Adds the numerical labels on top of each bar
  geom_text(aes(label = media), vjust = -0.3, size = 3.5) +
  labs(
    title = "Promedio anual de conteo de delitos en Quintana Roo",
    x = "Tipo de Delito",
    y = "Media (Average)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load the Mexican states GeoJSON file
mx_states <- st_read("C:/Users/emman/OneDrive/Documents/EstadisticaGerardo/StatisticsPRoject/mx.json")

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
  labs(title = "Regiones Econimicas de Mexico", fill = "Region") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Vector de estados del Noroeste, ya definido anteriormente
noroeste_estados <- c("Baja California", "Sonora", "Sinaloa", "Nayarit", "Baja California Sur")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_noroeste <- mx_states %>% dplyr::filter(name %in% noroeste_estados)

# Crear el mapa interactivo
leaflet(mx_noroeste) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#337ab7",
    weight = 2,
    opacity = 1,
    fillColor = "#1f78b4",
    fillOpacity = 0.5,
    label = ~name
  )

# Vector de estados del Noroeste, ya definido anteriormente
norte_estados <- c("Coahuila", "Durango", "Chihuahua", "Zacatecas", "San Luis Potosí")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_norte <- mx_states %>% dplyr::filter(name %in% norte_estados)

# Crear el mapa interactivo
leaflet(mx_norte) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Red",
    weight = 2,
    opacity = 1,
    fillColor = "Red",
    fillOpacity = 0.5,
    label = ~name
  )

noreste_estados <- c("Nuevo León", "Tamaulipas")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_noreste <- mx_states %>% dplyr::filter(name %in% noreste_estados)

# Crear el mapa interactivo
leaflet(mx_noreste) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Purple",
    weight = 2,
    opacity = 1,
    fillColor = "Purple",
    fillOpacity = 0.5,
    label = ~name
  )

Centro_Occidente_estados <- c("Colima","Jalisco", "Aguascalientes", "Michoacán", "Guanajuato")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_Centro_Occidente <- mx_states %>% dplyr::filter(name %in% Centro_Occidente_estados)

# Crear el mapa interactivo
leaflet(mx_Centro_Occidente) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Blue",
    weight = 2,
    opacity = 1,
    fillColor = "Blue",
    fillOpacity = 0.5,
    label = ~name
  )

Centro_Sur_estados <- c("Querétaro", "México", "Hidalgo", "Tlaxcala", "Puebla", "Ciudad de México", "Puebla", "Morelos" )

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_Centro_Sur <- mx_states %>% dplyr::filter(name %in% Centro_Sur_estados)

# Crear el mapa interactivo
leaflet(mx_Centro_Sur) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Blue",
    weight = 2,
    opacity = 1,
    fillColor = "Blue",
    fillOpacity = 0.5,
    label = ~name
  )

Golfo_de_Mexico_estados <- c("Veracruz", "Tabasco")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_Golfo_de_Mexico <- mx_states %>% dplyr::filter(name %in% Golfo_de_Mexico_estados)

# Crear el mapa interactivo
leaflet(mx_Golfo_de_Mexico) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Green",
    weight = 2,
    opacity = 1,
    fillColor = "Green",
    fillOpacity = 0.5,
    label = ~name
  )

Pacifico_sur_estados <- c("Guerrero", "Oaxaca", "Chiapas")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_Pacifico_sur <- mx_states %>% dplyr::filter(name %in% Pacifico_sur_estados)

# Crear el mapa interactivo
leaflet(mx_Pacifico_sur) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "Gold",
    weight = 2,
    opacity = 1,
    fillColor = "Gold",
    fillOpacity = 0.5,
    label = ~name
  )



Peninsula_de_Yucatan_estados <- c("Yucatán", "Campeche", "Quintana Roo")

# Filtrar los estados (suponiendo que el campo es name y el objeto mx_states ya existe)
mx_Peninsula_de_Yucatan <- mx_states %>% dplyr::filter(name %in% Peninsula_de_Yucatan_estados)

# Crear el mapa interactivo
leaflet(mx_Peninsula_de_Yucatan) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#76821C",
    weight = 2,
    opacity = 1,
    fillColor = "#76821C",
    fillOpacity = 0.5,
    label = ~name
)
