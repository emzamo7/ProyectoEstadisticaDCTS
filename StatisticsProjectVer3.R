library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)


df <- read.csv("C:/Users/emman/OneDrive/Documents/EstadisticaGerardo/IDM_NM_may25.csv", fileEncoding = "latin1")
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

# REDUJE EL DATASET A 17 variables
############################### AGUASCALIENTES 2015-2024 ######################


years_to_include <- 2015:2024  # creates a vector of years from 2015 to 2024
filtered_df_Aguascalientes <- df2[df2$Año %in% years_to_include & df2$Entidad == "AGUASCALIENTES", ]
filtered_df_Aguascalientes
View(filtered_df_Aguascalientes)

# Create a vector of the month columns
months_Aguascalientes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Sum all month values per year
crime_totals_Aguascalientes <- aggregate(. ~ Año, data = filtered_df[, c("Año", months_Aguascalientes)], sum)
crime_totals_Aguascalientes
# Optional: Add a "Total" column summing across all months
crime_totals_Aguascalientes$Total_Aguascalientes <- rowSums(crime_totals_Aguascalientes[, months_Aguascalientes])


crime_totals_Aguascalientes$Total_Aguascalientes #2015 - 2014

# Define the month columns
months_Aguascalientes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
years_to_include <- 2015:2024
filtered_df_Aguascalientes <- df2 %>%
  filter(Año %in% years_to_include, Entidad == "AGUASCALIENTES")
# Group by crime type and sum each month
crime_monthly_by_year_type_Aguascalientes <- filtered_df_Aguascalientes %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Aguascalientes), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Aguascalientes)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  mutate(Total_Aguascalientes = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024  # creates a vector of years from 2015 to 2024


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
crime_totals_BC$Total_BC <- rowSums(crime_totals_BC[, months])


crime_totals_BC$Total_BC #2015 - 2014

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
  mutate(Total_BC = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_BCS = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_Campeche = rowSums(across(all_of(months))))

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
View(top_crimes_per_year_Campeche)
############################### COAHUILA 2015-2024 ######################
months_Coahuila <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
years_to_include <- 2015:2024
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
  mutate(Total_Coahuila = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_Colima = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_Chiapas = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_Chihuahua = rowSums(across(all_of(months))))

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
  mutate(Total_CDMX = rowSums(across(all_of(months))))

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
  mutate(Total_Durango = rowSums(across(all_of(months))))

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
  mutate(Total_Guanajuato = rowSums(across(all_of(months))))

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
  mutate(Total_Guerrero = rowSums(across(all_of(months))))

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
  mutate(Total_Hidalgo = rowSums(across(all_of(months))))

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
  mutate(Total_Jalisco = rowSums(across(all_of(months))))

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
  mutate(Total_EdoMex = rowSums(across(all_of(months))))

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
  mutate(Total_Michoacan = rowSums(across(all_of(months))))

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
  mutate(Total_Nayarit = rowSums(across(all_of(months))))

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
  mutate(Total_NL = rowSums(across(all_of(months))))

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
  mutate(Total_Oaxaca = rowSums(across(all_of(months))))

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
  mutate(Total_Puebla = rowSums(across(all_of(months))))

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
  mutate(Total_Queretaro = rowSums(across(all_of(months))))

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
  mutate(Total_QRoo = rowSums(across(all_of(months))))

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
  mutate(Total_SLP = rowSums(across(all_of(months))))

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
  mutate(Total_Sinaloa = rowSums(across(all_of(months))))

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
years_to_include <- 2015:2024
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
  mutate(Total_Sonora = rowSums(across(all_of(months))))

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

############################### TAMAULIPAS 2015-2024 ######################

############################### TLAXCALA 2015-2024 ######################

############################### VERACRUZ 2015-2024 ######################

############################### YUCATÁN 2015-2024 ######################

############################### ZACATECAS 2015-2024 ######################