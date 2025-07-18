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
  summarise(Total_Coahuila = sum(Total_Coahuila) %>%
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
  slice_max(order_by = Total_BCS, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Coahuila))

# View the result
print(top_crimes_per_year_Coahuila, n = 30)
View(top_crimes_per_year_Coahuila)
############################### COLIMA 2015-2024 ######################

############################### CHIAPAS 2015-2024 ######################

############################### CHIHUAHUA 2015-2024 ######################

############################### CIUDAD DE MÉXICO 2015-2024 ######################

############################### DURANGO 2015-2024 ######################

############################### GUANAJUATO 2015-2024 ######################

############################### GUERRERO 2015-2024 ######################

############################### HIDALGO 2015-2024 ######################

############################### JALISCO 2015-2024 ######################

############################### MÉXICO 2015-2024 ######################

############################### MICHOACÁN 2015-2024 ######################

############################### NAYARIT 2015-2024 ######################

############################### NUEVO LEÓN 2015-2024 ######################

############################### OAXACA 2015-2024 ######################

############################### PUEBLA 2015-2024 ######################

############################### QUERÉTARO 2015-2024 ######################

############################### QUINTANA ROO 2015-2024 ######################

############################### SAN LUIS POTOSÍ 2015-2024 ######################

############################### SINALOA 2015-2024 ######################

############################### SONORA 2015-2024 ######################

############################### TABASCO 2015-2024 ######################

############################### TAMAULIPAS 2015-2024 ######################

############################### TLAXCALA 2015-2024 ######################

############################### VERACRUZ 2015-2024 ######################

############################### YUCATÁN 2015-2024 ######################

############################### ZACATECAS 2015-2024 ######################