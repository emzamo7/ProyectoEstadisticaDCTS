# Region Occidente
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
  slice_head(n = 10)

# View the result
print(top_crimes_Aguascalientes)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  mutate(Total_Aguascalientes = rowSums(across(all_of(months_Aguascalientes))))

# Find top 3 crimes per year
top_crimes_per_year_Aguascalientes <- crime_monthly_by_year_type_Aguascalientes %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Aguascalientes, n = 10, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Aguascalientes))
write.csv(top_crimes_per_year_Aguascalientes, "top_crimes_per_year_Aguascalientes.csv", row.names = FALSE)
# View the result
print(top_crimes_per_year_Aguascalientes, n = 100)
View(top_crimes_per_year_Aguascalientes)
#unique(df$Entidad)

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