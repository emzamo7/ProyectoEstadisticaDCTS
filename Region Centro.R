
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
# REDUJE EL DATASET A 17 variables <----------------------------------------






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






