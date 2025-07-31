# Region Norte

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