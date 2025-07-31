# Oriente - Golfo de Mexico
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(tibble)
library(leaflet)
library(tidyverse)

df <- read.csv("C:/Users/emman/Documents/StatisticsPRoject/IDM_NM_jun25.csv", fileEncoding = "latin1")
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
states_of_interest <- c("CAMPECHE","YUCATÁN","QUINTANA ROO")
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

# Filter for 'Violencia Familiar' only
violencia_familiar_Ver <- crime_monthly_by_year_type_Veracruz %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Veracruz) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Ver)

bc_vfVer <- tibble::tibble(
  Año = 2015:2024,
  Total_Ver = c(3572,4741,6326,6219,30324,10386,11706,11747,11796,11321)
)

# Población 2020 según tus datos
Poblacion_Ver_2024 <- 6355487 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenVer <- bc_vfVer %>%
  summarise(
    anios = n(),
    suma = sum(Total_Ver),
    media = mean(Total_Ver),
    mediana = median(Total_Ver),
    minimo = min(Total_Ver),
    maximo = max(Total_Ver),
    desviacion = sd(Total_Ver)
  )

print(resumenVer)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenVer$media / Poblacion_Ver_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Veracruz:", round(tasa_100k, 2), "\n")


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

# Filter for 'Violencia Familiar' only
violencia_familiar_Tab <- crime_monthly_by_year_type_Tabasco %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Tabasco) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Tab)

bc_vfTab <- tibble::tibble(
  Año = 2015:2024,
  Total_Tab = c(4230,5406,6596,6448,7215,6445,7791,7426,7514,7288)
)

# Población 2020 según tus datos
Poblacion_Tab_2024 <- 1826569 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenTab <- bc_vfTab %>%
  summarise(
    anios = n(),
    suma = sum(Total_Tab),
    media = mean(Total_Tab),
    mediana = median(Total_Tab),
    minimo = min(Total_Tab),
    maximo = max(Total_Tab),
    desviacion = sd(Total_Tab)
  )

print(resumenTab)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenTab$media / Poblacion_Tab_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Tabasco:", round(tasa_100k, 2), "\n")

#####################################  ANOVA  ###################################
##############################################################################
# Datos ejemplo ajustados a tu caso
df_longGlf <- tibble::tibble(
  Ano = rep(2015:2024, 2),
  Estado = rep(c("Tabasco", "Veracruz"), each = 10),
  Total = c(
    4230,5406,6596,6448,7215,6445,7791,7426,7514,7288,        # Tabasco
    3572,4741,6326,6219,30324,10386,11706,11747,11796,11321              # Veracruz
    )
)
# ANOVA de 1 factor
anova_resultGlf <- aov(Total ~ Estado, data = df_longGlf)
summary(anova_resultGlf)
# TUKEY
tukey_resultGlf <- TukeyHSD(anova_resultGlf)
print(tukey_resultGlf)

# Visual (opcional)
#plot(tukey_resultSE)


###################################IC#####################

# Tabasco:
media <- 6636      # Media anual de Tab
sd <- 1099          # Desviación estándar anual de Tab
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Tabasco: [%.1f, %.1f]\n", lower, upper))


# # Veracruz:
media <- 10814      # Media anual de Camp
sd <- 7559          # Desviación estándar anual de Camp
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Veracruz: [%.1f, %.1f]\n", lower, upper))






