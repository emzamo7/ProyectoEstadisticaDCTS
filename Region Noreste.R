#Region Noreste

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
states_of_interest <- c("TAMAULIPAS", "NUEVO LEÓN")




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


# Filter for 'Violencia Familiar' only
violencia_familiar_Tamps <- crime_monthly_by_year_type_Tamaulipas %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Tamaulipas) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Tamps)

bc_vfTamps <- tibble::tibble(
  Año = 2015:2024,
  Total_Tamps = c(2394,6378,7030,7029,7509,6467,7844,7863,8785,8331)
)

# Población 2020 según tus datos
Poblacion_Tamps_2024 <- 2781998  # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenTamps <- bc_vfTamps %>%
  summarise(
    anios = n(),
    suma = sum(Total_Tamps),
    media = mean(Total_Tamps),
    mediana = median(Total_Tamps),
    minimo = min(Total_Tamps),
    maximo = max(Total_Tamps),
    desviacion = sd(Total_Tamps)
  )

print(resumenTamps)






# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenTamps$media / Poblacion_Tamps_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Tamaulipas:", round(tasa_100k, 2), "\n")






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



# Filter for 'Violencia Familiar' only
violencia_familiar_NL <- crime_monthly_by_year_type_NL %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_NL) %>%
  arrange(Año)

# View the result
print(violencia_familiar_NL)

bc_vfNL <- tibble::tibble(
  Año = 2015:2024,
  Total_NL = c(17062,17773,17145,16410,16339,17940,21029,22480,20320,19184)
)

# Población 2020 según tus datos
Poblacion_NL_2024 <- 4885985  # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenNL <- bc_vfNL %>%
  summarise(
    anios = n(),
    suma = sum(Total_NL),
    media = mean(Total_NL),
    mediana = median(Total_NL),
    minimo = min(Total_NL),
    maximo = max(Total_NL),
    desviacion = sd(Total_NL)
  )

print(resumenNL)






# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100kNL <- (resumenNL$media / Poblacion_NL_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Tamaulipas:", round(tasa_100kNL, 2), "\n")



#################################################################################33

#####################################  ANOVA  ###################################
##############################################################################
# Datos ejemplo ajustados a tu caso
df_longNE <- tibble::tibble(
  Año = rep(2015:2024, 2),
  Estado = rep(c("Tamaulipas", "Nuevo León"), each = 10),
  Total = c(
    2394,6378,7030,7029,7509,6467,7844,7863,8785,8331,        # Tamaulipas
    17062,17773,17145,16410,16339,17940,21029,22480,20320,19184              # Nuevo León 
  )
)
# ANOVA de 1 factor
anova_resultNE <- aov(Total ~ Estado, data = df_longNE)
summary(anova_resultNE)
# TUKEY
tukey_resultNE <- TukeyHSD(anova_resultNE)
print(tukey_resultNE)

# Visual (opcional)
#plot(tukey_resultSE)


###################################IC#####################

# Tamaulipas:
media <- 6963      # Media anual de Tab
sd <- 1781          # Desviación estándar anual de Tab
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Tamaulipas: [%.1f, %.1f]\n", lower, upper))


# # Nuevo León:
media <- 18568      # Media anual de Camp
sd <- 2102          # Desviación estándar anual de Camp
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para NL: [%.1f, %.1f]\n", lower, upper))



#################################################################################33
