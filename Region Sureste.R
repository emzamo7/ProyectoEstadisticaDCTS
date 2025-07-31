# Region Sureste
# Region Sureste Peninsula de Yucatan
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
##################################################################################
states_of_interest <- c("CAMPECHE","YUCATÁN","QUINTANA ROO")

# Filter data: Only records for selected states, years, and crime
filtered_data <- df2 %>%
  filter(Año %in% years_to_include,
         Entidad %in% states_of_interest,
         Tipo.de.delito == "Violencia familiar")

# Sum over all months for each year and state
filtered_summary <- filtered_data %>%
  mutate(Total = Enero + Febrero + Marzo + Abril + Mayo + Junio + 
           Julio + Agosto + Septiembre + Octubre + Noviembre + Diciembre) %>%
  group_by(Año, Entidad) %>%
  summarise(Annual_Count = sum(Total, na.rm = TRUE), .groups = "drop")

# Plot the annual trend as a line graph
ggplot(filtered_summary, aes(x = Año, y = Annual_Count, color = Entidad)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 2015:2024) +
  geom_point(size = 2) +
  labs(title = "Evolución anual de Violencia Familiar (2015-2024)",
       x = "Año",
       y = "Casos anuales",
       color = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################### YUCATÁN 2015-2024 ######################
################################################################################
############################### YUCATAN ######################
################################################################################
filtered_df_Yuc <- df2[df2$Año %in% years_to_include & df2$Entidad == "YUCATÁN", ]
filtered_df_Yuc
View(filtered_df_Yuc)

# Create a vector of the month columns
months_Yuc <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Sum all month values per year
crime_totals_Yuc <- aggregate(. ~ Año, data = filtered_df_Yuc[, c("Año", months_Yuc)], sum)
crime_totals_Yuc
# Optional: Add a "Total" column summing across all months
#rime_totals_BC$Total_Yuc <- rowSums(crime_totals_BC[, months])


#crime_totals_BC$Total_BC #2015 - 2014

# Define the month columns
months_Yuc <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#years_to_include <- 2015:2024
filtered_df_Yuc<- df2 %>%
  filter(Año %in% years_to_include, Entidad == "YUCATÁN")
# Group by crime type and sum each month
crime_monthly_by_year_type_Yuc <- filtered_df_Yuc %>%
  group_by(Año, Tipo.de.delito) %>%
  summarise(across(all_of(months_Yuc), ~sum(.x, na.rm = TRUE)), .groups = "drop")

# View the full table
print(crime_monthly_by_year_type_Yuc)
### TOTAL
# Sum across all months to get total crimes per year & type
crime_monthly_by_year_type_Yuc <- crime_monthly_by_year_type_Yuc %>%
  mutate(Total_Yuc= rowSums(across(all_of(months_Yuc))))

# Group by crime type and sum all years to get overall totals
top_crimes_Yuc <- crime_monthly_by_year_type_Yuc %>%
  group_by(Tipo.de.delito) %>%
  summarise(Total_Yuc = sum(Total_Yuc)) %>%
  arrange(desc(Total_Yuc)) %>%
  slice_head(n = 3)

# View the result
print(top_crimes_Yuc)

### PER EACH YEAR

# Create a Total column across all months per row
crime_monthly_by_year_type_Yuc <- crime_monthly_by_year_type_Yuc %>%
  mutate(Total_Yuc = rowSums(across(all_of(months_Yuc))))

# Find top 3 crimes per year
top_crimes_per_year_Yuc <- crime_monthly_by_year_type_Yuc %>%
  group_by(Año) %>%
  slice_max(order_by = Total_Yuc, n = 3, with_ties = FALSE) %>%
  arrange(Año, desc(Total_Yuc))
write.csv(top_crimes_per_year_Yuc, "top_crimes_per_year_yUC.csv", row.names = FALSE)
# View the result
print(top_crimes_per_year_Yuc, n = 30)
View(top_crimes_per_year_Yuc)
#unique(df$Entidad)
# Filter for 'Violencia Familiar' only
violencia_familiar_Yuc <- crime_monthly_by_year_type_Yuc %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Yuc) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Yuc)

bc_vfYuc <- tibble::tibble(
  Año = 2015:2024,
  Total_Yuc = c(2158, 2270, 2670, 1935, 1295, 726, 884, 125, 193, 301)
)

# Población 2020 según tus datos
Poblacion_Yuc_2024 <- 1932179 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenYuc <- bc_vfYuc %>%
  summarise(
    anios = n(),
    suma = sum(Total_Yuc),
    media = mean(Total_Yuc),
    mediana = median(Total_Yuc),
    minimo = min(Total_Yuc),
    maximo = max(Total_Yuc),
    desviacion = sd(Total_Yuc)
  )

print(resumenYuc)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenYuc$media / Poblacion_Yuc_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Yucatan:", round(tasa_100k, 2), "\n")

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
View(top_crimes_per_year_Campeche) 

# Filter for 'Violencia Familiar' only
violencia_familiar_Camp <- crime_monthly_by_year_type_Campeche %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_Campeche) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Camp)

bc_vfCamp <- tibble::tibble(
  Año = 2015:2024,
  Total_Camp = c(18, 43, 65, 52, 62, 45, 314, 1957, 2151, 1186)
)

# Población 2020 según tus datos
Poblacion_Camp_2024 <- 732531 # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenCamp <- bc_vfCamp %>%
  summarise(
    anios = n(),
    suma = sum(Total_Camp),
    media = mean(Total_Camp),
    mediana = median(Total_Camp),
    minimo = min(Total_Camp),
    maximo = max(Total_Camp),
    desviacion = sd(Total_Camp)
  )

print(resumenCamp)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenCamp$media / Poblacion_Camp_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en Campeche:", round(tasa_100k, 2), "\n")


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

# Filter for 'Violencia Familiar' only
violencia_familiar_Qroo <- crime_monthly_by_year_type_QRoo %>%
  filter(Tipo.de.delito == "Violencia familiar") %>%
  select(Año, Total_QRoo) %>%
  arrange(Año)

# View the result
print(violencia_familiar_Qroo)

bc_vfQroo <- tibble::tibble(
  Año = 2015:2024,
  Total_Qroo = c(3058, 1524, 2633, 4025, 5508, 4813, 5848, 6161, 6745, 7195)
)

# Población 2020 según tus datos
Poblacion_Qroo_2024 <- 1639970  # PROYECCION DE LA POBLACION USANDO LA FORMULA DE CRECIMIENTO EXPONENCIAL

# Summarise: estadísticas descriptivas
resumenQroo <- bc_vfQroo %>%
  summarise(
    anios = n(),
    suma = sum(Total_Qroo),
    media = mean(Total_Qroo),
    mediana = median(Total_Qroo),
    minimo = min(Total_Qroo),
    maximo = max(Total_Qroo),
    desviacion = sd(Total_Qroo)
  )

print(resumenQroo)

# Cálculo de tasa por cada 100,000 habitantes (media anual)
tasa_100k <- (resumenQroo$media / Poblacion_Qroo_2024) * 100000
cat("Tasa anual promedio de Violencia Familiar por 100,000 habitantes en QRoo:", round(tasa_100k, 2), "\n")

##############################################################################3
#####################################  ANOVA  ###################################
##############################################################################
# Datos ejemplo ajustados a tu caso
df_longSE <- tibble::tibble(
  Año = rep(2015:2024, 3),
  Estado = rep(c("Quintana Roo","Campeche","Yucatán"), each = 10),
  Total = c(
    3058, 1524, 2633, 4025, 5508, 4813, 5848, 6161, 6745, 7195,        # Quintana Roo
    18, 43, 65, 52, 62, 45, 314, 1957, 2151, 1186,              # Campeche
    2158, 2270, 2670, 1935, 1295, 726, 884, 125, 193, 301                    # Yucatan
      )
)
# ANOVA de 1 factor
anova_resultSE <- aov(Total ~ Estado, data = df_longSE)
summary(anova_resultSE)
# TUKEY
tukey_resultSE <- TukeyHSD(anova_resultSE)
print(tukey_resultSE)

# Visual (opcional)
#plot(tukey_resultSE)

###################################IC#####################

# QRoo:
media <- 4751      # Media anual de QRoo
sd <- 1886          # Desviación estándar anual de Qroo
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para QRoo: [%.1f, %.1f]\n", lower, upper))


# # Campeche:
media <- 589      # Media anual de Camp
sd <- 850          # Desviación estándar anual de Camp
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Camp: [%.1f, %.1f]\n", lower, upper))

# # Yucatan:
media <- 1256      # Media anual de Yucatan
sd <- 945          # Desviación estándar anual de Yucatan
n <- 10             # Número de años

t_crit <- qt(0.975, df = n-1)       # t* para 95% y 9 g.l.
se <- sd / sqrt(n)                  # error estándar
lower <- media - t_crit * se
upper <- media + t_crit * se

cat(sprintf("IC 95%% para Yucatan: [%.1f, %.1f]\n", lower, upper))

#################################################################################33


