

install.packages("readxl")
library(readxl)
# Leer un archivo .xls
datos <- read_excel("D:/StatisticsPRoject/Poblacion_01.xlsx")

#Haciendo proyeccion de poblacion para el año 2024
#Usando la formula de crecimiento exponencial
#P_t = P_0 x (1 + r)^n
#Donde:
  #P_t: Poblacion estimada en el año objetivo(2024).
  #P_0: Poblacion conocida en el año base(2020)
  #r: Tasa de crecimiento anual compuesta.
  #n: Numero de años entre el año base y el objetivo (2024-2020 =4)
#Calculanto la tasa de crecimiento anual (r)
# r = (P_2020 / P_2010) - 1 # Tasa de crecimiento anual
  # P_2020: Poblacion en 2020
  #P2010: Poblacion en 2010



# Datos de población en los censos Aguascalientes
P_2010_Aguascalientes <- 810759
P_2020_Aguascalientes <- 1040412

# Número de años entre censos
n_censos_Aguascalientes <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Aguascalientes <- ((P_2020_Aguascalientes / P_2010_Aguascalientes)^(1 / n_censos_Aguascalientes)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Aguascalientes <- 4

# Proyección para 2024
P_2024_Aguascalientes <- P_2020_Aguascalientes * (1 + r_Aguascalientes)^n_proyeccion_Aguascalientes

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Aguascalientes * 100, 4), "%\n")
cat("Proyección de población de Aguascalientes para 2024:", round(P_2024_Aguascalientes), "\n")
#######################################################################################3
# Datos de población en los censos BC
P_2010_BC <- 2253184

P_2020_BC <- 2890717


# Número de años entre censos
n_censos_BC <- 10

# Calcular la tasa de crecimiento anual compuesta
r_BC <- ((P_2020_BC / P_2010_BC)^(1 / n_censos_BC)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_BC <- 4

# Proyección para 2024
P_2024_BC <- P_2020_BC * (1 + r_BC)^n_proyeccion_BC

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_BC * 100, 4), "%\n")
cat("Proyección de población de BC para 2024:", round(P_2024_BC), "\n")
#######################################################################################3
# Datos de población en los censos BCS
P_2010_BCS <- 458554
P_2020_BCS <- 601394



# Número de años entre censos
n_censos_BCS <- 10

# Calcular la tasa de crecimiento anual compuesta
r_BCS <- ((P_2020_BCS / P_2010_BCS)^(1 / n_censos_BCS)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_BCS <- 4

# Proyección para 2024
P_2024_BCS <- P_2020_BCS * (1 + r_BCS)^n_proyeccion_BCS

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_BCS * 100, 4), "%\n")
cat("Proyección de población de BCS para 2024:", round(P_2024_BCS), "\n")
#######################################################################################3
# Datos de población en los censos Campeche
P_2010_Campeche <- 585903
P_2020_Campeche <- 687245

# Número de años entre censos
n_censos_Campeche <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Campeche <- ((P_2020_Campeche / P_2010_Campeche)^(1 / n_censos_Campeche)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Campeche <- 4

# Proyección para 2024
P_2024_Campeche <- P_2020_Campeche * (1 + r_Campeche)^n_proyeccion_Campeche

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Campeche * 100, 4), "%\n")
cat("Proyección de población de Campeche para 2024:", round(P_2024_Campeche), "\n")

#######################################################################################3
# Datos de población en los censos Coahuila
P_2010_Coahuila <- 1951216
P_2020_Coahuila <- 2322491

# Número de años entre censos
n_censos_Coahuila <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Coahuila <- ((P_2020_Coahuila / P_2010_Coahuila)^(1 / n_censos_Coahuila)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Coahuila <- 4

# Proyección para 2024
P_2024_Coahuila <- P_2020_Coahuila * (1 + r_Coahuila)^n_proyeccion_Coahuila

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Coahuila * 100, 4), "%\n")
cat("Proyección de población de Coahuila para 2024:", round(P_2024_Coahuila), "\n")

#######################################################################################3
# Datos de población en los censos Colima
P_2010_Colima <- 472950
P_2020_Colima <- 558228

# Número de años entre censos
n_censos_Colima <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Colima <- ((P_2020_Colima / P_2010_Colima)^(1 / n_censos_Colima)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Colima <- 4

# Proyección para 2024
P_2024_Colima <- P_2020_Colima * (1 + r_Colima)^n_proyeccion_Colima

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Colima * 100, 4), "%\n")
cat("Proyección de población de Colima para 2024:", round(P_2024_Colima), "\n")

#######################################################################################3
# Datos de población en los censos Chiapas
P_2010_Chiapas <- 3151533
P_2020_Chiapas <- 3773752

# Número de años entre censos
n_censos_Chiapas <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Chiapas <- ((P_2020_Chiapas / P_2010_Chiapas)^(1 / n_censos_Chiapas)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Chiapas <- 4

# Proyección para 2024
P_2024_Chiapas <- P_2020_Chiapas * (1 + r_Chiapas)^n_proyeccion_Chiapas

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Chiapas * 100, 4), "%\n")
cat("Proyección de población de Chiapas para 2024:", round(P_2024_Chiapas), "\n")

#######################################################################################3
# Datos de población en los censos Chihuahua
P_2010_Chihuahua <- 2424064
P_2020_Chihuahua <- 2799546

# Número de años entre censos
n_censos_Chihuahua <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Chihuahua <- ((P_2020_Chihuahua / P_2010_Chihuahua)^(1 / n_censos_Chihuahua)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Chihuahua <- 4

# Proyección para 2024
P_2024_Chihuahua <- P_2020_Chihuahua * (1 + r_Chihuahua)^n_proyeccion_Chihuahua

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Chihuahua * 100, 4), "%\n")
cat("Proyección de población de Chihuahua para 2024:", round(P_2024_Chihuahua), "\n")
#######################################################################################3
# Datos de población en los censos CDMX
P_2010_CDMX <- 6913542
P_2020_CDMX <- 7557171

# Número de años entre censos
n_censos_CDMX<- 10

# Calcular la tasa de crecimiento anual compuesta
r_CDMX <- ((P_2020_CDMX / P_2010_CDMX)^(1 / n_censos_CDMX)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_CDMX <- 4

# Proyección para 2024
P_2024_CDMX <- P_2020_CDMX * (1 + r_CDMX)^n_proyeccion_CDMX

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_CDMX * 100, 4), "%\n")
cat("Proyección de población de Chihuahua para 2024:", round(P_2024_CDMX), "\n")

#######################################################################################3
# Datos de población en los censos Durango
P_2010_Durango <- 1108793
P_2020_Durango <- 1315571


# Número de años entre censos
n_censos_Durango<- 10

# Calcular la tasa de crecimiento anual compuesta
r_Durango <- ((P_2020_Durango / P_2010_Durango)^(1 / n_censos_Durango)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Durango <- 4

# Proyección para 2024
P_2024_Durango <- P_2020_Durango * (1 + r_Durango)^n_proyeccion_Durango

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Durango * 100, 4), "%\n")
cat("Proyección de población de Durango para 2024:", round(P_2024_Durango), "\n")

#######################################################################################3
# Datos de población en los censos Guerrero
P_2010_Gro <- 2264184
P_2020_Gro <- 2505724


# Número de años entre censos
n_censos_Gro <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Gro <- ((P_2020_Gro / P_2010_Gro)^(1 / n_censos_Gro)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Gro <- 4

# Proyección para 2024
P_2024_Gro <- P_2020_Gro * (1 + r_Gro)^n_proyeccion_Gro

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Gro * 100, 4), "%\n")
cat("Proyección de población de Guerrero para 2024:", round(P_2024_Gro), "\n")

#######################################################################################3
# Datos de población en los censos Hidalgo
P_2010_Hgo <- 1875609
P_2020_Hgo <- 2291423

# Número de años entre censos
n_censos_Hgo <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Hgo <- ((P_2020_Hgo / P_2010_Hgo)^(1 / n_censos_Hgo)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Hgo <- 4

# Proyección para 2024
P_2024_Hgo <- P_2020_Hgo * (1 + r_Hgo)^n_proyeccion_Hgo

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Hgo * 100, 4), "%\n")
cat("Proyección de población de Guerrero para 2024:", round(P_2024_Hgo), "\n")

#######################################################################################3
# Datos de población en los censos Jalisco
P_2010_Jlc <- 5214266
P_2020_Jlc <- 6244162

# Número de años entre censos
n_censos_Jlc <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Jlc <- ((P_2020_Jlc / P_2010_Jlc)^(1 / n_censos_Jlc)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Jlc <- 4

# Proyección para 2024
P_2024_Jlc <- P_2020_Jlc * (1 + r_Jlc)^n_proyeccion_Jlc

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Jlc * 100, 4), "%\n")
cat("Proyección de población de Jalisco para 2024:", round(P_2024_Jlc), "\n")
#######################################################################################3
# Datos de población en los censos EdoMex
P_2010_EdoMex <- 10821948
P_2020_EdoMex <- 12870351


# Número de años entre censos
n_censos_EdoMex <- 10

# Calcular la tasa de crecimiento anual compuesta
r_EdoMex <- ((P_2020_EdoMex / P_2010_EdoMex)^(1 / n_censos_EdoMex)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_EdoMex <- 4

# Proyección para 2024
P_2024_EdoMex <- P_2020_EdoMex * (1 + r_EdoMex)^n_proyeccion_EdoMex

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_EdoMex * 100, 4), "%\n")
cat("Proyección de población de EdoMex para 2024:", round(P_2024_EdoMex), "\n")
#######################################################################################3
# Datos de población en los censos Michoacan
P_2010_Mich <- 3046758
P_2020_Mich <- 3450522

# Número de años entre censos
n_censos_Mich <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Mich <- ((P_2020_Mich / P_2010_Mich)^(1 / n_censos_Mich)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Mich <- 4

# Proyección para 2024
P_2024_Mich <- P_2020_Mich * (1 + r_Mich)^n_proyeccion_Mich

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Mich * 100, 4), "%\n")
cat("Proyección de población de Michoacan para 2024:", round(P_2024_Mich), "\n")

#######################################################################################3
# Datos de población en los censos Morelos
P_2010_Morelos <- 1287163
P_2020_Morelos <- 1502969

# Número de años entre censos
n_censos_Morelos <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Morelos <- ((P_2020_Morelos / P_2010_Morelos)^(1 / n_censos_Morelos)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Morelos <- 4

# Proyección para 2024
P_2024_Morelos <- P_2020_Morelos * (1 + r_Morelos)^n_proyeccion_Morelos

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Morelos * 100, 4), "%\n")
cat("Proyección de población de Morelos para 2024:", round(P_2024_Morelos), "\n")

#######################################################################################3
# Datos de población en los censos Nayarit
P_2010_Nay <- 1287163
P_2020_Nay<- 1502969

# Número de años entre censos
n_censos_Nay<- 10

# Calcular la tasa de crecimiento anual compuesta
r_Nay <- ((P_2020_Nay / P_2010_Nay)^(1 / n_censos_Nay)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Nay <- 4

# Proyección para 2024
P_2024_Nay <- P_2020_Nay * (1 + r_Nay)^n_proyeccion_Nay

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Nay * 100, 4), "%\n")
cat("Proyección de población de Nayarit para 2024:", round(P_2024_Nay), "\n")

#######################################################################################3
# Datos de población en los censos Nuevo Leon
P_2010_NL <- 3394205
P_2020_NL <- 4402994

# Número de años entre censos
n_censos_NL<- 10

# Calcular la tasa de crecimiento anual compuesta
r_NL <- ((P_2020_NL / P_2010_NL)^(1 / n_censos_NL)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_NL <- 4

# Proyección para 2024
P_2024_NL <- P_2020_NL * (1 + r_NL)^n_proyeccion_NL

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_NL * 100, 4), "%\n")
cat("Proyección de población de NL para 2024:", round(P_2024_NL), "\n")

#######################################################################################3
# Datos de población en los censos Oaxaca
P_2010_Oax <- 2614567

P_2020_Oax <- 2985174


# Número de años entre censos
n_censos_Oax<- 10

# Calcular la tasa de crecimiento anual compuesta
r_Oax <- ((P_2020_Oax / P_2010_Oax)^(1 / n_censos_Oax)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Oax <- 4

# Proyección para 2024
P_2024_Oax <- P_2020_Oax * (1 + r_Oax)^n_proyeccion_Oax

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Oax * 100, 4), "%\n")
cat("Proyección de población de Oaxaca para 2024:", round(P_2024_Oax), "\n")

#######################################################################################3
# Datos de población en los censos Puebla
P_2010_Pue <- 3980085
P_2020_Pue <- 4802574

# Número de años entre censos
n_censos_Pue <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Pue <- ((P_2020_Pue / P_2010_Pue)^(1 / n_censos_Pue)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Pue <- 4

# Proyección para 2024
P_2024_Pue <- P_2020_Pue * (1 + r_Pue)^n_proyeccion_Pue

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Pue * 100, 4), "%\n")
cat("Proyección de población de Puebla para 2024:", round(P_2024_Pue), "\n")
#######################################################################################3
# Datos de población en los censos Queretaro
P_2010_Qro <- 1282872
P_2020_Qro <- 1785004

# Número de años entre censos
n_censos_Qro <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Qro <- ((P_2020_Qro / P_2010_Qro)^(1 / n_censos_Qro)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Qro <- 4

# Proyección para 2024
P_2024_Qro <- P_2020_Qro * (1 + r_Qro)^n_proyeccion_Qro

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Qro * 100, 4), "%\n")
cat("Proyección de población de Queretaro para 2024:", round(P_2024_Qro), "\n")
#######################################################################################3
# Datos de población en los censos Quintana Roo
P_2010_Qroo <- 944046
P_2020_Qroo <- 1400585

# Número de años entre censos
n_censos_Qroo <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Qroo <- ((P_2020_Qroo / P_2010_Qroo)^(1 / n_censos_Qroo)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Qroo <- 4

# Proyección para 2024
P_2024_Qroo <- P_2020_Qroo * (1 + r_Qroo)^n_proyeccion_Qroo

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Qroo * 100, 4), "%\n")
cat("Proyección de población de Quintana Roo para 2024:", round(P_2024_Qroo), "\n")
#######################################################################################3
# Datos de población en los censos San Luis Potosi
P_2010_SLP <- 1799393
P_2020_SLP <- 2095817


# Número de años entre censos
n_censos_SLP <- 10

# Calcular la tasa de crecimiento anual compuesta
r_SLP <- ((P_2020_SLP / P_2010_SLP)^(1 / n_censos_SLP)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_SLP <- 4

# Proyección para 2024
P_2024_SLP <- P_2020_SLP * (1 + r_SLP)^n_proyeccion_SLP

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_SLP * 100, 4), "%\n")
cat("Proyección de población de SLP para 2024:", round(P_2024_SLP), "\n")
#######################################################################################3
# Datos de población en los censos Sinaloa
P_2010_Sin <- 1980225

P_2020_Sin <- 2280039

# Número de años entre censos
n_censos_Sin <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Sin <- ((P_2020_Sin / P_2010_Sin)^(1 / n_censos_Sin)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Sin <- 4

# Proyección para 2024
P_2024_Sin <- P_2020_Sin * (1 + r_Sin)^n_proyeccion_Sin

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Sin * 100, 4), "%\n")
cat("Proyección de población de Sinaloa para 2024:", round(P_2024_Sin), "\n")
#######################################################################################3
# Datos de población en los censos Sonora
P_2010_Son <- 1894678
P_2020_Son <- 2220993

# Número de años entre censos
n_censos_Son <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Son <- ((P_2020_Son / P_2010_Son)^(1 / n_censos_Son)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Son <- 4

# Proyección para 2024
P_2024_Son <- P_2020_Son * (1 + r_Son)^n_proyeccion_Son

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Son * 100, 4), "%\n")
cat("Proyección de población de Sonora para 2024:", round(P_2024_Son), "\n")
#######################################################################################3
# Datos de población en los censos Tabasco
P_2010_Tab <- 1569074
P_2020_Tab <- 1748965

# Número de años entre censos
n_censos_Tab <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Tab <- ((P_2020_Tab / P_2010_Tab)^(1 / n_censos_Tab)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Tab <- 4

# Proyección para 2024
P_2024_Tab <- P_2020_Tab * (1 + r_Tab)^n_proyeccion_Tab

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Tab * 100, 4), "%\n")
cat("Proyección de población de Tabasco para 2024:", round(P_2024_Tab), "\n")

#######################################################################################3
# Datos de población en los censos Tamaulipas
P_2010_Tamps <- 2366026
P_2020_Tamps <- 2656198

# Número de años entre censos
n_censos_Tamps <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Tamps <- ((P_2020_Tamps / P_2010_Tamps)^(1 / n_censos_Tamps)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Tamps <- 4

# Proyección para 2024
P_2024_Tamps <- P_2020_Tamps * (1 + r_Tamps)^n_proyeccion_Tamps

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Tamps * 100, 4), "%\n")
cat("Proyección de población de Tamaulipas para 2024:", round(P_2024_Tamps), "\n")
#######################################################################################3
# Datos de población en los censos Tlaxcala
P_2010_Tlx <- 811899
P_2020_Tlx <- 995061


# Número de años entre censos
n_censos_Tlx <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Tlx <- ((P_2020_Tlx / P_2010_Tlx)^(1 / n_censos_Tlx)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Tlx <- 4

# Proyección para 2024
P_2024_Tlx <- P_2020_Tlx * (1 + r_Tlx)^n_proyeccion_Tlx

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Tlx * 100, 4), "%\n")
cat("Proyección de población de Tlaxcala para 2024:", round(P_2024_Tlx), "\n")

#######################################################################################3
# Datos de población en los censos Veracruz
P_2010_Vrz <- 5510613
P_2020_Vrz <- 6101676
  
# Número de años entre censos
n_censos_Vrz <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Vrz <- ((P_2020_Vrz / P_2010_Vrz)^(1 / n_censos_Vrz)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Vrz <- 4

# Proyección para 2024
P_2024_Vrz <- P_2020_Vrz * (1 + r_Vrz)^n_proyeccion_Vrz

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Vrz * 100, 4), "%\n")
cat("Proyección de población de Veracruz para 2024:", round(P_2024_Vrz), "\n")
#######################################################################################3
# Datos de población en los censos Yucatan
P_2010_Yuc <- 1420659
P_2020_Yuc <- 1769653
	

# Número de años entre censos
n_censos_Yuc <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Yuc <- ((P_2020_Yuc / P_2010_Yuc)^(1 / n_censos_Yuc)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Yuc <- 4

# Proyección para 2024
P_2024_Yuc <- P_2020_Yuc * (1 + r_Yuc)^n_proyeccion_Yuc

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Yuc * 100, 4), "%\n")
cat("Proyección de población de Yucatan para 2024:", round(P_2024_Yuc), "\n")
#######################################################################################3
# Datos de población en los censos Zacatecas
P_2010_Zac <- 1034697
P_2020_Zac <- 1163334
  	


# Número de años entre censos
n_censos_Zac <- 10

# Calcular la tasa de crecimiento anual compuesta
r_Zac <- ((P_2020_Zac / P_2010_Zac)^(1 / n_censos_Zac)) - 1

# Número de años para proyectar desde 2020 hasta 2024
n_proyeccion_Zac <- 4

# Proyección para 2024
P_2024_Zac <- P_2020_Zac * (1 + r_Zac)^n_proyeccion_Zac

# Mostrar resultados
cat("Tasa de crecimiento anual compuesta (r):", round(r_Zac * 100, 4), "%\n")
cat("Proyección de población de Zacatecas para 2024:", round(P_2024_Zac), "\n")
