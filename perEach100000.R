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
# Example: For Aguascalientes
casos_Aguascalientes <- 1954 # Example: cases in 2020
poblacion_Aguascalientes <- 1040412

tasa_100k_Aguascalientes <- (casos_Aguascalientes / poblacion_Aguascalientes) * 100000
cat("Aguascalientes:", tasa_100k_Aguascalientes, "casos por cada 100,000 habitantes\n")

Poblacion_Aguascalientes_2020 <- 1040412
Casos_Violencia_familiar_Aguascalientes <- 2153
Tasa_Violencia_Familiar_Aguascalientes <- (Casos_Violencia_familiar_Aguascalientes/Poblacion_Aguascalientes_2020) * 100000
Tasa_Violencia_Familiar_Aguascalientes

Poblacion_BC_2020 <- 2890717
Casos_Violencia_familiar_BC <- 10781
Tasa_Violencia_Familiar_BC <- (Casos_Violencia_familiar_BC/Poblacion_BC_2020) * 100000
Tasa_Violencia_Familiar_BC  <- Tasa_Violencia_Familiar_BC * 10
round(Tasa_Violencia_Familiar_BC)
Tasa_Violencia_Familiar_BC

Poblacion_BCS_2020 <- 601394
Casos_Violencia_familiar_BCS <- 2439
Tasa_Violencia_Familiar_BCS <- Casos_Violencia_familiar_BCS/Poblacion_BCS_2020 * 100000
Tasa_Violencia_Familiar_BCS

Poblacion_Sonora_2020 <- 2467704
Casos_Violencia_familiar_Sonora <- 4952
Tasa_Violencia_Familiar_Sonora <- Casos_Violencia_familiar_Sonora/Poblacion_Sonora_2020 * 100000
Tasa_Violencia_Familiar_Sonora

Poblacion_Sinaloa_2020 <- 2280039
Casos_Violencia_familiar_Sinaloa <- 4818
Tasa_Violencia_Familiar_Sinaloa <- Casos_Violencia_familiar_Sinaloa/Poblacion_Sinaloa_2020 * 100000
Tasa_Violencia_Familiar_Sinaloa

Poblacion_Nayarit_2020 <- 819398
Casos_Violencia_familiar_Nayarit <- 1108
Tasa_Violencia_Familiar_Nayarit <- Casos_Violencia_familiar_Nayarit/Poblacion_Nayarit_2020 * 100000
Tasa_Violencia_Familiar_Nayarit

Poblacion_Coahuila_2020 <- 2322491
Casos_Violencia_familiar_Coahuila <- 9762
Tasa_Violencia_Familiar_Coahuila <- Casos_Violencia_familiar_Coahuila/Poblacion_Coahuila_2020 * 100000
Tasa_Violencia_Familiar_Coahuila

Poblacion_Chihuahua_2020 <- 2799303
Casos_Violencia_familiar_Chihuahua <- 12418
Tasa_Violencia_Familiar_Chihuahua <- Casos_Violencia_familiar_Chihuahua/Poblacion_Chihuahua_2020 * 100000
Tasa_Violencia_Familiar_Chihuahua

Poblacion_Durango_2020 <- 1165158
Casos_Violencia_familiar_Durango <- 5176
Tasa_Violencia_Familiar_Durango <- Casos_Violencia_familiar_Durango/Poblacion_Durango_2020 * 100000
Tasa_Violencia_Familiar_Durango

Poblacion_Zacatecas_2020 <- 1163334
Casos_Violencia_familiar_Zacatecas <- 2584
Tasa_Violencia_Familiar_Zacatecas <- Casos_Violencia_familiar_Zacatecas/Poblacion_Zacatecas_2020 * 100000
Tasa_Violencia_Familiar_Zacatecas
### PENDIENTE hacerlo por cada 100,000 habitantes

Poblacion_SLP_2020 <- 1970051
Casos_Violencia_familiar_SLP <- 7142
Tasa_Violencia_Familiar_SLP <- Casos_Violencia_familiar_SLP/Poblacion_SLP_2020 * 100000
Tasa_Violencia_Familiar_SLP

Poblacion_NL_2020 <- 4394917
Casos_Violencia_familiar_NL <- 18568
Tasa_Violencia_Familiar_NL <- Casos_Violencia_familiar_NL/Poblacion_NL_2020 * 100000
Tasa_Violencia_Familiar_NL

Poblacion_Tamps_2020 <- 2093987
Casos_Violencia_familiar_Tamps <- 6963
Tasa_Violencia_Familiar_Tamps <- Casos_Violencia_familiar_Tamps/Poblacion_Tamps_2020 * 100000
Tasa_Violencia_Familiar_Tamps

Poblacion_Jalisco_2020 <- 6244162
Casos_Violencia_familiar_Jalisco <- 11915
Tasa_Violencia_Familiar_Jalisco <- Casos_Violencia_familiar_Jalisco/Poblacion_Jalisco_2020 * 100000
Tasa_Violencia_Familiar_Jalisco

Poblacion_Aguas_2020 <- 1040336
Casos_Violencia_familiar_Aguas <- 1954
Tasa_Violencia_Familiar_Aguas <- Casos_Violencia_familiar_Aguas/Poblacion_Aguas_2020 * 100000
Tasa_Violencia_Familiar_Aguas

Poblacion_Mich_2020 <- 3450522
Casos_Violencia_familiar_Mich <- 1183
Tasa_Violencia_Familiar_Mich <- Casos_Violencia_familiar_Mich/Poblacion_Mich_2020 * 100000
Tasa_Violencia_Familiar_Mich

Poblacion_Gto_2020 <- 4531242
Casos_Violencia_familiar_Gto <- 12022
Tasa_Violencia_Familiar_Gto <- Casos_Violencia_familiar_Gto/Poblacion_Gto_2020 * 100000
Tasa_Violencia_Familiar_Gto
Caso_porcada_GTO <- 100000/Tasa_Violencia_Familiar_Gto
Caso_porcada_GTO

Poblacion_Colima_2020 <- 558228
Casos_Violencia_familiar_Colima <- 1318
Tasa_Violencia_Familiar_Colima <- Casos_Violencia_familiar_Colima/Poblacion_Colima_2020 * 100000
Tasa_Violencia_Familiar_Colima
Caso_porcada_Colima <- 100000/Tasa_Violencia_Familiar_Colima
Caso_porcada_Colima

Poblacion_Qro_2020 <- 1785004
Casos_Violencia_familiar_Qro <- 3189
Tasa_Violencia_Familiar_Qro <- Casos_Violencia_familiar_Qro/Poblacion_Qro_2020 * 100000
Tasa_Violencia_Familiar_Qro
Caso_porcada_Qro <- 100000/Tasa_Violencia_Familiar_Qro
Caso_porcada_Qro

Poblacion_Mor_2020 <- 1502969
Casos_Violencia_familiar_Mor <- 5043
Tasa_Violencia_Familiar_Mor <- Casos_Violencia_familiar_Mor/Poblacion_Mor_2020 * 100000
Tasa_Violencia_Familiar_Mor
Caso_porcada_Mor <- 100000/Tasa_Violencia_Familiar_Mor
round(Caso_porcada_Mor)

Poblacion_EdoMex_2020 <- 12870351
Casos_Violencia_familiar_EdoMex <- 15600
Tasa_Violencia_Familiar_EdoMex <- Casos_Violencia_familiar_EdoMex/Poblacion_EdoMex_2020 * 100000
Tasa_Violencia_Familiar_EdoMex
Caso_porcada_EdoMex <- 100000/Tasa_Violencia_Familiar_EdoMex
round(Caso_porcada_EdoMex)

Poblacion_CDMX_2020 <- 7557171
Casos_Violencia_familiar_CDMX <- 27206
Tasa_Violencia_Familiar_CDMX <- Casos_Violencia_familiar_CDMX/Poblacion_CDMX_2020 * 100000
Tasa_Violencia_Familiar_CDMX
Caso_porcada_CDMX <- 100000/Tasa_Violencia_Familiar_CDMX
round(Caso_porcada_CDMX)

Poblacion_Hgo_2020 <- 2291423
Casos_Violencia_familiar_Hgo <- 5335
Tasa_Violencia_Familiar_Hgo <- Casos_Violencia_familiar_Hgo/Poblacion_Hgo_2020 * 100000
Tasa_Violencia_Familiar_Hgo
Caso_porcada_Hgo <- 100000/Tasa_Violencia_Familiar_Hgo
round(Caso_porcada_Hgo)

Poblacion_Tlx_2020 <- 995061
Casos_Violencia_familiar_Tlx <- 62
Tasa_Violencia_Familiar_Tlx <- Casos_Violencia_familiar_Tlx/Poblacion_Tlx_2020 * 100000
Tasa_Violencia_Familiar_Tlx
Caso_porcada_Tlx <- 100000/Tasa_Violencia_Familiar_Tlx
round(Caso_porcada_Tlx)

Poblacion_Pbl_2020 <- 4802574
Casos_Violencia_familiar_Pbl <- 7861
Tasa_Violencia_Familiar_Pbl <- Casos_Violencia_familiar_Pbl/Poblacion_Pbl_2020 * 100000
Tasa_Violencia_Familiar_Pbl
Caso_porcada_Pbl <- 100000/Tasa_Violencia_Familiar_Pbl
round(Caso_porcada_Pbl)

Poblacion_Ver_2020 <- 6101676
Casos_Violencia_familiar_Ver <- 8817
Tasa_Violencia_Familiar_Ver <- Casos_Violencia_familiar_Ver/Poblacion_Ver_2020 * 100000
Tasa_Violencia_Familiar_Ver
Caso_porcada_Ver <- 100000/Tasa_Violencia_Familiar_Ver
round(Caso_porcada_Ver)

Poblacion_Tab_2020 <- 1748965
Casos_Violencia_familiar_Tab <- 6636
Tasa_Violencia_Familiar_Tab <- Casos_Violencia_familiar_Tab/Poblacion_Tab_2020 * 100000
Tasa_Violencia_Familiar_Tab
Caso_porcada_Tab <- 100000/Tasa_Violencia_Familiar_Tab
round(Caso_porcada_Tab)

Poblacion_Gro_2020 <- 2505724
Casos_Violencia_familiar_Gro <- 2876
Tasa_Violencia_Familiar_Gro <- Casos_Violencia_familiar_Gro/Poblacion_Gro_2020 * 100000
Tasa_Violencia_Familiar_Gro
Caso_porcada_Gro <- 100000/Tasa_Violencia_Familiar_Gro
round(Caso_porcada_Gro)

Poblacion_Oax_2020 <- 2985174
Casos_Violencia_familiar_Oax <- 5388
Tasa_Violencia_Familiar_Oax <- Casos_Violencia_familiar_Oax/Poblacion_Oax_2020 * 100000
Tasa_Violencia_Familiar_Oax
Caso_porcada_Oax <- 100000/Tasa_Violencia_Familiar_Oax
round(Caso_porcada_Oax)

Poblacion_Chs_2020 <- 3773752
Casos_Violencia_familiar_Chs <- 2990
Tasa_Violencia_Familiar_Chs <- Casos_Violencia_familiar_Chs/Poblacion_Chs_2020 * 100000
Tasa_Violencia_Familiar_Chs
Caso_porcada_Chs <- 100000/Tasa_Violencia_Familiar_Chs
round(Caso_porcada_Chs)

Poblacion_Yuc_2020 <- 1769653
Casos_Violencia_familiar_Yuc <- 1256
Tasa_Violencia_Familiar_Yuc <- Casos_Violencia_familiar_Yuc/Poblacion_Yuc_2020 * 100000
Tasa_Violencia_Familiar_Yuc
Caso_porcada_Yuc <- 100000/Tasa_Violencia_Familiar_Yuc
round(Caso_porcada_Yuc)

Poblacion_Cmpch_2020 <- 685101
Casos_Violencia_familiar_Cmpch <- 589
Tasa_Violencia_Familiar_Cmpch <- Casos_Violencia_familiar_Cmpch/Poblacion_Cmpch_2020 * 100000
Tasa_Violencia_Familiar_Cmpch
Caso_porcada_Cmpch <- 100000/Tasa_Violencia_Familiar_Cmpch
round(Caso_porcada_Cmpch)

Poblacion_QRoo_2020 <- 1400585
Casos_Violencia_familiar_QRoo <- 4751
Tasa_Violencia_Familiar_QRoo <- Casos_Violencia_familiar_QRoo/Poblacion_QRoo_2020 * 100000
Tasa_Violencia_Familiar_QRoo
Caso_porcada_QRoo <- 100000/Tasa_Violencia_Familiar_QRoo
round(Caso_porcada_QRoo)
