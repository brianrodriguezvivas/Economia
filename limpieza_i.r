
rm(list=ls())
#setwd("C:/Users/Asus/OneDrive - Universidad de Antioquia/Escritorio/Hydrogen/Data/Analizadores3") # cuidado hacer una copia en el drive personal (no depender de datos que alguien peueda tocar)
library(readxl)
library(tidyr)

Year <- "2023"

if (Year == '2021'){ # El año 2021 no tiene datos para enero ni febrero y arranca el 8 de marzo
  Data <- read_excel('bd_analizadores3_2021-01-01_to_2021-12-31_2024.xlsx')
  Data <- data.frame(Data)
}
if (Year == '2022'){
  Data <- read_excel('bd_analizadores3_2022-01-01_to_2022-12-31_2023.xlsx')
  Data <- data.frame(Data)
}
if (Year == '2023'){
  Data <- read_excel('bd_analizadores3_2023-01-01_to_2023-12-31_2024.xlsx')
  Data <- data.frame(Data)
}

Data$datetime <- format(Data$datetime, format="%Y-%m-%d %H:%M:00")

subset_Data <- subset(Data, analizador == "AnP03_")
subset_Data <- subset_Data[,-c(1,3,4,5,6,7,9,10,11,12)]

head(subset_Data,15)

library(lubridate)

# Definir la fecha de inicio y fin para el intervalo
fecha_inicio <- ymd_hms(paste(Year,"-01-01 00:00:00"))
fecha_fin <- ymd_hms(paste(Year,"-12-31 23:59:59"))

# Crear una secuencia de fechas mensuales
intervalo_mensual_inicio <- seq(from = fecha_inicio, to = fecha_fin, by = "1 month")

# Calcular las fechas de término de cada mes
intervalo_mensual_fin <- intervalo_mensual_inicio + months(1) - seconds(1)

# Obtener los días en cada mes
dias_en_cada_mes <- as.numeric(days_in_month(intervalo_mensual_inicio))

# Obtener los minutos en cada mes
minutos_en_cada_mes <- dias_en_cada_mes*1440

# Imprimir las fechas de inicio y fin generadas
print(intervalo_mensual_inicio)
print(intervalo_mensual_fin)

library(tidyr)
library(dplyr)

############################################
# identificar datos repetidos y dejar solo uno
############################################

head(subset_Data)

#Identificar filas duplicadas basándose en la columna 'datetime'
duplicados <- duplicated(subset_Data$datetime)
subset_Data$datetime[which(duplicados==TRUE)] # muestra cuales datos son los que están repetidos
dim(subset_Data)

# Filtrar el dataframe para excluir las filas duplicadas
subset_Data <- subset_Data[!duplicados, ] # subset sin duplicados
dim(subset_Data)

############################################
mi_lista <- list() # identificar datos no reportados y ponerles NA
############################################

for (i in 1:12) {

fecha_inicio <- intervalo_mensual_inicio[i]
fecha_fin <- intervalo_mensual_fin[i]
   
df_mes <- subset_Data %>%
  filter(month(datetime) == i)
 
mes_completo <- NULL
for (j in 1:dias_en_cada_mes[i]){
  df_mes_dia <- df_mes %>%
    filter(day(datetime) == j)
  df_mes_dia$datetime <- as.POSIXct(df_mes_dia$datetime,tz = "UTC")
  class(df_mes_dia$datetime)
  dia_completo <- complete(df_mes_dia,
                           datetime = seq.POSIXt(from = floor_date(fecha_inicio, "day") + days(j-1) + hours(6) ,
                           to = floor_date(fecha_inicio, "day") + days(j-1) + hours(18) - minutes(1), by = "min")
                           )
  if(nrow(dia_completo)>720){dia_completo<-dia_completo[1:720,]}
  dia_completo <- data.frame(dia_completo)
  mes_completo <- rbind(mes_completo,dia_completo)
  remove(dia_completo)
}

  mi_lista[[i]] <- mes_completo
  remove(mes_completo)
}
 
#######################################################
# Imputando el NA de un minuto específico o a lo sumo dos NA consecutivos por el promedio entre vecindades
#######################################################

numberNA <- NULL
for (i in 1:12){
  numberNA <- c(numberNA,length(which(is.na(mi_lista[[i]][,2]))))
}
numberNA # Número de NA por mes

for (j in 1:12) {
indices_primeros_na <- which(is.na(mi_lista[[j]][,2]) & !is.na(c(0, mi_lista[[j]][,2][-length(mi_lista[[j]][,2])])))
longitudes_secuencias_na <- rle(is.na(mi_lista[[j]][,2]))$lengths[which(rle(is.na(mi_lista[[j]][,2]))$values)]

# Verificar si indices_primeros_na es un vector vacío
if (length(indices_primeros_na) > 0) {
  # Entrar al bucle for solo si indices_primeros_na no está vacío

for (i in 1:length(longitudes_secuencias_na)) {
  if (longitudes_secuencias_na[i] == 1) {
    mi_lista[[j]][,2][indices_primeros_na[i]] <- (mi_lista[[j]][,2][indices_primeros_na[i]-1] + mi_lista[[j]][,2][indices_primeros_na[i]+longitudes_secuencias_na[i]])*0.5
  }
  if (longitudes_secuencias_na[i] == 2) {
    mi_lista[[j]][,2][indices_primeros_na[i]] <- (mi_lista[[j]][,2][indices_primeros_na[i]-1] + mi_lista[[j]][,2][indices_primeros_na[i]+longitudes_secuencias_na[i]])*0.5
    mi_lista[[j]][,2][indices_primeros_na[i]+1] <- (mi_lista[[j]][,2][indices_primeros_na[i]-1] + mi_lista[[j]][,2][indices_primeros_na[i]+longitudes_secuencias_na[i]])*0.5
  }
}
remove(indices_primeros_na)
remove(longitudes_secuencias_na)
}

}

numberNA2 <- NULL
for (i in 1:12){
  numberNA2 <- c(numberNA2,length(which(is.na(mi_lista[[i]][,2]))))
}
numberNA
numberNA2 # Número de NA por mes

#######################################################
# Lista para almacenar las series completas de cada día
mi_lista2 <- list() # Ponerle ceros a los datos después de las 18 y antes de las 6 am
#######################################################

for (i in 1:12){
# Bucle para cada día de enero

  resultados <- array(data = NA, dim = c(0, 0))
 
for (j in 1:dias_en_cada_mes[i]) {
  # Filtrar la serie original para el día actual
  serie_dia <- mi_lista[[i]] %>%
    filter(datetime >= ymd_hms(paste(Year, sprintf("%02d", i), sprintf("%02d", j), " 00:00:00")) &
             datetime <= ymd_hms(paste(Year, sprintf("%02d", i), sprintf("%02d", j), " 23:59:59")))
 
  fechas_diarias <- tibble(
    datetime = seq(ymd_hms(paste(Year, sprintf("%02d", i), sprintf("%02d", j), " 00:00:00")),
                   ymd_hms(paste(Year, sprintf("%02d", i), sprintf("%02d", j), " 23:59:59")), by = "1 min")
  )
 
  serie_completa <- fechas_diarias %>%
    left_join(serie_dia, by = "datetime") %>%
    mutate(Pmax = if_else((hour(datetime) < 6 | hour(datetime) >= 18), coalesce(Pmax, 0), Pmax))
 
  # Almacenar la serie completa en la lista
  resultados <- rbind(resultados,data.frame(serie_completa))
  remove(serie_completa)
 
}
 
  mi_lista2[[i]] <- resultados
  remove(resultados)
 
}

#######################################################
mi_lista3 <- list() # Agrupando serie minutal en periodos de 15 minutos (96 x día)
#######################################################

for (i in 1:12){
# Agrupar los datos en intervalos de 15 minutos y calcular el promedio
resultados <- mi_lista2[[i]] %>%
  group_by(intervalo_15min = cut(datetime, breaks = "15 min")) %>%
  summarise(Promedio_15min = mean(Pmax, na.rm = TRUE)) # Cuando na.rm se establece en TRUE, la función mean ignora los valores faltantes al realizar el cálculo del promedio
  mi_lista3[[i]] <- data.frame(resultados)
  remove(resultados)
}

#######################################################
# Gráficas
#######################################################

Per15Min <- 96
serie_media <- array(data=NA, dim = c(12, 96))
serie_mediana <- array(data=NA, dim = c(12, 96))
rows <- 3
cols <- 4

# Establece el diseño de la figura
par(mfrow = c(rows, cols))

for (j in 1:12) {
  Pmax15min <- array(mi_lista3[[j]][, 2], dim = c(96, dias_en_cada_mes[j]))
  # Obtener el nombre del mes
  nombre_mes <- month.name[j]
 
  # Graficar el boxplot
  boxplot(t(Pmax15min), xlab = 'Period', ylab = 'Pmax', ylim=c(0,50), # Ajuste para que 'at' tenga la misma longitud que el número de observaciones
          col = 'lightgray', border = 'black', horizontal = FALSE, main = paste('Box plot (W) -', nombre_mes))
 
  serie_media[j,] <- rowMeans(Pmax15min, na.rm = TRUE)
  serie_mediana[j,] <- apply(Pmax15min, MARGIN = 1, FUN = median, na.rm = TRUE)
 
  # Eliminar Pmax15min
  remove(Pmax15min)
}
# Restaurar el diseño por defecto de una sola celda
par(mfrow = c(1, 1))


matplot(serie_media[1,], type = 'l', xlab = 'Period', ylab = 'Pmax', ylim = c(0,40),
        main = 'Average Pmax Time Series per Month', col = 2, lty = 1, lwd = 2)
for(j in 2:12){
lines(serie_media[j,],lty = 1, lwd = 2, col = j)
}
legend("topleft", legend = month.name[1:6], col = 1:6, lty = 1, lwd = 2,cex=1)
legend("topright", legend = month.name[7:12], col = 7:12, lty = 1, lwd = 2,cex=1)

matplot(serie_mediana[1,], type = 'l', xlab = 'Perid', ylab = 'Pmax', ylim = c(0,40),
        main = 'Median Pmax Time Series per Month', col = 2, lty = 1, lwd = 2)
for(j in 2:12){
  lines(serie_mediana[j,],lty = 1, lwd = 2, col = j)
}
legend("topleft", legend = month.name[1:6], col = 1:6, lty = 1, lwd = 2,cex=1)
legend("topright", legend = month.name[7:12], col = 7:12, lty = 1, lwd = 2,cex=1)

# GRAFICANDO Las SERIES de la Pmax

#plot(Pmax15min[1:96,1],type='l', ylim = c(0.98*min(Pmax15min),1.02*max(Pmax15min)), xlab = 'Periodo 15 minutal', ylab = 'Pmax (Ene/2022)', main = 'Pmax (1/01/2022 - 31/01/2022)', col = 2)
#for (j in 2:31) {
#  lines(Pmax15min[(1:96),j],type ='l', col = 2)
#}

#######################################################
mi_lista4 <- mi_lista2 # Imputando NAs por la serie media
#######################################################

# Detectando series media y medianas para cada mes
serie_media_minutal <- array(data=NA, dim = c(12, 1440))
serie_mediana_minutal <- array(data=NA, dim = c(12, 1440))
for (j in 1:12) {
  PmaxMinutal <- array(mi_lista2[[j]][, 2], dim = c(1440, dias_en_cada_mes[j]))
  serie_media_minutal[j,] <- rowMeans(PmaxMinutal, na.rm = TRUE)
  serie_mediana_minutal[j,] <- apply(PmaxMinutal, MARGIN = 1, FUN = median, na.rm = TRUE)
}

WhichAreNA <- list()
for (i in 1:12) {
  WhichAreNA[[i]] <- which(is.na(serie_media_minutal[i,]))
}

elementos_vacios <- sapply(WhichAreNA, function(x) length(x) == 0)
# Verificar si al menos un elemento es TRUE
# Si hay TRUE quiere decir que hay que imputar algunas series medias

if (any(elementos_vacios)==TRUE) {

  # vamos a imputar las series diarias sin datos
  # para ello vamos a promediar las series diarias de los años 2022 y 2023
 
  serie_media_2022 <- read_excel('C:/Users/Asus/OneDrive - Universidad de Antioquia/Escritorio/Hydrogen/Data/Analizadores3/serie_media_minutal_2022.xlsx')
  serie_media_2022 <- data.frame(serie_media_2022)
  serie_media_2023 <- read_excel('C:/Users/Asus/OneDrive - Universidad de Antioquia/Escritorio/Hydrogen/Data/Analizadores3/serie_media_minutal_2023.xlsx')
  serie_media_2022 <- data.frame(serie_media_2023)
 
for (i in 1:12){
if (elementos_vacios[i]==FALSE){
  serie_media_minutal[i,] = (t(serie_media_2022)[i,]+t(serie_media_2023)[i,])*0.5
}  
}
}

library(writexl)
# Obtener el año como cadena
year_str <- as.character(Year)
# Construir el nombre del archivo con el año
nombre_archivo_0 <- paste0("serie_media_minutal_",year_str,".xlsx")
# Construir la ruta completa
ruta_excel_0 <- file.path("C:/Users/Asus/OneDrive - Universidad de Antioquia/Escritorio/Hydrogen/Data/Analizadores3", nombre_archivo_0)

write_xlsx(as.data.frame(t(serie_media_minutal)), ruta_excel_0)

# Imputando el NA de un minuto específico por su valor medio

posiciones_na <- NULL
for (i in 1:12){
  for (j in 1:dias_en_cada_mes[i]) {
posiciones_na <- which(is.na(mi_lista2[[i]][(1440*j-1439):(1440*j), 2]))

if (length(posiciones_na) > 0 && length(posiciones_na) < 720) {

Valor_Izq <- mi_lista4[[i]][(1440*(j-1)+posiciones_na[1]-1), 2]
Valor_Der <- mi_lista4[[i]][(1440*(j-1)+tail(posiciones_na, n = 1)+1), 2]
Serie_media_Izq <- serie_media_minutal[i,posiciones_na[1]-1]
Serie_media_Der <- serie_media_minutal[i,tail(posiciones_na, n = 1)+1]

if (is.nan(Valor_Izq/Serie_media_Izq)) {
    FactIni <- 0
  } else {
  FactIni <- Valor_Izq/Serie_media_Izq
}

if (is.nan(Valor_Der/Serie_media_Der)) {
    FactFin <- 0
  } else {
  FactFin <- Valor_Der/Serie_media_Der
}

Step <- abs(FactFin-FactIni)/length(posiciones_na)

if (FactFin-FactIni < 0) { Dir <- -1} else { Dir <- 1} # si Dir es -1 la serie decrece

FactDegra <- NULL
for(k in 1:length(posiciones_na)){
  FactDegra <- c(FactDegra,FactIni+Dir*k*Step)
}
mi_lista4[[i]][(1440*(j-1)+posiciones_na), 2] <- serie_media_minutal[i,posiciones_na]*FactDegra # Segundo tipo de imputación, se imputa usando factor degradado
}
else {
mi_lista4[[i]][(1440*(j-1)+posiciones_na), 2] <- serie_media_minutal[i,posiciones_na] # Tercer tipo de imputación, si casi no hay datos en el día se imputa por la serie media!
}
remove(posiciones_na)
print(j)
  }
}

#plot(mi_lista2[[3]][(13700:14100),2],type="l")
#plot(mi_lista4[[3]][(13700:14100),2],type="l")

library(openxlsx)

# Obtener el año como cadena
year_str <- as.character(Year)

# Construir el nombre del archivo con el año
nombre_archivo <- paste0("Sol_PV_",year_str,".xlsx")

# Construir la ruta completa
ruta_excel <- file.path("C:/Users/Asus/OneDrive - Universidad de Antioquia/Escritorio/Hydrogen/Data/Analizadores3", nombre_archivo)

# Crear un objeto de libro de Excel
libro_excel <- createWorkbook()

names(mi_lista4) <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Escribir cada elemento de la lista en una hoja de Excel
for (nombre_hoja in names(mi_lista4)) {
  addWorksheet(libro_excel, sheetName = nombre_hoja)
  writeData(libro_excel, sheet = nombre_hoja, x = mi_lista4[[nombre_hoja]])
}

# Guardar el libro de Excel
saveWorkbook(libro_excel, file = ruta_excel)

# verificar que se haya creado el archivo
# mediante la función file.exists
file.exists(ruta_excel)

#######################################################
mi_lista5 <- list() # Agrupando serie minutal en periodos de 15 minutos (96 x día) SIN NAs
#######################################################

for (i in 1:12){
  # Agrupar los datos en intervalos de 15 minutos y calcular el promedio
  resultados <- mi_lista4[[i]] %>%
    group_by(intervalo_15min = cut(datetime, breaks = "15 min")) %>%
    summarise(Promedio_15min = mean(Pmax, na.rm = TRUE)) # Cuando na.rm se establece en TRUE, la función mean ignora los valores faltantes al realizar el cálculo del promedio
  mi_lista5[[i]] <- data.frame(resultados)
  remove(resultados)
}

#######################################################
# Gráficas
#######################################################

Per15Min <- 96
serie_media <- array(data=NA, dim = c(12, 96))
serie_mediana <- array(data=NA, dim = c(12, 96))
rows <- 3
cols <- 4

# Establece el diseño de la figura
par(mfrow = c(rows, cols))

for (j in 1:12) {
  Pmax15min <- array(mi_lista5[[j]][, 2], dim = c(96, dias_en_cada_mes[j]))
  # Obtener el nombre del mes
  nombre_mes <- month.name[j]
 
  # Graficar el boxplot
  boxplot(t(Pmax15min), xlab = 'Period', ylab = 'Pmax', ylim=c(0,50), # Ajuste para que 'at' tenga la misma longitud que el número de observaciones
          col = 'lightgray', border = 'black', horizontal = FALSE, main = paste('Box plot (W) -', nombre_mes))
 
  serie_media[j,] <- rowMeans(Pmax15min, na.rm = TRUE)
  serie_mediana[j,] <- apply(Pmax15min, MARGIN = 1, FUN = median, na.rm = TRUE)
 
  # Eliminar Pmax15min
  remove(Pmax15min)
}
# Restaurar el diseño por defecto de una sola celda
par(mfrow = c(1, 1))


matplot(serie_media[1,], type = 'l', xlab = 'Period', ylab = 'Pmax', ylim = c(0,40),
        main = 'Average Pmax Time Series per Month', col = 2, lty = 1, lwd = 2)
for(j in 2:12){
  lines(serie_media[j,],lty = 1, lwd = 2, col = j)
}
legend("topright", legend = month.name[1:12], col = 1:12, lty = 1, lwd = 2,cex=0.75)

matplot(serie_mediana[1,], type = 'l', xlab = 'Perid', ylab = 'Pmax', ylim = c(0,40),
        main = 'Median Pmax Time Series per Month', col = 2, lty = 1, lwd = 2)
for(j in 2:12){
  lines(serie_mediana[j,],lty = 1, lwd = 2, col = j)
}
legend("topright", legend = month.name[1:12], col = 1:12, lty = 1, lwd = 2,cex=0.75)

#######################################################
CumPmax <- NULL # sumando el Pmax minutal de cada Mes
#######################################################

for (i in 1:12){
  CumPmax <- c(CumPmax,1/60*sum(mi_lista4[[i]][,2]))
}

max(CumPmax) # Dato máximo
min(CumPmax) # Dato mínimo
mean(CumPmax) # Dato promedio
median(CumPmax) # Mediana
(max(CumPmax)-min(CumPmax)) # Diff entre dato máximo y mínimo
sd(CumPmax) # Desviación estándar
var(CumPmax) # Varianza
IQR(CumPmax) # Rango_intercuartilico

boxplot(CumPmax)


#########


2023-01-07 09:00:00 2023-01-07 09:06:00 FactDegra:  [0.06771588822793824, 0.13543177645587648, 0.20314766468381473, 0.27086355291175296, 0.3385794411396912, 0.40629532936762947]
Valor_izqui:  nan Valor_Dere:  7.3823 Serie_media_Izq:  17.710524 Serie_media_Der:  18.169787999999997