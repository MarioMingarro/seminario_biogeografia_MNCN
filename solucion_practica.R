library(dplyr) # Manipulación de datos
library(sf) # Manejo de datos espaciales
library(ggplot2) # Visualización de datos
library(geodata) # Descarga de datos geográficos
library(terra) # Manejo de datos raster
library(gridExtra) # Organiza múltiples gráficos

# DATOS ----
## BATIMETRÏA ----
batimetria_canarias_2024 <- terra::rast("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/BATIMETRIA/batimetria_canarias_2024.tif")
orientacion_AE <- terra::rast("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/BATIMETRIA/orientacion_AE.tif")

## AP ----
WDPA_CAN <- read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/AP/WDPA_CAN.shp")

## OTROS ----
squatina <- read.csv2("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Squatina_squatina.csv")
Area_estudio_25830 <- read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Area_estudio_25830.shp")
Islas_Canarias_25830 <- read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Islas_Canarias_25830.shp")

# FASE 1 ----
## BATIMETRÏA ----

## AP ----
WDPA_CAN_MARINE <- WDPA_CAN %>% 
  dplyr::filter(MARINE==2)

WDPA_CAN_MARINE_AREA <- WDPA_CAN %>% 
  dplyr::filter(MARINE==2) %>% 
  dplyr::mutate(AREA = st_area())

## OTROS ----
squatina_4326 <- sf::st_as_sf(na.omit(squatina), coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
Area_estudio <- sf::st_transform(Area_estudio_25830, crs = 4083)
Islas_Canarias <- sf::st_transform(Islas_Canarias_25830, crs = 4083)

