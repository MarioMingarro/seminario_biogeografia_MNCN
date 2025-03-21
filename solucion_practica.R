library(dplyr) # Manipulación de datos
library(sf) # Manejo de datos espaciales
library(terra) # Manejo de datos raster


# DATOS ----
## BATIMETRÏA ----
batimetria_canarias_2024 <- terra::rast("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/BATIMETRIA/batimetria_canarias_2024.tif")
orientacion_AE <- terra::rast("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/BATIMETRIA/orientacion_AE.tif")

## AP ----
WDPA_CAN <- sf::read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/AP/WDPA_CAN.shp")

## OTROS ----
squatina <- read.csv2("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Squatina_squatina.csv")
Area_estudio_25830 <- sf::read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Area_estudio_25830.shp")
Islas_Canarias_25830 <- sf::read_sf("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/PRACTICAS/DATOS_PRACTICA/1_DATA/Islas_Canarias_25830.shp")

# FASE 1 ----
## AP ----
WDPA_CAN_MARINE <- WDPA_CAN %>% 
  dplyr::filter(MARINE==2)

WDPA_CAN_MARINE_AREA <- WDPA_CAN %>% 
  dplyr::filter(MARINE==2) %>% 
  dplyr::mutate(AREA = as.numeric(st_area(.)/ 1000000))

## OTROS ----
squatina_4326 <- sf::st_as_sf(na.omit(squatina), coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
Area_estudio <- sf::st_transform(Area_estudio_25830, crs = 4083)
Islas_Canarias <- sf::st_union(sf::st_transform(Islas_Canarias_25830, crs = 4083))
Islas_Canarias <- sf::st_union(Islas_Canarias)

## BATIMETRÏA ----
Area_estudio_vect <- terra::vect(Area_estudio)
batimetria_canarias_2024_AE <- terra::mask(terra::crop(batimetria_canarias_2024, Area_estudio_vect), Area_estudio_vect)


# FASE 2 ----
## OTROS ----
squatina_corregida <- sf::st_transform(squatina_4326, crs = 4083)
squatina_corregida <- sf::st_intersection(squatina_corregida, Area_estudio)
squatina_corregida <- sf::st_difference(squatina_corregida, Islas_Canarias)

## AP ----
WDPA_CAN_MARINE_AREA_SPP <- sf::st_join(WDPA_CAN_MARINE_AREA, squatina_corregida, join = st_intersects)
WDPA_CAN_MARINE_AREA_SPP_DENSIDAD <- WDPA_CAN_MARINE_AREA_SPP %>%
                                      group_by(NAME) %>%
                                      summarise(n = n(), AREA = max(AREA)) %>%
                                      mutate(DENSIDAD = n / AREA)

## BATIMETRÏA ----
squatina_corregida_vect <- terra::vect(squatina_corregida)
squatina_corregida_mbsl <- terra::extract(batimetria_canarias_2024_AE, squatina_corregida_vect, xy = TRUE)
squatina_corregida_mbsl <- sf::st_as_sf(squatina_corregida_mbsl, coords = c("x", "y"), crs = 4083)
squatina_corregida_mbsl_noprotegido <- sf::st_difference(squatina_corregida_mbsl, WDPA_CAN_MARINE_AREA)

rm(list = setdiff(ls(), c("squatina_corregida_mbsl_noprotegido", "squatina_corregida_mbsl", 
                          "Area_estudio", "Islas_Canarias", "batimetria_canarias_2024_AE", 
                          "orientacion_AE", "WDPA_CAN_MARINE_AREA_SPP_DENSIDAD")))
