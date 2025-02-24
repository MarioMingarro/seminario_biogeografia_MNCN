library(sf)
library(dplyr)
library(ggplot2)

#Carga capa desde csv

endemism <- read.csv2("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")

# se comprueba su estructura

str(endemism)

# Convertir a objeto sf

endemism_sf <- st_as_sf(endemism, coords = c("Longitud", "Latitud"), crs = 4326)

# Comprobar el crs
st_crs(endemism_sf) 

# Transformar el shapefile a ETRS89 30N (EPSG: 25830)
endemism_sf <- sf::st_transform(endemism_sf, crs = 25830)



# Mapa básico  con ggplot
ggplot() +
  geom_sf(data = endemism_sf, color = "red")

# Mapa combinado con ENP y endemismos
ggplot() +
  geom_sf(data = endemism_sf, aes(color = Especie))

#Añadir mas datos utilizando funciones de paquetes
library(geodata)

# Cargar datos de España usando geodata
spain_map <- geodata::gadm(country = "ESP", level = 1, path = "data/") # Nivel 1 para provincias
spain_map <- sf::st_as_sf(spain_map)

# Transformar la proyección si es necesario (asumiendo que endemism_sf está en 25830)
st_crs(spain_map) == st_crs(endemism_sf)
spain <- sf::st_transform(spain_map, crs = 25830)
st_crs(spain) == st_crs(endemism_sf)


ggplot() +
  geom_sf(data = spain)+
  geom_sf(data = endemism_sf, color = "red")


# Ejercicio 1
str(spain)
unique(spain$NAME_1)

spain_peninsular <- spain %>%
  dplyr::select(CCAA = NAME_1) %>%
  dplyr::filter(!CCAA %in% c("Islas Baleares", "Islas Canarias", "Ceuta y Melilla"))

ggplot() +
  geom_sf(data = spain_peninsular)+
  geom_sf(data = endemism_sf, color = "red")

#######

spain_peninsular_dissolved <- st_union(spain_peninsular)

ggplot() +
  geom_sf(data = spain_peninsular, color = "green4") +
  geom_sf(data = spain_peninsular_dissolved, fill= "transparent", color = "black", linewidth  = 1)+
  geom_sf(data = endemism_sf, color = "red")

world_map <- geodata::world(path=tempdir(), resolution = 2) # Puedes ajustar la resolución
world_map <- sf::st_as_sf(world_map)


ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_sf(data = endemism_sf, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


# Crear malla 
malla <- st_make_grid(spain_peninsular_dissolved, cellsize = 50000, square = FALSE)
class(malla)
malla <- sf::st_as_sf(malla)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = endemism_sf, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


malla <- st_intersection(malla, spain_peninsular_dissolved)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = endemism_sf, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()



# Ejercicio 2 ----
# Seleccionar los registros dentro de España peninsular 
endemism_sf_spain <- st_intersection(endemism_sf, spain_peninsular_dissolved)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_sf(data = endemism_sf, color = "green")+
  geom_sf(data = endemism_sf_spain, color = "red")+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()

#---------

malla_puntos <- st_join(malla, endemism_sf_spain, join = st_intersects)
malla_puntos <- malla_puntos %>%
  filter(Especie > 0)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemism_sf_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


malla_puntos <- malla_puntos %>% mutate(area_km2 = as.numeric(st_area(.)/1000000))

library(terra)


# Instalar y cargar las librerías necesarias
#install.packages(c("terra", "geodata", "rnaturalearth"))

# 1. Descargar datos raster: DEM y clima
# Descargar un DEM (Digital Elevation Model) de la región de España

spain_peninsular_vect <- terra::vect(spain_peninsular_dissolved)
dem <- geodata::elevation_30s(country = "ESP", path=tempdir())

dem_df <- as.data.frame(dem, xy = TRUE)

# 2. Renombrar las columnas (si es necesario)
colnames(dem_df) <- c("x", "y", "elevation") # Asumiendo que tu raster tiene una sola capa llamada "elevation"

# 3. Usar ggplot2 para trazar el raster
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation))

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation))+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemism_sf_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()

# Cambiar resolucion a 0,25º
dem <-  terra::aggregate(dem, 3, fun = "median")

# 3. Sistemas de referencia de coordenadas (CRS)
# Verificar el CRS de los datos
terra::crs(dem)

# Transformar el CRS de los datos vectoriales al CRS del DEM
dem <- terra::project(dem, crs(spain_peninsular_vect))

# Verificar el CRS de los datos
terra::crs(dem) == terra::crs(spain_peninsular_vect)

# 4. Recortar datos raster con máscara y extensión
# Recortar el DEM usando la extensión de España
dem_recortado <- terra::crop(dem, spain_peninsular_vect)
dem_recortado <- terra::mask(dem_recortado, spain_peninsular_vect)
dem_recortado_df <- as.data.frame(dem_recortado, xy = TRUE)

# 2. Renombrar las columnas (si es necesario)
colnames(dem_recortado_df) <- c("x", "y", "elevation") # Asumiendo que tu raster tiene una sola capa llamada "elevation"

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", linewidth  = 1)+
  geom_raster(data = dem_recortado_df, aes(x = x, y = y, fill = elevation))+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemism_sf_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()

# Descargar datos climáticos (temperatura máxima mensual) de la misma región
tmax <- geodata::worldclim_country(country = "ESP", var = "tmax", res = 5, path=tempdir())
# Recortar la temperatura máxima usando la máscara de España
tmax_recortado <- terra::mask(tmax, espana_proj)

# 5. Estadísticas zonales
# Crear polígonos de ejemplo (provincias de España)
# Nota: Para un análisis real, necesitarías un shapefile con los límites de las provincias.
# Aquí crearemos polígonos aleatorios dentro de España para fines demostrativos.
set.seed(123)
provincias <- terra::spatSample(espana_proj, size = 10, "polygons")

# Calcular la elevación media por provincia
elevacion_media <- terra::zonal(dem_recortado, provincias, fun = "mean")

# Imprimir los resultados
print(elevacion_media)

# 6. Visualización básica
# Visualizar el DEM recortado
plot(dem_recortado, main = "DEM Recortado de España")

# Visualizar la temperatura máxima recortada
plot(tmax_recortado[[1]], main = "Temperatura Máxima en Enero")

# Visualizar los polígonos de las provincias
plot(provincias, add = TRUE, border = "red")









#### ENPS 
# Leer el archivo shapefile de enp
enp <- st_read("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/enp/Enp2023.shp")
plot(enp$geometry)

str(enp)
unique(enp$CCAA_N_ENP)
# dplyr filtrado
enp <- enp %>%
  dplyr::select(Nombre = SITENAME, Tipo = FIGURA_LP, CCAA = CCAA_N_ENP) %>% 
  dplyr::filter(!(CCAA %in% c("Illes Balears", "Canarias")) & Tipo == "Parque Natural")

plot(enp$geometry)

# Resumen estadístico de los atributos
class(enp)
str(enp)
st_crs(enp)

aa <- st_difference(spain_peninsular_dissolved,st_union(enp), col = 'lightblue', add = TRUE)
st_difference(spain_peninsular_dissolved, enp)
ggplot() +
  geom_sf(data = spain)+
  geom_sf(data = aa)
#Utilizar mas de dos capas comparar crs
st_crs(endemism_sf) == st_crs(enp)

# Transformar el shapefile a 25830
enp <- sf::st_transform(enp, crs = 25830)
endemism_sf <- sf::st_transform(endemism_sf, crs = 25830)
malla_puntos <- malla %>%
  mutate(num_intersec = lengths(st_intersects(malla, endemism_sf))) %>% 
  filter(num_intersec > 0)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = malla_puntos, aes(fill = Especie))

malla_point <- st_join(enp, malla_puntos,join = st_disjoint)
malla_point <- st_intersection(enp, malla_puntos)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = malla_point, aes(fill = Tipo))


