library(sf)
library(dplyr)
library(ggplot2)
library(geodata)
library(terra)

#Carga capa desde csv

endemismos <- read.csv2("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")

# se comprueba su estructura

str(endemismos)

# Convertir a objeto sf

endemismos <- st_as_sf(endemismos, coords = c("Longitud", "Latitud"), crs = 4326)

# Comprobar el crs
st_crs(endemismos) 

# Transformar el shapefile a ETRS89 30N (EPSG: 25830)
endemismos <- sf::st_transform(endemismos, crs = 25830)



# Mapa básico  con ggplot
ggplot() +
  geom_sf(data = endemismos, color = "red")

# Mapa combinado con ENP y endemismos
ggplot() +
  geom_sf(data = endemismos, aes(color = Especie))

#Añadir mas datos utilizando funciones de paquetes



# Cargar datos de España usando geodata
spain <- geodata::gadm(country = "ESP", level = 1, path = tempdir()) # Nivel 1 para provincias
spain <- sf::st_as_sf(spain)

# Transformar la proyección si es necesario (asumiendo que endemismos está en 25830)
st_crs(spain) == st_crs(endemismos)
spain <- sf::st_transform(spain, crs = 25830)
st_crs(spain) == st_crs(endemismos)


ggplot() +
  geom_sf(data = spain)+
  geom_sf(data = endemismos, color = "red")


# Ejercicio 1
str(spain)
unique(spain$NAME_1)

spain_peninsular <- spain %>%
  dplyr::select(CCAA = NAME_1) %>%
  dplyr::filter(!CCAA %in% c("Islas Baleares", "Islas Canarias", "Ceuta y Melilla"))

ggplot() +
  geom_sf(data = spain_peninsular)+
  geom_sf(data = endemismos, color = "red")

#######

spain_peninsular_mask <- st_union(spain_peninsular)
spain_peninsular_mask <- sf::st_as_sf(spain_peninsular_mask)

ggplot() +
  geom_sf(data = spain_peninsular, color = "green4") +
  geom_sf(data = spain_peninsular_mask, fill= "transparent", color = "black", linewidth  = 1)+
  geom_sf(data = endemismos, color = "red")

world_map <- geodata::world(path=tempdir(), resolution = 2) # Puedes ajustar la resolución
world_map <- sf::st_as_sf(world_map)


ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_sf(data = endemismos, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


# Crear malla 
malla <- st_make_grid(spain_peninsular_mask, cellsize = 50000, square = FALSE)
malla <- sf::st_as_sf(malla)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = endemismos, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


malla <- st_intersection(malla, spain_peninsular_mask)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = endemismos, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()



# Ejercicio 2 ----
# Seleccionar los registros dentro de España peninsular 
endemismos_spain <- st_intersection(endemismos, spain_peninsular_mask)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_sf(data = endemismos, color = "green")+
  geom_sf(data = endemismos_spain, color = "red")+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()

#---------

malla_puntos <- st_join(malla, endemismos_spain, join = st_intersects)
malla_puntos <- malla_puntos %>%
  filter(Especie > 0)

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemismos_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


malla_puntos <- malla_puntos %>% mutate(area_km2 = as.numeric(st_area(.)/1000000))




# RAster
# Descargar un DEM (Digital Elevation Model) de la región de España

dem <- geodata::elevation_30s(country = "ESP", path=tempdir())

# Cambiar resolucion a 0,25º
dem <-  terra::aggregate(dem, 3, fun = "median")

dem_df <- as.data.frame(dem, xy = TRUE)

# 2. Renombrar las columnas 
colnames(dem_df) <- c("x", "y", "elevation") # Asumiendo que tu raster tiene una sola capa llamada "elevation"

# 3. Usar ggplot2 para trazar el raster
ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation))+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemismos_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()



# 3. Sistemas de referencia de coordenadas (CRS)
# Verificar el CRS de los datos. siempre mas facil proyectar vector que raster
terra::crs(dem)

# Reproyección de datos vectoriales suele ser más rápida que la de rasteres
spain_peninsular_vect <- sf::st_transform(spain_peninsular_mask, crs = 4326)
spain_peninsular_vect <- terra::vect(spain_peninsular_vect)


# 4. Recortar datos raster con máscara y extensión
# Recortar el DEM usando la extensión de España
dem_recortado <- terra::crop(dem, spain_peninsular_vect)
dem_recortado <- terra::mask(dem_recortado, spain_peninsular_vect)
dem_recortado_df <- as.data.frame(dem_recortado, xy = TRUE)
colnames(dem_recortado_df) <- c("x", "y", "elevation")

ggplot() +
  geom_sf(data = world_map , fill = "gray30", color = "black")+
  geom_sf(data = spain_peninsular_mask, color = "black", linewidth  = 1)+
  geom_raster(data = dem_recortado_df, aes(x = x, y = y, fill = elevation))+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)+
  geom_sf(data = endemismos_spain, color = "red", alpha = .4)+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44)) +
  theme_light()


# Descargar datos climáticos (temperatura máxima mensual) de la misma región
tmax <- geodata::worldclim_country(country = "ESP", var = "tmax", res = 0.5, path=tempdir())
tmin <- geodata::worldclim_country(country = "ESP", var = "tmin", res = 0.5, path=tempdir())

#visualizar rápido raster
plot(tmax)

tmax <- mean(tmax)
tmin <- mean(tmin)

plot(tmax)

tmed <- (tmax + tmin) /2

plot(tmed)

# convertir a spatvector para trabjar mejor con terra
malla_puntos_vect <- terra::vect(malla_puntos)

# Verificar el CRS de los datos
terra::crs(tmed) == terra::crs(malla_puntos_vect)

tmed <- terra::project(tmed, terra::crs(malla_puntos_vect))

# Calcular la elevación media por cuadricula
tmed_malla <- terra::zonal(tmed, malla_puntos_vect, fun = "median")

malla_puntos <- mutate(malla_puntos, T_med = tmed_malla$mean)

head(malla_puntos)

# Presentación resultados
library(gridExtra)

# Mapa existente
mapa <- ggplot() +
  geom_sf(data = world_map, fill = "gray30", color = "black") +
  geom_raster(data = dem_recortado_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = malla_puntos, aes(col = Especie),fill = "transparent", linewidth = 1) +
  geom_sf(data = endemismos_spain, color = "red", alpha = 0.4) +
  geom_sf(data = spain_peninsular_mask, color = "black", fill = "transparent", linewidth = 1) +
  coord_sf(xlim = c(-10, 4), ylim = c(35, 44)) +
  theme_light()+
  scale_fill_gradient(low = "white", high = "black")

# Boxplot
boxplot_plot <- ggplot() +
  geom_boxplot(data = malla_puntos, aes(x = Especie, y = T_med, fill = Especie)) +
  theme(
    panel.background = element_rect(fill = "gray80"), # Fondo gris oscuro
    plot.background = element_rect(fill = "gray20"), # Fondo del gráfico gris oscuro
    legend.position = "none",
    line = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(color = "white"), # Titulo eje y blanco
    axis.text.y = element_text(color="white") # texto eje y blanco
  ) +
  labs(y = "ºC")

boxplot_grob <- ggplotGrob(boxplot_plot)

# Ajustar la posición del boxplot en la parte inferior del mapa
mapa + 
  annotation_custom(grob = boxplot_grob, 
                    xmin = -10, xmax = 4, 
                    ymin = 34.6, # Ajustar la posición vertical (20% del rango del eje y)
                    ymax = 35.8) # Ajustar la altura del boxplot











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

aa <- st_difference(spain_peninsular_mask,st_union(enp), col = 'lightblue', add = TRUE)
st_difference(spain_peninsular_mask, enp)
ggplot() +
  geom_sf(data = spain)+
  geom_sf(data = aa)
#Utilizar mas de dos capas comparar crs
st_crs(endemismos) == st_crs(enp)

# Transformar el shapefile a 25830
enp <- sf::st_transform(enp, crs = 25830)
endemismos <- sf::st_transform(endemismos, crs = 25830)
malla_puntos <- malla %>%
  mutate(num_intersec = lengths(st_intersects(malla, endemismos))) %>% 
  filter(num_intersec > 0)

ggplot() +
  geom_sf(data = spain_peninsular_mask)+
  geom_sf(data = endemismos, color = "red")+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)

ggplot() +
  geom_sf(data = spain_peninsular_mask)+
  geom_sf(data = malla_puntos, aes(fill = Especie))

malla_point <- st_join(enp, malla_puntos,join = st_disjoint)
malla_point <- st_intersection(enp, malla_puntos)

ggplot() +
  geom_sf(data = spain_peninsular_mask)+
  geom_sf(data = malla_point, aes(fill = Tipo))


