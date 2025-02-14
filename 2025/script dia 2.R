library(dplyr)
library(sf)
library(ggplot2)


# Cargar datos
endemism <- read.csv("C:/Users/mario/Dropbox/CURSO_FORMACION_CSIC/Curso_formacion_csic/endemism.csv")

# Asegurar que las coordenadas sean numéricas
endemism <- endemism %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE)) %>%
  select(ESPECIE.2017, LATITUDE, LONGITUDE) %>%  # Quedarse solo con las columnas necesarias
  distinct(LATITUDE, LONGITUDE, .keep_all = TRUE)  # Eliminar duplicados en latitud y longitud

# Seleccionar 5 especies únicas al azar
especies_seleccionadas <- endemism %>%
  distinct(ESPECIE.2017) %>%
  slice_sample(n = 3)

# Filtrar datos con esas especies
datos_filtrados <- endemism %>%
  filter(ESPECIE.2017 %in% especies_seleccionadas$ESPECIE.2017)

# Seleccionar 1000 registros aleatorios
set.seed(123)  # Fijar semilla para reproducibilidad
muestra <- datos_filtrados %>%
  slice_sample(n = 100)

# Exportar los datos seleccionados a un archivo CSV
write.csv(muestra, "endemism_seleccionados.csv", row.names = FALSE)



##########################

library(sf)
library(dplyr)
library(ggplot2)

endemism <- read.csv2("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")

# Convertir a objeto sf
endemism_sf <- st_as_sf(endemism, coords = c("Longitud", "Latitud"), crs = 4326)

# Leer el archivo shapefile de enp
enp <- st_read("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/enp/Enp2023.shp")


# Resumen estadístico de los atributos
class(enp)
summary(enp)
st_crs(endemism_sf)



enp <- enp %>%
  dplyr::select(SITENAME, FIGURA_LP, AREA_HA, CCAA_N_ENP) %>% 
  dplyr::filter(!(CCAA_N_ENP %in% c("Illes Balears", "Canarias")) & FIGURA_LP == "Parque Natural")


# Transformar el shapefile a WGS84 (si no está en este sistema)
enp <- sf::st_transform(enp, crs = 25830)
endemism_sf <- sf::st_transform(endemism_sf, crs = 25830)


# Mapa básico de ENP
ggplot() +
  geom_sf(data = enp)

# Mapa de endemismos con puntos
ggplot() +
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red") # Puedes añadir color según un atributo

# Mapa combinado con ENP y endemismos
ggplot() +
  geom_sf(data = enp) +
  geom_sf(data = endemism_sf, aes(color = Especie))

library(rnaturalearth)
spain <- ne_states(country = "spain")
world <- ne_countries(scale = 10)

ggplot() +
  geom_sf(data = spain)+
  geom_sf(data = enp, color = "green4") +
  geom_sf(data = endemism_sf, color = "red")

spain_peninsular <- spain %>%
    dplyr::filter(!region %in% c("Islas Baleares", "Canary Is.", "Ceuta", "Melilla")) 

ggplot() +
  geom_sf(data = spain_peninsular)+
  geom_sf(data = enp, color = "green4") +
  geom_sf(data = endemism_sf, color = "red")

spain_peninsular_dissolved <- st_union(spain_peninsular)
ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp, color = "green4") +
  geom_sf(data = endemism_sf, color = "red")

ggplot() +
  geom_sf(data = world , fill = "gray30")+
  geom_sf(data = spain_peninsular_dissolved, color = "black", size = 4)+
  geom_sf(data = enp, color = "darkgreen", fill = "green4", alpha = .2) +
  geom_sf(data = endemism_sf, color = "red")+
  coord_sf(xlim = c(-10, 4),
           ylim = c(35,44),
           expand = FALSE) +
  theme_minimal()


puntos_en_spain <- st_intersection(endemism_sf, spain_peninsular)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = puntos_en_spain, color = "green4")

endemism_sf <- endemism_sf %>%
  rename(spp = ESPECIE.2017)


malla <- st_make_grid(
  st_as_sfc(st_bbox(endemism_sf), crs = 25830)
)



malla <- st_make_grid(endemism_sf_m, cellsize = 50000, square = FALSE)
class(malla)
malla <- sf::st_as_sf(malla)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = puntos_en_spain, color = "green4")+
  geom_sf(data = malla, fill = "transparent")


malla_puntos <- st_intersects(malla, endemism_sf)
malla_puntos <- lengths(malla_puntos) > 0
malla_puntos <- malla[malla_puntos, ]

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = puntos_en_spain, color = "green4")+
  geom_sf(data = malla, fill = "transparent")+
  geom_sf(data = malla_puntos, fill = "blue", alpha = .2)

malla_puntos <- malla %>%
  mutate(intersecta = lengths(st_intersects(malla, endemism_sf)) > 0) %>%  # Verifica si hay intersección
  filter(intersecta == TRUE)  # Filtra los polígonos que tienen intersección con puntos

# Intenta hacer que las geometrías sean válidas
endemism_sf <- st_make_valid(endemism_sf)
enp <- st_make_valid(enp)
puntos_en_enp <- st_intersection(endemism_sf, enp)


enp_puntos <- st_intersects(enp, endemism_sf)
enp_puntos <- lengths(enp_puntos) > 0
enp_puntos <- enp[enp_puntos, ]

enp_puntos <- enp %>%
  mutate(intersecta = lengths(st_intersects(enp, endemism_sf)) > 0) %>%  # Verifica si hay intersección
  filter(intersecta == TRUE)  # Filtra los polígonos que tienen intersección con puntos
