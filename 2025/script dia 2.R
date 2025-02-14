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







# Convertir a objeto sf
datos_sf <- muestra %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  mutate(Long = st_coordinates(.)[,1], Lat = st_coordinates(.)[,2])

# Filtrar solo puntos dentro de la Península Ibérica (aprox.)
datos_sf <- datos_sf %>%
  filter(Lat >= 35 & Lat <= 45, Long >= -10 & Long <= 5)

# Mapa con ggplot
ggplot() +
  borders("world", regions = c("Spain", "Portugal"), colour = "gray50", fill = "lightgray") +  # Fondo mapa
  geom_point(data = datos_sf, aes(x = Long, y = Lat, color = ESPECIE.2017), size = 2) +  # Puntos por especie
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45), expand = FALSE) +  # Zoom en la península
  theme_minimal() +
  ggtitle("Distribución de 5 especies seleccionadas en la Península Ibérica") +
  theme(legend.title = element_blank())  # Ocultar título de la leyenda


##########################

library(sf)
library(dplyr)
library(ggplot2)

endemism <- read.csv("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")

# Convertir a objeto sf
endemism_sf <- st_as_sf(endemism, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Leer el archivo shapefile de enp
enp <- st_read("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/enp/Enp2023.shp")


# Resumen estadístico de los atributos
class(enp)
summary(enp)
st_crs(endemism_sf)
crs(endemism_sf)


enp <- enp %>%
  dplyr::select(SITENAME, FIGURA_LP, AREA_HA, CCAA_N_ENP) %>% 
  dplyr::filter(!(CCAA_N_ENP %in% c("Illes Balears", "Canarias")) & FIGURA_LP == "Parque Natural")


# Transformar el shapefile a WGS84 (si no está en este sistema)
enp <- sf::st_transform(enp, crs = 4326)
endemism_sf <- sf::st_transform(endemism_sf, crs = 4326)


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
  geom_sf(data = endemism_sf, aes(color = ESPECIE.2017))

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
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp, color = "green4") +
  geom_sf(data = endemism_sf, color = "red")+
  coord_sf(xlim = st_bbox(spain_peninsular_dissolved)[c("xmin", "xmax")],
           ylim = st_bbox(spain_peninsular_dissolved)[c("ymin", "ymax")],
           expand = FALSE) 


puntos_en_spain <- st_intersection(endemism_sf, spain_peninsular)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = puntos_en_spain, color = "green4")

endemism_sf <- endemism_sf %>%
  rename(spp = ESPECIE.2017)


# Intenta hacer que las geometrías sean válidas
endemism_sf <- st_make_valid(endemism_sf)
enp <- st_make_valid(enp)
puntos_en_enp <- st_intersection(endemism_sf, enp)

ggplot() +
  geom_sf(data = spain_peninsular_dissolved)+
  geom_sf(data = enp)+
  geom_sf(data = endemism_sf, color = "red")+
  geom_sf(data = puntos_en_spain, color = "green4")+
  geom_sf(data = puntos_en_enp, color = "blue")+
  geom_sf(data = enp_puntos, color = "gray2")

enp_puntos <- st_intersection(enp,endemism_sf)

enp_puntos <- st_intersects(enp, endemism_sf)
enp_puntos <- lengths(enp_puntos) > 0
enp_puntos <- enp[enp_puntos, ]

enp_puntos <- enp %>%
  mutate(intersecta = lengths(st_intersects(enp, endemism_sf)) > 0) %>%  # Verifica si hay intersección
  filter(intersecta == TRUE)  # Filtra los polígonos que tienen intersección con puntos
