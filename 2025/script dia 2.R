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


endemism <- read.csv("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")
library(sf)
enp <- st_read("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/enp/Enp2023.shp")

enp <- enp %>% 
  filter(FIGURA_LP=="Parque Natural")

library(tmap)
endemism_sf <- endemism %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

library(ggplot2)
library(sf)
library(dplyr)

# Cargar datos de especies seleccionadas
endemism <- read.csv("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/endemism_seleccionados.csv")

# Cargar shapefile de ENP (Espacios Naturales Protegidos)
enp <- st_read("C:/A_TRABAJO/CURSO_FORMACION_CSIC/Formacion_CSIC_2025/DATA/enp/Enp2023.shp")

# Asegurar que las coordenadas sean numéricas y preparar los datos
endemism <- endemism %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE)) %>%
  select(ESPECIE.2017, LATITUDE, LONGITUDE) %>%  # Quedarse solo con las columnas necesarias
  distinct(LATITUDE, LONGITUDE, .keep_all = TRUE)  # Eliminar duplicados en latitud y longitud

# Convertir a objeto sf para las especies
endemism_sf <- endemism %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Crear el mapa con ggplot
ggplot() +
  # Añadir los polígonos de los ENP
  geom_sf(data = enp, fill = "transparent", color = "darkgreen", size = 0.5) +
  # Añadir los puntos de las especies
  geom_sf(data = endemism_sf, aes(color = ESPECIE.2017), size = 1, shape = 16, alpha = 0.7) +
  # Ajustar límites de la vista
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45), expand = FALSE) + 
  # Añadir título y tema
  ggtitle("Distribución de especies en la Península Ibérica con ENP") +
  theme_minimal() +
  theme(legend.title = element_blank())  # Opcional: eliminar el título de la leyenda

library(readxl)
library(dplyr)
datos_intro_r <- read_xlsx("C:/intro_r/datos_intro_r.xlsx")
datos_sf <- datos_intro_r %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

plot(datos_sf)

library(ggplot2)

ggplot(datos_sf, aes(x = Long, y = Lat)) +
  geom_point(color = "red", size = 2) +
  borders("world", colour = "gray50", fill = "lightgray") +  # Agregar mapa base
  theme_minimal()


library(sf)
length(ls("package:sf"))

# Carga y Exploración de Datos Espaciales -----

# Cargar un archivo shapefile
shapefile <- st_read("ruta/al/archivo.shp")

# Tipo de objeto
class(shapefile)

# Dimensiones (número de filas y columnas)
dim(shapefile)

# Tipo de geometría (puntos, líneas o polígonos)
st_geometry(shapefile)

# Sistema de referencia de coordenadas (CRS)
st_crs(shapefile)

# Resumen general de los datos
summary(shapefile)


library(dplyr)

# Ver las primeras filas del dataset
head(shapefile)

# Seleccionar columnas específicas
shapefile %>% select(Nombre, Tipo)

# Filtrar elementos específicos
shapefile %>% filter(Tipo == "Bosque")

# Crear una nueva variable derivada
shapefile <- shapefile %>% mutate(Area_km2 = st_area(geometry) / 1e6)

# Proyecciones y Sistemas de Coordenadas -----
# Ver el CRS del shapefile cargado
st_crs(shapefile)

# Asignar un CRS conocido (Ejemplo: WGS 84 - EPSG:4326)
st_crs(shapefile) <- 4326

# Transformar a un CRS proyectado (Ejemplo: UTM zona 33N - EPSG:32633)
shapefile_utm <- st_transform(shapefile, crs = 32633)

# Verificar la nueva proyección
st_crs(shapefile_utm)

