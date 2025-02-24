library(sf)
library(dplyr)

test <- sf::st_read("C:/A_TRABAJO/KK/test.shp")

resultado <- test %>%
  group_by(geometry) %>%
  summarise(
    ad_1_agg = paste(ad_1_agg, collapse = "; "),
    ad_2_agg = paste(ad_2_agg, collapse = "; "),
    ad_3_agg = paste(ad_3_agg, collapse = "; "),
    ad_4_agg = paste(ad_4_agg, collapse = "; "),
    ad_5_agg = paste(ad_5_agg, collapse = "; "),
    pdr_agg = paste(pdr_agg, collapse = "; "),
    n = n()
  ) %>%
  st_as_sf()

sf::write_sf(resultado, "C:/A_TRABAJO/KK/resultado.shp")






# Identificar geometrías duplicadas
geometrias_unicas <- unique(st_geometry(test))

# Convertir a sfc y asignar CRS
geometrias_unicas_sfc <- st_sfc(geometrias_unicas, crs = st_crs(test))

# Crear un sf vacío para almacenar los resultados
geometrias_unicas <- st_sf(geometry = geometrias_unicas_sfc)
sf::write_sf(geometrias_unicas, "C:/A_TRABAJO/KK/test_unicas.shp" )


str(test)
str(geometrias_unicas)


# Agrupar por geometría y agregar los datos de ad_agg
resultado <- test %>%
  group_by(geometry) %>%
  summarise(
    ad_1_agg = paste(ad_1_agg, collapse = ", "),
    ad_2_agg = paste(ad_2_agg, collapse = ", "),
    ad_3_agg = paste(ad_3_agg, collapse = ", "),
    ad_4_agg = paste(ad_4_agg, collapse = ", "),
    ad_5_agg = paste(ad_5_agg, collapse = ", "),
    pdr_agg = paste(pdr_agg, collapse = ", "),
    n = n()
  ) %>%
  st_as_sf()

# Mostrar el resultado
print(resultado)

test %>%
  group_by(geometry) %>%
  summarise(ad_1_agg)
