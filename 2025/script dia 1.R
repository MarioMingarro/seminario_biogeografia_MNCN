# Vector numérico
num <- c(1, 2, 3, 4, 5)
# Vector de caracteres
car <- c("A", "Lu", "cc", "Sof")
# Matriz
mat <- matrix(1:4, 
              nrow = 4, 
              ncol=2)
# Data frame
df <- data.frame(
  car = c("A", "Lu", "cc", "Sof"),
  num = c(1, 2, 3, 4))

# Lista
lis <- list(
  num = c(1, 2, 3, 4),
  car = c("A", "Lu", "cc", "Sof"),
  mat = matrix(1:4, 
                nrow = 4, 
                ncol=2))


sumaresta <- function(a, b) {
  print(a + b)
  print(a - b)
}

sumaresta(3, 5) 


getwd()  # Muestra la ruta actual del directorio de trabajo

setwd("C:/ruta/a/tu/carpeta") # Modifica la ruta actual del directorio de trabajo

list.files()  # Muestra los archivos y carpetas en el directorio actual

dir.create("nueva_carpeta") # Crear una nueva carpeta (directorio)

dir.exists("nueva_carpeta")  #  Comprobar si un directorio existe


# Vector numérico
df <- data.frame(
  car = c("A", "Lu", "cc", "Sof"),
  num = c(1, 2, 3, 4))

df

write.csv(df, "archivo.csv") # Exportar como .csv
df_2 <- read.csv("C:/ruta/a/tu/carpeta/archivo.csv") # Importar como .csv


EJ_1

getwd()
dir.exists("C:/intro_r")
dir.create("C:\intro_r")
dir.exists("C:/intro_r")
setwd("C:/intro_r")
list.files()


library(readxl)
library(dplyr)
datos_intro_r <- read_xlsx("C:/intro_r/datos_intro_r.xlsx")





# --- FILTRADO DE DATOS --- 

# Filtrar datos donde el año sea exactamente 1901
datos_1901 <- filter(datos_intro_r, year == 2000)

# Filtrar datos donde el año esté entre 1901 y 2000 (ambos incluidos)
datos_1901_2000 <- filter(datos_intro_r, year >= 1901 & year <= 2000)

# Filtrar datos donde el año sea posterior a 1950 y Tx sea mayor a 0.6
datos_and <- filter(datos_intro_r, year > 1950 & Tx > 0.6)

# Filtrar datos donde el año sea posterior a 1950 o Tn sea menor a 0.5
datos_or <- filter(datos_intro_r, year > 1950 | Tn < 0.5)

# Filtrar datos donde el año sea posterior a 1950 y se cumpla al menos una de estas condiciones: Tx > 0.6 o Tn < 0.5
datos_comb <- filter(datos_intro_r, year > 1950 & (Tx > 0.6 | Tn < 0.5))

# Filtrar datos donde el año sea 1901, 1950 o 2000
datos_especificos <- filter(datos_intro_r, year %in% c(1901, 1950, 2000))

# Filtrar datos donde Tx no sea NA
datos_sin_na <- filter(datos_intro_r, !is.na(Tx))


# --- SELECCIÓN DE COLUMNAS --- 

# Seleccionar un rango de columnas consecutivas (de year a Long)
datos_rango <- select(datos_intro_r, year:Long)

# Seleccionar un conjunto específico de columnas usando `c()`
datos_combinado <- select(datos_intro_r, c(year, month, Tx, Tn))

# Seleccionar todas las columnas excepto Tx y Tn
datos_sin_temp <- select(datos_intro_r, !(Tx:Tn))

# Seleccionar columnas que cumplan dos condiciones: comiencen con "L" y contengan "n"
datos_interseccion <- select(datos_intro_r, starts_with("L") & contains("n"))

# Seleccionar columnas que cumplan al menos una de dos condiciones: comiencen con "T" o terminen en "r" 
datos_union <- select(datos_intro_r, starts_with("T") | ends_with("r"))

# Seleccionar columnas cuyos nombres coincidan con una expresión regular (ejemplo: nombres con exactamente dos caracteres)
datos_regex <- select(datos_intro_r, matches("^..$"))

# Seleccionar columnas que coincidan con nombres almacenados en un vector usando `all_of()`
cols <- c("year", "Tx", "Tn")
datos_all_of <- select(datos_intro_r, all_of(cols))

# Seleccionar columnas según una condición aplicada a los datos (ejemplo: seleccionar solo columnas numéricas)
datos_numericos <- select(datos_intro_r, where(is.numeric))


# --- CREACIÓN Y MODIFICACIÓN DE VARIABLES --- 

# Crear una nueva columna con la media de Tx y Tn
datos_mutados <- mutate(datos_intro_r, T_prom = (Tx + Tn) / 2)

# Crear una nueva columna categorizando los valores de Tx
datos_categorias <- mutate(datos_intro_r, Tx_categoria = ifelse(Tx > 0.6, "Alta", "Baja"))

# Modificar una columna existente (convertir year en factor)
datos_factor <- mutate(datos_intro_r, year = as.factor(year))


# --- ORDENACIÓN DE DATOS --- 

# Ordenar los datos de menor a mayor según el año
datos_ordenados_asc <- arrange(datos_intro_r, year)

# Ordenar los datos de mayor a menor según Tx
datos_ordenados_desc <- arrange(datos_intro_r, desc(Tx))

# Ordenar primero por año y luego por Tx de mayor a menor
datos_ordenados_multi <- arrange(datos_intro_r, year, desc(Tx))


# --- AGRUPACIÓN Y RESUMEN DE DATOS --- 

# Agrupar los datos por especie
datos_agrupados <- group_by(datos_intro_r, specie)

# Calcular la cantidad de registros por especie
datos_conteo <- summarize(datos_agrupados, conteo = n())

# Calcular la temperatura promedio por especie
datos_resumen <- summarize(datos_agrupados, Tx_promedio = mean(Tx, na.rm = TRUE))

# Calcular la temperatura máxima y mínima por año con la cantidad de registros por año
datos_estadisticos <- summarize(group_by(datos_intro_r, year), 
                                Tx_max = max(Tx, na.rm = TRUE), 
                                Tx_min = min(Tx, na.rm = TRUE),
                                conteo = n())


# --- OTRAS FUNCIONES --- 

# Renombrar las columnas 'Tx' y 'Tn'
datos_renombrados <- rename(datos_intro_r, temperatura_maxima = Tx, temperatura_minima = Tn)

# Obtener los valores únicos de la columna 'year'
datos_unicos <- distinct(datos_intro_r, year)

# Obtener un conjunto de filas únicas según múltiples columnas
datos_unicos_multiple <- distinct(datos_intro_r, year, month)

# Crear una nueva columna categorizando los valores de Tx en tres grupos usando `case_when`
datos_categorias_avanzado <- mutate(datos_intro_r, 
                                    Tx_categoria = case_when(
                                      Tx > 0.6 ~ "Alta",
                                      Tx <= 0.6 & Tx > 0.4 ~ "Media",
                                      TRUE ~ "Baja"))

# Realizar un left join entre dos dataframes por la columna "id"
datos_unidos <- left_join(datos_intro_r, datos_categorias_avanzado, by = "specie")

# Reemplazar los valores NA en la columna Tx por 0
datos_reemplazados <- mutate(datos_intro_r, Tx = replace(Tx, is.na(Tx), 0))

## PIPE ----

resultado <- filter(datos_intro_r, year > 1950)
resultado <- arrange(resultado, Tx)


resultado <- datos_intro_r %>%  filter(year > 1950) %>%  arrange(Tx)



# EJERCICIO 2 -----




# Definir la función
celsius_to_fahrenheit <- function(celsius) { # Crea objeto (función)
  return (celsius * 9/5) + 32 # Devuelve la conversión de la temperatura de Celsius a Fahrenheit
}

# Aplicar la función a la columna Tx
datos_intro_r %>% # Partiendo de datos_intro_r, mediante pipe pasa al siguiente paso
  mutate(Tx_F = celsius_to_fahrenheit(Tx)) # Añade nueva columa al aplicar la función sobre la columna Tx

datos_intro_r %>%  # Partiendo de datos_intro_r, mediante pipe pasa al siguiente paso
  filter(year > 1950) %>%  # Filtra los datos para incluir solo años posteriores a 1950
  group_by(specie) %>%  # Agrupa los datos por especie
  summarize(mean_Tx_F = mean(Tx_F, na.rm = TRUE))  # Calcula la temperatura máxima promedio por especie, ignorando NA

datos_intro_r %>%  # Partiendo de datos_intro_r, mediante pipe pasa al siguiente paso
  group_by(specie) %>%  # Agrupa los datos por la columna "specie"
  summarize(sd_Tx = sd(Tx, na.rm = TRUE)) %>%  # Calcula la desviación estándar de "Tx" para cada especie, ignorando NA
  arrange(desc(sd_Tx)) %>%  # Ordena las especies en orden descendente según la desviación estándar de "Tx"
  top_n(1, sd_Tx)  # Selecciona la especie con la mayor desviación estándar de "Tx"


datos_intro_r %>%
  mutate(decada = (year %/% 10) * 10) %>%  # Crear una nueva columna "decada", dividiendo el año entre 10 y multiplicando por 10
  group_by(specie, decada) %>%  # Agrupar los datos por especie y década
  summarize(mean_Tx = mean(Tx, na.rm = TRUE)) %>%  # Calcular la temperatura máxima promedio en cada década, ignorando valores NA
  arrange(specie, decada) %>%  # Ordenar los resultados por especie y década en orden ascendente
  group_by(specie) %>%  # Agrupar nuevamente por especie para calcular la variación entre décadas
  mutate(change_Tx = mean_Tx - lag(mean_Tx)) %>%  # Calcular la diferencia con la temperatura promedio de la década anterior
  ungroup() %>%  # Desagrupar los datos para evitar cálculos no deseados en los siguientes pasos
  filter(!is.na(change_Tx)) %>%  # Eliminar la primera fila de cada especie, ya que no tiene una década previa para comparar
  arrange(desc(change_Tx))  # Ordenar los resultados de mayor a menor aumento en temperatura

