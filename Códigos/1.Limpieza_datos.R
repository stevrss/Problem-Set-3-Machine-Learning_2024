#------------------------------------------------------------------------------#
#---------------------- LIMPIEZA DATA - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

#Autor: Juliet Molano

#-------------------------------------------------------------------------------
# 1. Cargar base de datos de entrenamiento y testeo ----------------------------
#-------------------------------------------------------------------------------

  setwd(paste0(wd,"/Datos")) #Directorio datos 

  ### Base de datos de entrenamiento

  train <- import("train.csv") 
  length(train) # 16 variables 
  nrow(train) #38.644 observaciones   
  
  ### Base de datos de testeo
  
  test <- import("test.csv") # Base de testeo
  length(test) # 16 variables 
  nrow(test) #10.286 observaciones   
  
#-------------------------------------------------------------------------------
# 2. Mirar las variables que contiene cada base  -------------------------------
#-------------------------------------------------------------------------------

  #Base de entrenamiento
  names(train)  
  str(train)  
  table(train$property_type) #Solo hay apartamentos y casas
  table(train$city) #Solo Bogota
  table(train$operation_type) #Solo venta
  
  #Base de test
  names(test) 
  str(test)  # variables de precio está en NA
  table(test$property_type) #Solo hay apartamentos y casas
  table(test$city) #Solo Bogota
  table(test$operation_type) #Solo venta
  
  #-------------------------------------------------------------------------------
  # 3. Revisar missings de las variables -----------------------------------------
  #-------------------------------------------------------------------------------
  
  # Base de entrenamiento ------------------------------------------------------
  
  # Gráficas de missings
  
  # Forma 1: Gráfica básica para visualizar missing por tipo de dato
  g1 <- vis_dat(train) +
    scale_fill_manual(name = "TIpo",
                      values = c(
      "character" = "#B0C4DE", # Azul claro para datos tipo texto
      "integer" = "#87CEEB",  # Azul cielo para datos tipo entero
      "numeric" = "#4682B4",  # Azul acero para datos numéricos
      "NA" = "#D3D3D3"       # Gris claro para valores faltantes
    )) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) # Ajustar etiquetas del eje X
  g1
  setwd(paste0(wd,"/Graficas")) # Establecer el directorio de salida para gráficos
  ggsave("graf_miss_f1_train.png", plot = g1, width = 10, height = 8, dpi = 300)
  
  # Forma 2: Gráfica con etiquetas descriptivas y eje X ajustado
  g2 <- vis_dat(train) +
    scale_fill_manual(
      name = "Tipo", # Cambiar título de la leyenda a "Tipo"
      values = c(
        "character" = "#B0C4DE", # Azul claro
        "integer" = "#87CEEB",  # Azul cielo
        "numeric" = "#4682B4",  # Azul acero
        "NA" = "#D3D3D3"        # Gris claro
      )
    ) +
    labs(
      y = "Observaciones", # Etiqueta del eje Y
      fill = "Tipo"
    ) +
    scale_x_discrete(
      labels = c(
        "property_id" = "ID Propiedad",
        "city" = "Ciudad",
        "property_type" = "Tipo Propiedad",
        "operation_type" = "Tipo Operación",
        "title" = "Título",
        "description" = "Descripción",
        "month" = "Mes",
        "year" = "Año",
        "surface_total" = "Superficie Total",
        "surface_covered" = "Superficie Cubierta",
        "rooms" = "Habitaciones",
        "bathrooms" = "Baños",
        "bedrooms" = "Dormitorios",
        "price" = "Precio",
        "lat" = "Latitud",
        "lon" = "Longitud"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Rotar etiquetas del eje X
      legend.title = element_text(face = "bold") # Título de la leyenda en negrita
    )
  g2
  ggsave("graf_miss_f2_train.png")
  
  # Tablas de porcentajes de missings
  db_miss <- skim(train) %>% dplyr::select(skim_variable, n_missing) # Extraer variables y sus missing
  Nobs <- nrow(train) # Número total de observaciones
  db_miss <- db_miss %>% 
    filter(n_missing != 0) %>% # Filtrar variables con valores faltantes
    mutate(p_missing = n_missing / Nobs) %>% # Calcular porcentaje de missing
    arrange(-n_missing) # Ordenar por número de missings en orden descendente
  db_miss
  
  # Base de testeo --------------------------------------------------------------
  
  # Gráficas de missings
  
  # Forma 1: Gráfica básica para el conjunto de test
  g3 <- vis_dat(test) +
    scale_fill_manual(
      name = "Tipo de dato", # Nombre de la leyenda
      values = c(
        "character" = "#B0C4DE", # Azul claro
        "integer" = "#87CEEB",  # Azul cielo
        "numeric" = "#4682B4",  # Azul acero
        "NA" = "#D3D3D3"       # Gris claro
      ))
  setwd(paste0(wd, "/Graficas")) # Establecer el directorio de salida
  ggsave("graf1_miss_f1_test.png", plot = g3, width = 10, height = 8, dpi = 300)
  
  # Forma 2: Gráfica con etiquetas descriptivas para el conjunto de test
  g4 <- vis_dat(test) +
    scale_fill_manual(
      name = "Tipo", # Cambiar título de la leyenda a "Tipo"
      values = c(
        "character" = "#B0C4DE", # Azul claro
        "integer" = "#87CEEB",  # Azul cielo
        "numeric" = "#4682B4",  # Azul acero
        "NA" = "#D3D3D3"        # Gris claro
      )
    ) +
    labs(
      y = "Observaciones", # Etiqueta del eje Y
      fill = "Tipo"
    ) +
    scale_x_discrete(
      labels = c(
        "property_id" = "ID Propiedad",
        "city" = "Ciudad",
        "property_type" = "Tipo Propiedad",
        "operation_type" = "Tipo Operación",
        "title" = "Título",
        "description" = "Descripción",
        "month" = "Mes",
        "year" = "Año",
        "surface_total" = "Superficie Total",
        "surface_covered" = "Superficie Cubierta",
        "rooms" = "Habitaciones",
        "bathrooms" = "Baños",
        "bedrooms" = "Dormitorios",
        "price" = "Precio",
        "lat" = "Latitud",
        "lon" = "Longitud"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Rotar etiquetas del eje X
      legend.title = element_text(face = "bold") # Título de la leyenda en negrita
    )
  g4
  ggsave("graf_miss_f2_test.png", plot = g4)
  
  # Tablas de missings para el conjunto de test
  db_miss_test <- skim(test) %>% dplyr::select(skim_variable, n_missing) # Extraer variables y sus missings
  Nobs <- nrow(test) # Número total de observaciones
  db_miss_test <- db_miss_test %>% 
    filter(n_missing != 0) %>% # Filtrar variables con valores faltantes
    mutate(p_missing = n_missing / Nobs) %>% # Calcular porcentaje de missing
    arrange(-n_missing) # Ordenar por número de missings en orden descendente
  db_miss_test
  
  
  #-------------------------------------------------------------------------------
  # 4. Tratamiento de los missings -----------------------------------------------
  #-------------------------------------------------------------------------------
  
  # Base de entrenamiento ------------------------------------------------------
  
  ## Verificar el tipo de variables: `rooms` y `bathrooms`
  # Se utiliza `sapply` para confirmar que las variables tienen el tipo de dato esperado
  sapply(train[, c("rooms", "bathrooms")], class) # Identificar clases de las columnas seleccionadas
  
  ## Imputación de la moda (valor más frecuente) en las variables `rooms` y `bathrooms`
  # Agrupamos por `property_type` para imputar la moda por tipo de propiedad
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      rooms_imp = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms), # Imputar moda en `rooms`
      bathrooms_imp = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms) # Imputar moda en `bathrooms`
    ) %>%
    ungroup() # Desagrupamos para evitar efectos no deseados en futuras operaciones
  
  ## Imputación de la media y la mediana para `surface_total` y `surface_covered`
  
  ### Imputación usando la media
  # Se imputan los valores faltantes de las variables `surface_total` y `surface_covered` por su media
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_imp_mean = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total), # Media para `surface_total`
      surface_covered_imp_mean = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered) # Media para `surface_covered`
    ) %>%
    ungroup()
  
  ### Imputación usando la mediana
  # Se imputan los valores faltantes de las variables `surface_total` y `surface_covered` por su mediana
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_median = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total), # Mediana para `surface_total`
      surface_covered_median = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered) # Mediana para `surface_covered`
    ) %>%
    ungroup()
  
  # Base de testeo -------------------------------------------------------------
  
  ## Imputación de la moda (valor más frecuente) en las variables `rooms` y `bathrooms`
  # Agrupamos por `property_type` para imputar la moda por tipo de propiedad en la base de testeo
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      rooms_imp = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms), # Imputar moda en `rooms`
      bathrooms_imp = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms) # Imputar moda en `bathrooms`
    ) %>%
    ungroup()
  
  ## Imputación de la media y la mediana para `surface_total` y `surface_covered`
  
  ### Imputación usando la media
  # Se imputan los valores faltantes de las variables `surface_total` y `surface_covered` por su media en la base de testeo
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_imp_mean = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total), # Media para `surface_total`
      surface_covered_imp_mean = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered) # Media para `surface_covered`
    ) %>%
    ungroup()
  
  ### Imputación usando la mediana
  # Se imputan los valores faltantes de las variables `surface_total` y `surface_covered` por su mediana en la base de testeo
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_median = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total), # Mediana para `surface_total`
      surface_covered_median = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered) # Mediana para `surface_covered`
    ) %>%
    ungroup()
  
  # Convertir a dataframes -----------------------------------------------------
  # Se convierten las bases `train` y `test` de vuelta a `data.frame` para compatibilidad con funciones que no usan `tibble`
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  
#-------------------------------------------------------------------------------
# 5. Mirar distribuciones de las variables numericas----------------------------
#-------------------------------------------------------------------------------  
  
  #Mirar distribuciones de variables numericas
  
    #Base train
  stargazer(train)
  stargazer(train,type="text")
  
   #Base test
  stargazer(test)
  stargazer(test,type="text")
  
  
#-------------------------------------------------------------------------------
# 6. Creacion de variables -----------------------------------------------------
#-------------------------------------------------------------------------------   
  
  #Transformacion a logaritmo
  train$Log_precio <- log(train$price)
  summary(train$price)
  
  # precio x metro cuadrado
  train <- train %>%
    mutate(precio_mt2 = round(price/surface_total_median,0))%>%
    mutate(precio_mt2 = precio_mt2/1000000)  # precio x Mt2 en millones
  summary(train$precio_mt2)
  stargazer(train["precio_mt2"],type="text")
  
  # estadísticas descriptivas 
  stargazer(train)
  stargazer(train,type="text")
  

#-------------------------------------------------------------------------------
# 7. Variables a partir de datos espaciales ------------------------------------
#-------------------------------------------------------------------------------

# Verificar valores faltantes en las coordenadas (lon y lat) -------------------

# Base train
sum(is.na(train$lon))  # Verificar valores NA en longitud (no hay NAs)
sum(is.na(train$lat))  # Verificar valores NA en latitud

# Base test
sum(is.na(test$lon))  # Verificar valores NA en longitud
sum(is.na(test$lat))  # Verificar valores NA en latitud

# Visualización inicial de los datos ------------------------------------------

# Base train: Visualizar datos espaciales en un mapa interactivo
leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = train$lon, lat = train$lat)

# Base test: Visualizar datos espaciales en un mapa interactivo
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, lat = test$lat)

# Georeferencia por localidades en Bogotá --------------------------------------
  #Fuente:https://bogota-laburbano.opendatasoft.com/explore/dataset/poligonos-localidades/export/

# Leer y preparar shapefile de localidades
setwd(paste0(wd, "\\Datos espaciales\\Localidades"))
local <- st_read("poligonos-localidades.geojson")
local <- subset(local, !(Nombre.de.la.localidad == "SUMAPAZ")) # Quitar Sumapaz (rural)
local <- st_transform(local, 4326) # Homogeneizar proyección


# Mapa básico de localidades
ggplot() +
  geom_sf(data = local, color = "black")

# Mapa resaltando solo Chapinero
ggplot() +
  geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
  scale_fill_manual(
    values = c(
      "CHAPINERO" = "#FF6F61", # Resaltar Chapinero
      .default = "#B0BEC5"     # Color gris para otras localidades
    )
  ) +
  labs(title = "Mapa de Localidades") +
  theme_minimal()

# Mapa con todas las localidades
ggplot() +
  geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
  labs(title = "Mapa de Localidades", fill = "Localidades") +
  theme_minimal()

# Convertir bases train y test en datos espaciales -----------------------------

sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) # Homogeneizar proyección
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Realizar la unión espacial para asignar localidad ---------------------------

# Base train
sf_train <- st_join(sf_train, local, join = st_intersects)
train$localidad <- sf_train$Nombre.de.la.localidad
table(is.na(train$localidad)) # Verificar si hay NA en localidad

# Base test
sf_test <- st_join(sf_test, local, join = st_intersects)
test$localidad <- sf_test$Nombre.de.la.localidad
table(is.na(test$localidad)) # Verificar si hay NA en localidad

# Mapas ------------------------------------------------------------------------

# Base train: Mapa con precio por metro cuadrado
ggplot() +
  geom_sf(data = local, color = "black") +
  geom_sf(data = sf_train, aes(color = precio_mt2), shape = 15, size = 0.3) +
  scale_color_gradient(
    name = "Precio m²",
    low = "lightblue", high = "darkblue"
  ) +
  theme_bw()

# Base test: Mapa básico con propiedades resaltadas
ggplot() +
  geom_sf(data = local, color = "black") +
  geom_sf(data = sf_test, shape = 15, size = 0.3, color = "orange") +
  theme_bw()

# Mapa por tipo de propiedad: Apartamentos y casas -----------------------------

# Base train
ggplot() +
  geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
  geom_sf(data = sf_train %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 0.8, alpha = 0.6) +
  geom_sf(data = sf_train %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 0.8, alpha = 0.8) +
  scale_fill_manual(
    name = "Localidad",
    values = c(
      "CHAPINERO" = "#ADD8E6", # Azul claro para Chapinero
      .default = "#FAFAFA"     # Gris claro para otras localidades
    )
  ) +
  scale_color_manual(
    name = "Tipo de Propiedad",
    values = c(Apartamento = "#4682B4", Casa = "#d73027") # Azul y rojo
  ) +
  labs(
    title = "Distribución de Propiedades por Localidad",
    x = "Longitud", y = "Latitud"
  ) +
  theme_minimal()

# Base test
ggplot() +
  geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
  geom_sf(data = sf_test %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 0.8, alpha = 0.6) +
  geom_sf(data = sf_test %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 0.8, alpha = 0.8) +
  scale_fill_manual(
    name = "Localidad",
    values = c(
      "CHAPINERO" = "#ADD8E6", # Azul claro para Chapinero
      .default = "#FAFAFA"     # Gris claro para otras localidades
    )
  ) +
  scale_color_manual(
    name = "Tipo de Propiedad",
    values = c(Apartamento = "#4682B4", Casa = "#d73027") # Azul y rojo
  ) +
  labs(
    title = "Distribución de Propiedades por Localidad",
    x = "Longitud", y = "Latitud"
  ) +
  theme_minimal()

# Calcular los centros del mapa -----------------------------------------------

latitud_central_train <- mean(train$lat, na.rm = TRUE) # Centro para train
longitud_central_train <- mean(train$lon, na.rm = TRUE)

latitud_central_test <- mean(test$lat, na.rm = TRUE) # Centro para test
longitud_central_test <- mean(test$lon, na.rm = TRUE)
  
  
#-------------------------------------------------------------------------------
#8. Creacion de variables a partir de informacions geoespacial ------------------#
#-------------------------------------------------------------------------------
 
 #Sacar informacion espacial de Bogota
  
  # Posibles categorías de las que podemos extraer información geosespacial. 
  print(available_tags("leisure"))
  print(available_features()) # para ver todas las categorias
  
#--------------------#
#      Parques
#--------------------#

# 1. Extraer información de todos los parques en Bogotá ------------------------

# Usamos OpenStreetMap (OSM) para obtener datos sobre los parques en Bogotá.
parques <- opq(bbox = getbb("Bogotá Colombia")) %>% # Definir el área geográfica (bounding box).
  add_osm_feature(key = "leisure", value = "park")  # Filtrar características con key "leisure" y value "park".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto de clase `sf` para trabajar con geometrías espaciales.
parques_sf <- osmdata_sf(parques)

# Extraemos las geometrías de los polígonos de los parques, incluyendo el ID y el nombre.
parques_geometria <- parques_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionar solo columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos que estén en el formato correcto.
parques_geometria <- st_as_sf(parques_sf$osm_polygons)

# 3. Calcular centroides de los parques ----------------------------------------

# Calculamos el centroide de cada parque para representar su ubicación como un único punto.
centroides <- st_centroid(parques_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraer la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraer la coordenada Y (latitud).
  )

# 4. Visualización en un mapa interactivo --------------------------------------

# Creamos un mapa con Leaflet para visualizar los parques y sus centroides.
leaflet() %>%
  addTiles() %>%  # Agregar capa base del mapa.
  setView(lng = longitud_central_train, lat = latitud_central_train, zoom = 12) %>%  # Configurar vista inicial.
  addPolygons(data = parques_geometria, col = "red", weight = 10, opacity = 0.8, popup = parques_geometria$name) %>%  # Dibujar polígonos.
  addCircles(lng = centroides$x, lat = centroides$y, col = "darkblue", opacity = 0.5, radius = 1)  # Agregar centroides.

# 5. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases de datos train/test a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 6. Calcular distancias a parques ---------------------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de los parques.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Para la base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Para la base test.

# 7. Determinar la distancia mínima a cualquier parque -------------------------

# Para cada propiedad, encontramos la distancia mínima a un parque.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# Agregamos esta información como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_parque = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_parque = dist_min_te)  # Base test.

# 8. Evaluar el tamaño del parque más cercano ----------------------------------

# Identificamos la posición del parque más cercano para cada propiedad.
posicion_t <- apply(dist_matrix_t, 1, function(x) which(min(x) == x))  # Base train.
posicion_te <- apply(dist_matrix_te, 1, function(x) which(min(x) == x))  # Base test.

# Extraemos el área de cada parque.
areas <- st_area(parques_geometria)  # Calcular áreas de los polígonos.

# Agregamos el área del parque más cercano como una nueva variable en las bases originales.
train <- train %>% mutate(area_parque = as.numeric(areas[posicion_t]))  # Base train.
test <- test %>% mutate(area_parque = as.numeric(areas[posicion_te]))  # Base test.
  
  
#--------------------#
#Estacion de policia
#--------------------#
  

# 1. Extraer información sobre estaciones de policía en Bogotá ----------------

# Usamos OpenStreetMap (OSM) para obtener datos de estaciones de policía.
policia <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definir el área geográfica (Bogotá).
  add_osm_feature(key = "amenity", value = "police") # Filtrar características con la key "amenity" y value "police".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto `sf` para trabajar con geometrías espaciales.
policia_sf <- osmdata_sf(policia)

# Extraemos las geometrías de las estaciones de policía, incluyendo el ID y el nombre.
policia_geometria <- policia_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionar columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
policia_geometria <- st_as_sf(policia_sf$osm_polygons)

# 3. Calcular centroides de las estaciones de policía --------------------------

# Calculamos el centroide de cada estación para representar su ubicación como un único punto.
centroides_p <- st_centroid(policia_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraer la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraer la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroidesp_sf <- st_as_sf(centroides_p, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a estaciones de policía -------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de las estaciones de policía.
dist_matrixp_t <- st_distance(x = sf_train, y = centroidesp_sf)  # Base train.
dist_matrixp_te <- st_distance(x = sf_test, y = centroidesp_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrixp_t)  # Dimensiones para train.
dim(dist_matrixp_te)  # Dimensiones para test.

# 6. Determinar la distancia mínima a cualquier estación de policía ------------

# Para cada propiedad, encontramos la distancia mínima a una estación de policía.
dist_p_t <- apply(dist_matrixp_t, 1, min)  # Base train.
dist_p_te <- apply(dist_matrixp_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a las estaciones de policía como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_policia = dist_p_t)  # Base train.
test <- test %>% mutate(distancia_policia = dist_p_te)  # Base test.


#--------------------#
#     Gimnasio
#--------------------# 
  
# 1. Extraer información sobre gimnasios en Bogotá -----------------------------

# Usamos OpenStreetMap (OSM) para obtener datos sobre gimnasios en Bogotá.
gym <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definir el área geográfica (bounding box).
  add_osm_feature(key = "leisure", value = "fitness_centre")  # Filtrar con key "leisure" y value "fitness_centre".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto `sf` para trabajar con geometrías espaciales.
gym_sf <- osmdata_sf(gym)

# Extraemos las geometrías de los gimnasios, incluyendo el ID y el nombre.
gym_geometria <- gym_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos solo las columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
gym_geometria <- st_as_sf(gym_sf$osm_polygons)

# 3. Calcular centroides de los gimnasios --------------------------------------

# Calculamos el centroide de cada gimnasio para representar su ubicación como un único punto.
centroides_g <- st_centroid(gym_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraer la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraer la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroides_g_sf <- st_as_sf(centroides_g, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a gimnasios -------------------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de los gimnasios.
dist_matrix_g_t <- st_distance(x = sf_train, y = centroides_g_sf)  # Base train.
dist_matrix_g_te <- st_distance(x = sf_test, y = centroides_g_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_g_t)  # Dimensiones para train.
dim(dist_matrix_g_te)  # Dimensiones para test.

# 6. Determinar la distancia mínima a cualquier gimnasio -----------------------

# Para cada propiedad, encontramos la distancia mínima a un gimnasio.
dist_g_t <- apply(dist_matrix_g_t, 1, min)  # Base train.
dist_g_te <- apply(dist_matrix_g_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a los gimnasios como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_gym = dist_g_t)  # Base train.
test <- test %>% mutate(distancia_gym = dist_g_te)  # Base test.
  
#--------------------#
#         Bus
#--------------------# 
  
# 1. Extraer información sobre estaciones de bus en Bogotá ---------------------

# Usamos OpenStreetMap (OSM) para obtener datos sobre las estaciones de bus.
bus <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definir el área geográfica de interés (Bogotá).
  add_osm_feature(key = "amenity", value = "bus_station")  # Filtrar datos con key "amenity" y value "bus_station".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto `sf` para trabajar con geometrías espaciales.
bus_sf <- osmdata_sf(bus)

# Extraemos las geometrías de las estaciones de bus, incluyendo el ID y el nombre.
bus_geometria <- bus_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurar que están en el formato correcto.
bus_geometria <- st_as_sf(bus_sf$osm_polygons)

# 3. Calcular centroides de las estaciones de bus ------------------------------

# Calculamos el centroide de cada estación para representar su ubicación como un único punto.
centroides <- st_centroid(bus_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraer la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraer la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a estaciones de bus -----------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de las estaciones de bus.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier estación de bus ----------------

# Para cada propiedad, encontramos la distancia mínima a una estación de bus.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a las estaciones de bus como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_bus = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_bus = dist_min_te)  # Base test.
  

#--------------------#
#    Supermercados
#--------------------#   
  
# 1. Extraer información sobre supermercados en Bogotá -------------------------

# Usamos OpenStreetMap (OSM) para obtener datos sobre los supermercados en Bogotá.
super <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica de interés (bounding box).
  add_osm_feature(key = "shop", value = "supermarket")  # Filtramos datos con la key "shop" y value "supermarket".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto `sf` para trabajar con geometrías espaciales.
super_sf <- osmdata_sf(super)

# Extraemos las geometrías de los supermercados, incluyendo el ID y el nombre.
super_geometria <- super_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurar que estén en el formato correcto.
super_geometria <- st_as_sf(super_sf$osm_polygons)

# 3. Calcular centroides de los supermercados ----------------------------------

# Calculamos el centroide de cada supermercado para representar su ubicación como un único punto.
centroides <- st_centroid(super_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a supermercados ---------------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de los supermercados.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier supermercado -------------------

# Para cada propiedad, encontramos la distancia mínima a un supermercado.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a los supermercados como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_super = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_super = dist_min_te)  # Base test.

  
#--------------------#
#        Bar 
#--------------------#   
  
# 1. Extraer información sobre bares en Bogotá ---------------------------------

# Usamos OpenStreetMap (OSM) para obtener datos sobre bares en Bogotá.
bar <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definir el área geográfica de interés (bounding box).
  add_osm_feature(key = "amenity", value = "bar")  # Filtrar datos con la key "amenity" y value "bar".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un objeto `sf` para trabajar con geometrías espaciales.
bar_sf <- osmdata_sf(bar)  # Nota: el código original tenía `super`, pero debería ser `bar`.

# Extraemos las geometrías de los bares, incluyendo el ID y el nombre.
bar_geometria <- bar_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurar que estén en el formato correcto.
bar_geometria <- st_as_sf(bar_sf$osm_polygons)

# 3. Calcular centroides de los bares ------------------------------------------

# Calculamos el centroide de cada bar para representar su ubicación como un único punto.
centroides <- st_centroid(bar_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326 (coordenadas geográficas).
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a bares -----------------------------------------------

# Creamos matrices de distancia entre propiedades y los centroides de los bares.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier bar ----------------------------

# Para cada propiedad, encontramos la distancia mínima a un bar.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a los bares como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_bar = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_bar = dist_min_te)  # Base test.
  
#--------------------#
#       Hospital 
#--------------------#   
  
# 1. Extraer información sobre hospitales en Bogotá ----------------------------

# Utilizamos OpenStreetMap (OSM) para obtener información geoespacial sobre hospitales.
hospital <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica (Bogotá).
  add_osm_feature(key = "amenity", value = "hospital")  # Filtramos datos con la key "amenity" y value "hospital".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Convertimos los datos obtenidos a un formato `sf` para trabajar con geometrías espaciales.
hospital_sf <- osmdata_sf(hospital)

# Extraemos las geometrías de los hospitales, incluyendo el ID y el nombre.
hospital_geometria <- hospital_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos las columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurar su formato.
hospital_geometria <- st_as_sf(hospital_sf$osm_polygons)

# 3. Calcular centroides de los hospitales -------------------------------------

# Calculamos el centroide de cada hospital para representar su ubicación como un único punto.
centroides <- st_centroid(hospital_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a hospitales ------------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de los hospitales.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier hospital -----------------------

# Para cada propiedad, encontramos la distancia mínima a un hospital.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a hospitales como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_hosp = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_hosp = dist_min_te)  # Base test.
  
  
#--------------------#
#       Colegio 
#--------------------#   
  
# 1. Extraer información sobre colegios en Bogotá ------------------------------

# Usamos OpenStreetMap (OSM) para obtener datos geoespaciales de colegios en Bogotá.
colegio <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica de interés (Bogotá).
  add_osm_feature(key = "amenity", value = "school")  # Filtramos datos con la key "amenity" y value "school".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Convertimos los datos obtenidos a un formato `sf` para trabajar con geometrías espaciales.
colegio_sf <- osmdata_sf(colegio)

# Extraemos las geometrías de los colegios, incluyendo el ID y el nombre.
colegio_geometria <- colegio_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurar que estén en el formato correcto.
colegio_geometria <- st_as_sf(colegio_sf$osm_polygons)

# 3. Calcular centroides de los colegios ---------------------------------------

# Calculamos el centroide de cada colegio para representar su ubicación como un único punto.
centroides <- st_centroid(colegio_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a colegios --------------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de los colegios.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier colegio ------------------------

# Para cada propiedad, encontramos la distancia mínima a un colegio.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a los colegios como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_cole = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_cole = dist_min_te)  # Base test.
  
#--------------------#
#   Centro Comercial 
#--------------------#   

# 1. Extraer información sobre edificios comerciales en Bogotá -----------------

# Utilizamos OpenStreetMap (OSM) para obtener información geoespacial de edificios comerciales.
comercial <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica (bounding box) para Bogotá.
  add_osm_feature(key = "building", value = "commercial")  # Filtramos datos con la key "building" y value "commercial".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos en un formato `sf` para trabajar con geometrías espaciales.
comercial_sf <- osmdata_sf(comercial)

# Extraemos las geometrías de los edificios comerciales, incluyendo el ID y el nombre.
comercial_geometria <- comercial_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos las columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
comercial_geometria <- st_as_sf(comercial_sf$osm_polygons)

# 3. Calcular centroides de los edificios comerciales --------------------------

# Calculamos el centroide de cada edificio comercial para representar su ubicación como un único punto.
centroides <- st_centroid(comercial_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a edificios comerciales --------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de los edificios comerciales.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier edificio comercial -------------

# Para cada propiedad, encontramos la distancia mínima a un edificio comercial.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a edificios comerciales como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_cc = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_cc = dist_min_te)  # Base test.

  
#--------------------#
#      Restaurantes
#--------------------#  
  
# 1. Extraer información sobre restaurantes en Bogotá --------------------------

# Usamos OpenStreetMap (OSM) para obtener datos geoespaciales de restaurantes en Bogotá.
restaurantes <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica de interés (Bogotá).
  add_osm_feature(key = "amenity", value = "restaurant")  # Filtramos datos con la key "amenity" y value "restaurant".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos a un formato `sf` para trabajar con geometrías espaciales.
restaurantes_sf <- osmdata_sf(restaurantes)

# Extraemos las geometrías de los restaurantes, incluyendo el ID y el nombre.
restaurantes_geometria <- restaurantes_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
restaurantes_geometria <- st_as_sf(restaurantes_sf$osm_polygons)

# 3. Calcular centroides de los restaurantes -----------------------------------

# Calculamos el centroide de cada restaurante para representar su ubicación como un único punto.
centroides <- st_centroid(restaurantes_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a restaurantes ----------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de los restaurantes.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier restaurante --------------------

# Para cada propiedad, encontramos la distancia mínima a un restaurante.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a restaurantes como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_rest = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_rest = dist_min_te)  # Base test.
  

#--------------------#
#      Librerias
#--------------------#  
  
# 1. Extraer información sobre librerías en Bogotá -----------------------------

# Usamos OpenStreetMap (OSM) para obtener datos geoespaciales de librerías en Bogotá.
libreria <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica (bounding box) para Bogotá.
  add_osm_feature(key = "amenity", value = "library")  # Filtramos datos con la key "amenity" y value "library".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Convertimos los datos obtenidos en un formato `sf` para trabajar con geometrías espaciales.
libreria_sf <- osmdata_sf(libreria)

# Extraemos las geometrías de las librerías, incluyendo el ID y el nombre.
libreria_geometria <- libreria_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
libreria_geometria <- st_as_sf(libreria_sf$osm_polygons)

# 3. Calcular centroides de las librerías --------------------------------------

# Calculamos el centroide de cada librería para representar su ubicación como un único punto.
centroides <- st_centroid(libreria_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a librerías -------------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de las librerías.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier librería -----------------------

# Para cada propiedad, encontramos la distancia mínima a una librería.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a librerías como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_libreria = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_libreria = dist_min_te)  # Base test.
  
#--------------------#
#     Universidad
#--------------------#    
  
# 1. Extraer información sobre universidades en Bogotá -------------------------

# Usamos OpenStreetMap (OSM) para obtener datos geoespaciales de universidades en Bogotá.
universidad <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área geográfica de interés (Bogotá).
  add_osm_feature(key = "amenity", value = "university")  # Filtramos datos con la key "amenity" y value "university".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos a un formato `sf` para trabajar con geometrías espaciales.
universidad_sf <- osmdata_sf(universidad)

# Extraemos las geometrías de las universidades, incluyendo el ID y el nombre.
universidad_geometria <- universidad_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
universidad_geometria <- st_as_sf(universidad_sf$osm_polygons)

# 3. Calcular centroides de las universidades ----------------------------------

# Calculamos el centroide de cada universidad para representar su ubicación como un único punto.
centroides <- st_centroid(universidad_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a universidades ---------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de las universidades.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier universidad --------------------

# Para cada propiedad, encontramos la distancia mínima a una universidad.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a universidades como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_uni = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_uni = dist_min_te)  # Base test.
  
#--------------------#
#      Banco
#--------------------#    
  
# 1. Extraer información sobre bancos en Bogotá --------------------------------

# Usamos OpenStreetMap (OSM) para obtener datos geoespaciales de bancos en Bogotá.
banco <- opq(bbox = getbb("Bogotá Colombia")) %>%  # Definimos el área de interés (Bogotá).
  add_osm_feature(key = "amenity", value = "bank")  # Filtramos datos con la key "amenity" y value "bank".

# 2. Convertir a formato sf (simple features) ----------------------------------

# Transformamos los datos obtenidos a un formato `sf` para trabajar con geometrías espaciales.
banco_sf <- osmdata_sf(banco)

# Extraemos las geometrías de los bancos, incluyendo el ID y el nombre.
banco_geometria <- banco_sf$osm_polygons %>%
  dplyr::select(osm_id, name)  # Seleccionamos columnas relevantes.

# Convertimos las geometrías a un objeto `sf` para asegurarnos de que estén en el formato correcto.
banco_geometria <- st_as_sf(banco_sf$osm_polygons)

# 3. Calcular centroides de los bancos -----------------------------------------

# Calculamos el centroide de cada banco para representar su ubicación como un único punto.
centroides <- st_centroid(banco_geometria, byid = TRUE) %>%
  mutate(
    x = st_coordinates(.)[, "X"],  # Extraemos la coordenada X (longitud).
    y = st_coordinates(.)[, "Y"]   # Extraemos la coordenada Y (latitud).
  )

# 4. Asegurar proyecciones consistentes ----------------------------------------

# Convertimos los centroides y las bases `train` y `test` a objetos `sf` con proyección EPSG 4326.
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancias a bancos ----------------------------------------------

# Creamos matrices de distancia entre las propiedades y los centroides de los bancos.
dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)  # Base train.
dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)  # Base test.

# Verificamos las dimensiones de las matrices de distancia (opcional).
dim(dist_matrix_t)  # Dimensiones de la matriz para train.
dim(dist_matrix_te)  # Dimensiones de la matriz para test.

# 6. Determinar la distancia mínima a cualquier banco --------------------------

# Para cada propiedad, encontramos la distancia mínima a un banco.
dist_min_t <- apply(dist_matrix_t, 1, min)  # Base train.
dist_min_te <- apply(dist_matrix_te, 1, min)  # Base test.

# 7. Agregar la distancia mínima a las bases originales ------------------------

# Agregamos la distancia mínima a bancos como una nueva variable en las bases originales.
train <- train %>% mutate(distancia_banco = dist_min_t)  # Base train.
test <- test %>% mutate(distancia_banco = dist_min_te)  # Base test.


#--------------------#
# Avenida mas cerca
#--------------------#   

  # 1. Convertir los dataframes `train` y `test` a objetos sf
  sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
  
  # 2. Obtener datos de avenidas de OpenStreetMap para Bogotá
  avenidas <- opq(bbox = getbb("Bogota Colombia")) %>%
    add_osm_feature(key = "highway", value = "secondary")
  avenidas_sf <- osmdata_sf(avenidas)
  
  # 3. Extraer las geometrías de las avenidas
  avenidas_geometria <- avenidas_sf$osm_lines %>%
    select(osm_id, name, geometry)
  
  # 4. Encontrar las geometrías más cercanas
  cercano_train <- st_nearest_feature(sf_train, avenidas_geometria)
  cercano_test <- st_nearest_feature(sf_test, avenidas_geometria)
  
  # 5. Calcular las distancias
  dist_train <- st_distance(sf_train, avenidas_geometria[cercano_train, ], by_element = TRUE)
  dist_test <- st_distance(sf_test, avenidas_geometria[cercano_test, ], by_element = TRUE)
  
  # 6. Agregar las distancias calculadas a los dataframes originales
  train$dist_avenida <- as.numeric(dist_train)
  test$dist_avenida <- as.numeric(dist_test)
  
  # 7. Verificar el resultado
  head(train)
  head(test)

#-------------------------------------------------------------------------------
# 9. Creacion variables a  parir del texto -------------------------------------
#-------------------------------------------------------------------------------  
  
#------------------------------#
#     Tipo de propiedad
#------------------------------#
  
# 1. Verificar un ejemplo de la descripción ------------------------------------
train$description[2]  # Se visualiza el texto original de una descripción.

# 2. Normalizar el texto en la columna "description" ---------------------------

# Se normaliza el texto:
# - Convierte todo el texto a minúsculas (`str_to_lower`).
# - Elimina acentos y caracteres especiales (`iconv`).
# - Reemplaza caracteres no alfanuméricos por espacios (`str_replace_all`).
# - Elimina espacios duplicados y recorta espacios al inicio/final (`gsub` y `str_trim`).
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", 
                                     str_replace_all(
                                       iconv(str_to_lower(description), from = "UTF-8", to = "ASCII//TRANSLIT"),
                                       "[^[:alnum:]]", " "))))

# Verificar cambios en la descripción normalizada.
train$description[2]

# 3. Ajustar la columna `property_type` usando la descripción -----------------

# Crear una nueva columna `property_type_2`:
# - Si la descripción contiene "casa", se asigna "Casa" como tipo de propiedad.
# - En caso contrario, se mantiene el valor original de `property_type`.
train <- train %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))

# Repetir para identificar "Apartamento" o "apto" en la descripción.
train <- train %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))

# Verificar las frecuencias de las columnas nuevas y originales.
table(train$property_type_2)
table(train$property_type)

# Repetir el mismo procedimiento para la base `test`.
test <- test %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type)) %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))

# Verificar frecuencias en la base `test`.
table(test$property_type_2)
table(test$property_type)

# 4. Imputación de variables utilizando el nuevo tipo de propiedad ------------

# Verificar clases de variables numéricas relevantes en `train`.
sapply(train[, c("rooms", "bathrooms")], class)

# Imputar la moda (most frequent value) para las variables `rooms` y `bathrooms`.
train <- train %>% 
  group_by(property_type_2) %>% 
  mutate(
    rooms_imp2 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
    bathrooms_imp2 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms)
  ) %>%
  ungroup()

# Imputar la media para `surface_total` y `surface_covered`.
train <- train %>% 
  group_by(property_type_2) %>% 
  mutate(
    surface_total_imp_mean2 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
    surface_covered_imp_mean2 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
  ) %>%
  ungroup()

# Imputar la mediana para `surface_total` y `surface_covered`.
train <- train %>% 
  group_by(property_type_2) %>% 
  mutate(
    surface_total_median2 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
    surface_covered_median2 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
  ) %>%
  ungroup()

# Repetir imputaciones en la base `test`.
test <- test %>% 
  group_by(property_type_2) %>% 
  mutate(
    rooms_imp2 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
    bathrooms_imp2 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms)
  ) %>%
  ungroup()

test <- test %>% 
  group_by(property_type_2) %>% 
  mutate(
    surface_total_imp_mean2 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
    surface_covered_imp_mean2 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
  ) %>%
  ungroup()

test <- test %>% 
  group_by(property_type_2) %>% 
  mutate(
    surface_total_median2 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
    surface_covered_median2 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
  ) %>%
  ungroup()

# Convertir las bases `train` y `test` a dataframes para finalizar.
train <- as.data.frame(train)
test <- as.data.frame(test)
  
#---------------------#
#      Pisos 
#---------------------#
  
# 1. Número de pisos para casas ------------------------------------------------

# Base `train`:
# - Se extrae la información del número de pisos desde la descripción usando `str_extract`.
# - Se asigna NA a las propiedades que no son de tipo "Casa".
train <- train %>%
  mutate(n_pisos = str_extract(description, "(\\w+|\\d+) pisos")) %>%
  mutate(n_pisos = ifelse(property_type == "Casa", n_pisos, NA))

# Base `test`:
test <- test %>%
  mutate(n_pisos = str_extract(description, "(\\w+|\\d+) pisos")) %>%
  mutate(n_pisos = ifelse(property_type == "Casa", n_pisos, NA))

# Convertir números escritos a formato numérico.
# Definimos equivalencias entre números escritos y sus valores numéricos.
numeros_escritos <- c("dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", 
                      "nueve", "diez", "once", "doce", "trece", "catorce", "quince")
numeros_numericos <- as.character(2:15)

# Base `train`:
train <- train %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos, numeros_escritos))) %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+"))) %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico > 10, 1, n_pisos_numerico))

# Base `test`:
test <- test %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos, numeros_escritos))) %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+"))) %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico > 10, 1, n_pisos_numerico))

# Crear un gráfico para visualizar la distribución de `n_pisos_numerico` en `train`.
ggplot(train %>% filter(n_pisos_numerico > 1), aes(x = factor(n_pisos_numerico))) +
  geom_bar() +
  labs(title = "", x = "Pisos", y = "Observaciones") +
  theme_minimal()

# 2. Número de pisos para apartamentos -----------------------------------------

# Base `train`:
# - Extraer información del piso desde la descripción.
train <- train %>%
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

# Convertir equivalencias para números escritos (primero, segundo, etc.).
numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", 
                      "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", 
                      "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

# Aplicar las transformaciones en `train`:
train <- train %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos, numeros_escritos))) %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+"))) %>%
  mutate(piso_numerico = ifelse(piso_numerico > 35, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type_2 == "Casa", 1, piso_numerico)) %>%
  mutate(piso_numerico = replace_na(piso_numerico, 2))  # Reemplazar NA con la moda (2).

# Contar ocurrencias por `piso_numerico` para apartamentos.
train %>%
  filter(property_type_2 == "Apartamento") %>%
  count(piso_numerico)

# Base `test`:
test <- test %>%
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)")) %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos, numeros_escritos))) %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+"))) %>%
  mutate(piso_numerico = ifelse(piso_numerico > 35, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type_2 == "Casa", 1, piso_numerico)) %>%
  mutate(piso_numerico = replace_na(piso_numerico, 2))  # Reemplazar NA con la moda (2).

# Contar ocurrencias por `piso_numerico` para apartamentos en `test`.
test %>%
  filter(property_type_2 == "Apartamento") %>%
  count(piso_numerico)

  
#------------------------------------------------------------------------------#
# 10. Ejercicios Adicional: Agregar variable de Barrio
#------------------------------------------------------------------------------#
  
#--------------------#
#  Agregar barrio
#--------------------#

#Link: https://www.ideca.gov.co/recursos/mapas/sector-catastral

#. Pegar variable de barrio 

# Establece el directorio de trabajo donde se encuentran los datos espaciales
setwd(paste0(wd, "\\Datos espaciales\\Barrios"))

# Lee el archivo GeoJSON con la información de los barrios y lo carga como un objeto `sf`
barrios <- st_read("SECTOR.geojson")

# Transforma el sistema de coordenadas de los datos espaciales de barrios a CRS 4326 (WGS 84)
barrios <- st_transform(barrios, 4326)

# Convierte las bases `train` y `test` a objetos espaciales `sf` con las coordenadas de longitud y latitud
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Verifica si las geometrías de los barrios son válidas. Si no lo son, las corrige.
if (!all(st_is_valid(barrios))) {
  barrios <- st_make_valid(barrios)  # Corrige geometrías inválidas
}

# Realiza una unión espacial para asignar cada punto de `sf_train` y `sf_test` al polígono del barrio en el que se encuentran
sf_train <- st_join(sf_train, barrios, join = st_intersects)
sf_test <- st_join(sf_test, barrios, join = st_intersects)

# Agrega el nombre del barrio (`SCANOMBRE`) como una nueva variable en las bases `train` y `test`
train$barrio <- sf_train$SCANOMBRE
table(is.na(train$barrio))  # Verifica si hay valores faltantes en la variable `barrio`
test$barrio <- sf_test$SCANOMBRE
table(is.na(test$barrio))  # Verifica si hay valores faltantes en `test`

barrio_moda <- as.character(names(sort(table(train$barrio), decreasing = TRUE)[1]))
train$barrio[is.na(train$barrio)] <- barrio_moda

  
#--------------------#
#  Agregar estrato
#--------------------#   
  
# Establecer el directorio de trabajo para los datos espaciales de manzanas
setwd(paste0(wd, "\\Datos espaciales\\Manzanas\\Datos abiertos")) # Cambia el directorio actual para trabajar con los datos de manzanas.
  #Fuente:https://datosabiertos.bogota.gov.co/dataset/estratificacion-para-bogota

# Leer el archivo shapefile con información de las manzanas y corregir geometrías inválidas
manzanas <- st_read("ManzanaEstratificacion.shp")  # Carga los datos de manzanas desde un archivo shapefile.
manzanas <- st_transform(manzanas, 4326)  # Transforma el sistema de coordenadas al estándar WGS 84.

# Corregir geometrías inválidas
manzanas <- st_make_valid(manzanas)  # Soluciona posibles problemas en las geometrías del shapefile.

# Convertir los dataframes `train` y `test` en objetos espaciales `sf` con coordenadas y CRS 4326
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)  # Convierte `train` a un objeto espacial.
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)  # Convierte `test` a un objeto espacial.

# Realizar la unión espacial asegurándose de que no queden observaciones con valores faltantes
sf_train <- st_join(sf_train, manzanas, join = st_intersects, left = TRUE)  # Asocia cada punto de `sf_train` a la manzana correspondiente.
sf_test <- st_join(sf_test, manzanas, join = st_intersects, left = TRUE)  # Hace lo mismo con `sf_test`.

# Verificar las geometrías resultantes
sf_train <- st_make_valid(sf_train)  # Valida y corrige geometrías de `sf_train`.
sf_test <- st_make_valid(sf_test)  # Valida y corrige geometrías de `sf_test`.

# Mirar missings
train$estrato <- sf_train$ESTRATO  # Agrega la variable `ESTRATO` (estrato socioeconómico) a `train`.
summary(train$estrato)  # Resumen estadístico de `estrato` en `train`.
test$estrato <- sf_test$ESTRATO  # Agrega la variable `ESTRATO` a `test`.
summary(test$estrato)  # Resumen estadístico de `estrato` en `test`.

# Calcular porcentaje de valores faltantes por localidad
missing_by_locality_train <- train %>%
  group_by(localidad) %>%
  summarise(
    missings = sum(is.na(estrato)),  # Cuenta los valores faltantes.
    total = n(),  # Total de observaciones por localidad.
    porcentaje = (missings / total) * 100  # Porcentaje de valores faltantes.
  )
missing_by_locality_train  # Imprime el porcentaje de valores faltantes por localidad.

# Imputar el estrato por la moda dentro de cada localidad
train <- train %>%
  group_by(localidad) %>%
  mutate(
    estrato_imp = ifelse(
      is.na(estrato),  # Si `estrato` es NA...
      as.numeric(mlv(estrato, method = "mfv", na.rm = TRUE)),  # Imputa con la moda (most frequent value).
      estrato  # De lo contrario, conserva el valor original.
    )
  ) %>%
  ungroup()
table(is.na(train$estrato_imp))  # Verifica si quedan valores faltantes en `estrato_imp`.

test <- test %>%
  group_by(localidad) %>%
  mutate(
    estrato_imp = ifelse(
      is.na(estrato), 
      as.numeric(mlv(estrato, method = "mfv", na.rm = TRUE)), 
      estrato
    )
  ) %>%
  ungroup()
table(is.na(test$estrato_imp))  # Verifica si quedan valores faltantes en `estrato_imp` de `test`.

# Identificar localidades con valores faltantes en el estrato imputado
missing_estrato <- test[is.na(test$estrato_imp), ]  # Filtra observaciones con `estrato_imp` faltante.
missing_por_localidad <- table(missing_estrato$localidad)  # Tabla de frecuencias de localidades con valores faltantes.
print(missing_por_localidad)  # Imprime las localidades con valores faltantes.

# Reemplazar valores faltantes restantes por el estrato predominante en Bogotá (estrato 3)
train$estrato_imp[is.na(train$estrato_imp)] <- 3  # Imputa los faltantes restantes en `train` con el valor 3.
test$estrato_imp[is.na(test$estrato_imp)] <- 3  # Imputa los faltantes restantes en `test` con el valor 3.

# Verificar nuevamente si quedan valores faltantes
table(is.na(train$estrato_imp))  # Verifica que no queden valores faltantes en `train`.
table(is.na(test$estrato_imp))  # Verifica que no queden valores faltantes en `test`.
  
#------------------------------------------------------------------------------#
# 11.  Quitar variables que no son de interes
#------------------------------------------------------------------------------#  
  
#Base train 
  train <- train %>% 
    dplyr::select(-n_pisos, -piso_info, estrato)
  names(train)
  
#Base test
  test <- test %>% 
    dplyr::select(-n_pisos, -piso_info, estrato)
  names(test)
  
#------------------------------------------------------------------------------#
# 12. Guardar bases de datos
#------------------------------------------------------------------------------#
  
  setwd(paste0(wd, "\\Bases")) #Directorio datos
  saveRDS(train, "train.rds")  # Guardar el objeto `train`
  saveRDS(test, "test.rds")    # Guardar el objeto `test`
  
  