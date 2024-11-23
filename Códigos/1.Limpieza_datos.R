#------------------------------------------------------------------------------#
#---------------------- LIMPIEZA DATA - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

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
# 3. Revisar missings de las variables   ---------------------------------------
#-------------------------------------------------------------------------------
  
#Base de entrenamiento -----------------------------------------------------
  
  #Grafica de missings
  
    #Forma 1
  vis_dat(train) +
    scale_fill_manual(values = c(
      "character" = "#B0C4DE", # Azul claro
      "integer" = "#87CEEB",  # Azul cielo
      "numeric" = "#4682B4",  # Azul acero
      "NA" = "#D3D3D3"       # Gris claro
    ))
  
    #Forma 2
  vis_dat(train) +
    scale_fill_manual(values = c(
      "character" = "#B0C4DE", # Azul claro
      "integer" = "#87CEEB",  # Azul cielo
      "numeric" = "#4682B4",  # Azul acero
      "NA" = "#D3D3D3"       # Gris claro
    ))   +
    labs(
      fill = "Tipo de dato",
      x = NULL, 
      y = "Observaciones"
    ) +
    scale_x_discrete(labels = c(
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
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  
  
  #Tablas de porcentajes de missing
  db_miss <- skim(train) %>% dplyr::select(skim_variable, n_missing)
  Nobs= nrow(train) 
  db_miss<- db_miss %>% filter(n_missing!= 0)
  db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
  db_miss  
  
#Base de testeo ----------------------------------------------------------------
  
  #Graficas de missings
  
    #Forma 1
  vis_dat(test) +
    scale_fill_manual(
      name = "Tipo de dato", # Cambiar el nombre de la leyenda aquí
      values = c(
        "character" = "#B0C4DE", # Azul claro
        "integer" = "#87CEEB",  # Azul cielo
        "numeric" = "#4682B4",  # Azul acero
        "NA" = "#D3D3D3"       # Gris claro
      ))
  
    #Forma 2
  vis_dat(test) +
    scale_fill_manual(
      name = "Tipo de dato", # Cambiar el nombre de la leyenda aquí
      values = c(
      "character" = "#B0C4DE", # Azul claro
      "integer" = "#87CEEB",  # Azul cielo
      "numeric" = "#4682B4",  # Azul acero
      "NA" = "#D3D3D3"       # Gris claro
    ))  +
    labs(
      fill = "Tipo de dato",
      x = NULL, 
      y = "Observaciones"
    ) +
    scale_x_discrete(labels = c(
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
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  
  #Tabla de missings
  db_miss_test <- skim(test) %>% dplyr::select(skim_variable, n_missing)
  Nobs= nrow(train) 
  db_miss_test<- db_miss_test %>% filter(n_missing!= 0)
  db_miss_test<- db_miss_test %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
  db_miss_test  
  
#-------------------------------------------------------------------------------
# 4. Tratamiento de los missings -----------------------------------------------
#-------------------------------------------------------------------------------
  
#Base de entrenamiento ----------------
    
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  
  sapply(train[, c("rooms", "bathrooms", "bedrooms")], class) #Tipo de variable
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      rooms_imp = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  
  ##. Imputacion media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  
    #Imputacion media
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_imp_mean = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()

    #Imputacion mediana
  train <- train %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_median = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  

  #Base de testeo ----------------
  
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      rooms_imp = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  ## Imputación media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  
    # Imputación media
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_imp_mean = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
    # Imputación mediana
  test <- test %>% 
    group_by(property_type) %>% 
    mutate(
      surface_total_median = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
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
  

# Histograma -----------------------------------------------------------
  
  # Frecuencia
  
    #Forma 1 
  p <- ggplot(train, aes(x = price/ 1000000)) +
    geom_histogram(fill = "darkblue", alpha = 0.4) +
    labs(x = "Precio", y = "Cantidad") +
    theme_bw()
  ggplotly(p)
  
    #Forma 2
  p <- ggplot(train, aes(x = price)) +
    geom_histogram(fill = "darkblue", alpha = 0.4) +
    labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
    scale_x_log10(labels = scales::dollar) +
    theme_bw()
  ggplotly(p)
  
   #Densidad
  hist((train$price / 1000000), probability = TRUE, col = '#A6CFE2', border = "grey30", breaks = 25, 
       xlab = "Precio (millones)", main = "Distribución del Precio (en millones)")
  abline(v = mean((train$price / 1000000)), col = 'red', lwd = 3, lty = 2)
  lines(density((train$price / 1000000)), col = '#00008B', lwd = 3)
  legend("topright", legend = c("Media", "Densidad"), col = c("red", "#00008B"), lty = c(2, 1), lwd = 3, bty = "n")
  

  #Frecuencias por tipo de propiedad
  media_apto <- mean((train$price[train$property_type == "Apartamento"])/1000000)
  media_casa <- mean((train$price[train$property_type == "Casa"])/1000000)
  ggplot(train, aes(x = (price/1000000), fill = property_type)) +
    geom_histogram(color="white", alpha = 0.6, position = 'identity', binwidth = 50) +
    geom_vline(aes(xintercept = media_apto), color = "darkgreen", linetype = "dashed")+
    geom_vline(aes(xintercept = media_casa), color = "darkorange3", linetype = "dashed")+
    labs(x = 'Precio de Viviendas (Millones)', y ="Frecuencia")+
    scale_fill_manual(values=c("cadetblue4", "darkorange"))+
    guides(fill = guide_legend(title = "Inmueble"))+
    facet_wrap(~property_type)
  
  
#-------------------------------------------------------------------------------
# 7. Datos espaciales ----------------------------------------------------------
#-------------------------------------------------------------------------------   

# Verificar la cantidad de valores NA en lon y lat (No hay NA)-----------------
    
  #Base train
    sum(is.na(train$lon))  # Número de NA en la columna lon
    sum(is.na(train$lat))  # Número de NA en la columna lat

  #Base test
    sum(is.na(test$lon))  # Número de NA en la columna lon
    sum(is.na(test$lat))  # Número de NA en la columna lat  
    
  
# Observamos la primera visualización ------------------------------------------
    
  ##. primera visualización de datos (train)
    
    leaflet() %>% #Mirar mapa
      addTiles() %>%
      addCircles(lng = train$lon, #Longitud
                 lat = train$lat) #Latitud
    names(train)
    
  ##. primera visualización de datos (test)
    leaflet() %>%
      addTiles() %>%
      addCircles(lng = test$lon, 
                 lat = test$lat)
    
# Georeferencia por localidad en Bogota ----------------------------------------
  
  setwd(paste0(wd, "\\Datos espaciales\\Localidades")) #Directorio datos
  local <- st_read("poligonos-localidades.geojson")
  local <- subset(local, !(Nombre.de.la.localidad == "SUMAPAZ")) #quitar Sumapaz <- st_read("poligonos-localidades.geojson")

#Mapa de localidades
  local <- st_transform(local,4626) #Homogenizar la proyeccion de los datos
  ggplot()+
    geom_sf(data=local, color = "black")
  
#Mapa - resaltando solo Chapinero
  ggplot() +
    geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
    scale_fill_manual(
      values = c(
        "CHAPINERO" = "#FF6F61", # Resaltado en un color distintivo
        .default = "#B0BEC5"     # Todas las demás localidades en gris
      )
    ) +
    labs(
      title = "Mapa de Localidades"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.title = element_blank(), # Eliminar título de la leyenda
      legend.position = "right",
      legend.text = element_text(size = 10)
    )

#Mapa con todas las localidades
  ggplot() +
    geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
    labs(
      title = "Mapa de Localidades",
      fill = "Localidades"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
  
  
 # Convertir en datos espaciales las bases de train y test
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4626) #Homogenizar la proyeccion de los datos
  sf_test<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4626) #Homogenizar la proyeccion de los datos

# Realizar la unión espacial basada en la proximidad de coordenadas ---------
  
    #Base train
  sf_train <- st_join(sf_train, local, join = st_intersects)
  names(sf_train)
  dim(sf_train)
  
    #Base test
  sf_test <- st_join(sf_test, local, join = st_intersects)
  names(sf_test)
  dim(sf_test)
  
  # Agregar variable a train y test  ------------------------------------------
  train$localidad <- sf_train$Nombre.de.la.localidad
  names(train)
  
  test$localidad <- sf_test$Nombre.de.la.localidad
  names(test)
  
#Mapas ---------------------------------------------------------------------------
  
  #Base train
  ggplot()+
    geom_sf(data=local, color = "black") + #shapefile de comunas
    geom_sf(data=sf_train,aes(color = precio_mt2) ,shape=15, size=0.3)+
    theme_bw()
  
  #Base test  
  ggplot()+
    geom_sf(data=local, color = "black") + 
    geom_sf(data=sf_test,shape=15, size=0.3,color="orange") +
    theme_bw()
  
  
#Mapa por apartamento y casa -  Distribución de propiedaedes
  
  #Base train 
  ggplot() +
    geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) + 
    geom_sf(data = sf_train %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 0.8, alpha = 0.6) +
    geom_sf(data = sf_train %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 0.8, alpha = 0.8) +
    scale_fill_manual(
      name = "Localidad",
      values = c(
        "CHAPINERO" = "#ADD8E6", # Azul claro
        .default = "#FAFAFA"     # Gris muy claro
      )) +
    scale_color_manual(
      name = "Tipo de Propiedad", 
      values = c(Apartamento = "#4682B4", Casa = "#d73027") # Azul medio y rojo tomate
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = 2) # Cambia el tamaño de los puntos en la leyenda
      )    ) +
    labs(
      title = "Distribución de Propiedades por Localidad",
      x = "Longitud",
      y = "Latitud" ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "italic", color = "black", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  #Base test 
  ggplot() +
    geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) + 
    geom_sf(data = sf_test %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 0.8, alpha = 0.6) +
    geom_sf(data = sf_test %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 0.8, alpha = 0.8) +
    scale_fill_manual(
      name = "Localidad",
      values = c(
        "CHAPINERO" = "#ADD8E6", # Azul claro
        .default = "#FAFAFA"     # Gris muy claro
      )) +
    scale_color_manual(
      name = "Tipo de Propiedad", 
      values = c(Apartamento = "#4682B4", Casa = "#d73027") # Azul medio y rojo tomate
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = 2) # Cambia el tamaño de los puntos en la leyenda
      )    ) +
    labs(
      title = "Distribución de Propiedades por Localidad",
      x = "Longitud",
      y = "Latitud" ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "italic", color = "black", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10)
    )
  

  # Encontramos el queremos que sea el centro del mapa 
  latitud_central_train <- mean(train$lat)
  longitud_central_train <- mean(train$lon)
  
  latitud_central_test <- mean(test$lat)
  longitud_central_test <- mean(test$lon)
  
  
#-------------------------------------------------------------------------------
#Creacion de variables a partir de informacions geoespacial ------------------#
#-------------------------------------------------------------------------------
 
 #Sacar informacion espacial de Bogota
  
  # Posibles categorías de las que podemos extraer información geosespacial. 
  print(available_tags("leisure"))
  print(available_features()) # para ver todas las categorias

  #Informacion espacial de Bogots
  bogota<-opq(bbox = getbb("Bogotá Colombia"))
  bogota
  
#Informacion especifica a extraer
  
#--------------------#
#      Parques
#--------------------#
  
    # Extraemos la info de todos los parques de Cali
    parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
      add_osm_feature(key = "leisure" , value = "park") 
  
    # Cambiamos el formato para que sea un objeto sf (simple features)
    parques_sf <- osmdata_sf(parques)
    
    # De las features del parque nos interesa su geometría y donde están ubicados 
    parques_geometria <- parques_sf$osm_polygons %>% 
      dplyr::select(osm_id, name) 
    
    # Guardemos los polígonos de los parques 
    parques_geometria <- st_as_sf(parques_sf$osm_polygons) #Poligonos
  
    # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
    centroides <- st_centroid(parques_geometria, byid = T)
    centroides <- centroides %>%
      mutate(x=st_coordinates(centroides)[, "X"]) %>%
      mutate(y=st_coordinates(centroides)[, "Y"]) 
    
    #Creacion de los mapas con los parques
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central_train, lat = latitud_central_train, zoom = 12) %>%
      addPolygons(data = parques_geometria, col = "red",weight = 10,
                  opacity = 0.8, popup = parques_geometria$name) %>%
      addCircles(lng = centroides$x, 
                 lat = centroides$y, 
                 col = "darkblue", opacity = 0.5, radius = 1)
  
    #Aegurar que la proyeccion de los centroides y datos de propiedades
    centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
    sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
    sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
    
  #Calcular distancia de la propiedad al parque mas cercano --------------------
  
    # Matrix distancia de la propiedad a cada centroide de los parques
    dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
    dim(dist_matrix_t)
    
    dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
    dim(dist_matrix_te) 
    
    #Encontrar la distancia minima a cualquier parque
    dist_min_t <- apply(dist_matrix_t, 1, min) 
    dist_min_te <- apply(dist_matrix_te, 1, min) 
    
  
  # La agregamos como variable a nuestra base de datos original ---
   train <- train %>% mutate(distancia_parque = dist_min_t) #Distancia train
   test <-  test %>% mutate(distancia_parque = dist_min_te) #Distancia minima test

  # Ahora vamos a evaluar si el tamaño del parque más cercano influye
  posicion_t <- apply(dist_matrix_t, 1, function(x) which(min(x) == x))
  posicion_te <- apply(dist_matrix_te, 1, function(x) which(min(x) == x)) 
  
  # De la geometría de los parques extraemos el área
  areas <- st_area(parques_geometria)
  
  #Agregamos la variable a nuestra base de datos original
  
  #Base train
  train <- train %>%
    mutate(area_parque = as.numeric(areas[posicion_t]))
  
  test <- test %>%
    mutate(area_parque = as.numeric(areas[posicion_te]))
  
  
#Graficas  

  #Distribucion de la distancia alos pagues
  
    #Train
  p <- ggplot(train%>%sample_n(1000), aes(x = distancia_parque, y = price)) +
    geom_point(col = "darkblue", alpha = 0.4) +
    labs(x = "Distancia mínima a un parque en metros (log-scale)", 
         y = "Valor de venta  (log-scale)",
         title = "Relación entre la proximidad a un parque y el precio del immueble") +
    scale_x_log10() +
    scale_y_log10(labels = scales::dollar) +
    theme_bw()
  ggplotly(p)
  
    #Relacion
  p <- ggplot(train%>%sample_n(1000), aes(x = area_parque, y = price)) +
    geom_point(col = "darkblue", alpha = 0.4) +
    labs(x = "Área del parque más cercano (log-scale)", 
         y = "Valor del arriendo (log-scale)",
         title = "Relación entre área de un parque y el precio del immueble") +
    scale_x_log10() +
    scale_y_log10(labels = scales::dollar) +
    theme_bw()
  ggplotly(p)
  
#--------------------#
#Estacion de policia
#--------------------#
  
  # Extraemos la info de las estaciones de Bogota
  policia <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "police") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  policia_sf <- osmdata_sf(policia)
  
  # De las features de las estaciones de policia nos interesa su geometría y donde están ubicados 
  policia_geometria <- policia_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de policia 
  policia_geometria <- st_as_sf(policia_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides_p <- st_centroid(policia_geometria, byid = T)
  centroides_p <- centroides_p %>%
    mutate(x=st_coordinates(centroides_p)[, "X"]) %>%
    mutate(y=st_coordinates(centroides_p)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroidesp_sf <- st_as_sf(centroides_p, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  #Calcular distancia de la propiedad al parque mas cercano --------------------
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrixp_t <- st_distance(x = sf_train, y = centroidesp_sf)
  dim(dist_matrix_t)
  
  dist_matrixp_te <- st_distance(x = sf_test, y = centroidesp_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_p_t <- apply(dist_matrixp_t, 1, min) 
  dist_p_te <- apply(dist_matrixp_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_policia = dist_p_t) #Distancia train
  test <-  test %>% mutate(distancia_policia = dist_p_te) #Distancia minima test
  
#--------------------#
#     Gimnasio
#--------------------# 
  
  # Extraemos la info de las estaciones de Bogota
  gym <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "leisure" , value = "fitness_centre") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  gym_sf <- osmdata_sf(gym)
  
  # De las features de las estaciones de policia nos interesa su geometría y donde están ubicados 
  gym_geometria <- gym_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de policia 
  gym_geometria <- st_as_sf(gym_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides_g <- st_centroid(gym_geometria, byid = T)
  centroides_g <- centroides_g %>%
    mutate(x=st_coordinates(centroides_g)[, "X"]) %>%
    mutate(y=st_coordinates(centroides_g)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_g_sf <- st_as_sf(centroides_g, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  #Calcular distancia de la propiedad al parque mas cercano --------------------
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_g_t <- st_distance(x = sf_train, y = centroides_g_sf)
  dim(dist_matrix_t)
  
  dist_matrix_g_te <- st_distance(x = sf_test, y = centroides_g_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_g_t <- apply(dist_matrix_g_t, 1, min) 
  dist_g_te <- apply(dist_matrix_g_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_gym = dist_g_t) #Distancia train
  test <-  test %>% mutate(distancia_gym = dist_g_te) #Distancia minima test
  
  
#--------------------#
#         Bus
#--------------------# 
  
  # Extraemos la info de las estaciones
  bus <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "bus_station") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  bus_sf <- osmdata_sf(bus)
  
  # De las features de las estaciones nos interesa su geometría y donde están ubicados 
  bus_geometria <- bus_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  bus_geometria <- st_as_sf(bus_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(bus_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_bus = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_bus = dist_min_te) #Distancia minima test
  

#--------------------#
#    Supermercados
#--------------------#   
  
  # Extraemos la info 
  super <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "shop" , value = "supermarket") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  super_sf <- osmdata_sf(super)
  
  # De las features nos interesa su geometría y donde están ubicados 
  super_geometria <- super_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  super_geometria <- st_as_sf(super_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(super_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_super = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_super = dist_min_te) #Distancia minima test
  
#--------------------#
#        Bar 
#--------------------#   
  
  # Extraemos la info 
  bar <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "bar") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  bar_sf <- osmdata_sf(super)
  
  # De las features nos interesa su geometría y donde están ubicados 
  bar_geometria <- bar_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  bar_geometria <- st_as_sf(bar_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(bar_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_bar = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_bar = dist_min_te) #Distancia minima test
  
#--------------------#
#       Hospital 
#--------------------#   
  
  # Extraemos la info 
  hospital <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "hospital") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  hospital_sf <- osmdata_sf(hospital)
  
  # De las features nos interesa su geometría y donde están ubicados 
  hospital_geometria <- hospital_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  hospital_geometria <- st_as_sf(hospital_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(hospital_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_hosp = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_hosp = dist_min_te) #Distancia minima test
  
  
#--------------------#
#       Colegio 
#--------------------#   
  
  # Extraemos la info 
  colegio <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "school") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  colegio_sf <- osmdata_sf(colegio)
  
  # De las features nos interesa su geometría y donde están ubicados 
  colegio_geometria <- colegio_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  colegio_geometria <- st_as_sf(colegio_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(colegio_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_cole = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_cole = dist_min_te) #Distancia minima test
  
#--------------------#
#   Centro Comercial 
#--------------------#   

  # Extraemos la info 
  comercial <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "building" , value = "commercial") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  comercial_sf <- osmdata_sf(comercial)
  
  # De las features nos interesa su geometría y donde están ubicados 
  comercial_geometria <- comercial_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  comercial_geometria <- st_as_sf(comercial_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(comercial_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_cc = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_cc = dist_min_te) #Distancia minima test
  
#--------------------#
#      Restaurantes
#--------------------#  
  
  # Extraemos la info 
  restaurantes <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "restaurant") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
 restaurantes_sf <- osmdata_sf(comercial)
  
  # De las features nos interesa su geometría y donde están ubicados 
  restaurantes_geometria <- restaurantes_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
   restaurantes_geometria <- st_as_sf(restaurantes_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(restaurantes_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_rest = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_rest = dist_min_te) #Distancia minima test
  

#--------------------#
#      Libreria
#--------------------#  
  
  # Extraemos la info 
  libreria <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "library") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  libreria_sf <- osmdata_sf(libreria)
  
  # De las features nos interesa su geometría y donde están ubicados 
  libreria_geometria <- libreria_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  libreria_geometria <- st_as_sf(libreria_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(libreria_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_libreria = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_libreria = dist_min_te) #Distancia minima test
  
#--------------------#
#     Universidad
#--------------------#    
  
  # Extraemos la info 
  universidad <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "university") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  universidad_sf <- osmdata_sf(universidad)
  
  # De las features nos interesa su geometría y donde están ubicados 
  universidad_geometria <- universidad_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  universidad_geometria <- st_as_sf(universidad_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(universidad_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_uni = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_uni = dist_min_te) #Distancia minima test
  
#--------------------#
#      Banco
#--------------------#    
  
  # Extraemos la info 
  banco <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = "amenity" , value = "bank") 
  
  # Cambiamos el formato para que sea un objeto sf (simple features)
  banco_sf <- osmdata_sf(banco)
  
  # De las features nos interesa su geometría y donde están ubicados 
  banco_geometria <- banco_sf$osm_polygons %>% 
    dplyr::select(osm_id, name) 
  
  # Guardemos los polígonos de las estaciones de bus 
  banco_geometria <- st_as_sf(banco_sf$osm_polygons) #Poligonos
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  centroides <- st_centroid(banco_geometria, byid = T)
  centroides <- centroides %>%
    mutate(x=st_coordinates(centroides)[, "X"]) %>%
    mutate(y=st_coordinates(centroides)[, "Y"]) 
  
  #Aegurar que la proyeccion de los centroides y datos de propiedades
  centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
  
  # Matrix distancia de la propiedad a cada centroide de los parques
  dist_matrix_t <- st_distance(x = sf_train, y = centroides_sf)
  dim(dist_matrix_t)
  
  dist_matrix_te <- st_distance(x = sf_test, y = centroides_sf)
  dim(dist_matrix_te) 
  
  #Encontrar la distancia minima a cualquier parque
  dist_min_t <- apply(dist_matrix_t, 1, min) 
  dist_min_te <- apply(dist_matrix_te, 1, min) 
  
  # La agregamos como variable a nuestra base de datos original ---
  train <- train %>% mutate(distancia_banco = dist_min_t) #Distancia train
  test <-  test %>% mutate(distancia_banco = dist_min_te) #Distancia minima test 
  
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
# 8. Creacion variables a  parir del texto -------------------------------------
#-------------------------------------------------------------------------------  
  
#------------------------------#
#     Tipo de propiedad
#------------------------------#
  
  train$description[2]
  
  #Normalizar el texto ----
  train <- train %>%
    mutate(description = str_trim(gsub("\\s+", " ", 
                                       str_replace_all(
                                         iconv(str_to_lower(description), from = "UTF-8", to = "ASCII//TRANSLIT"),
                                         "[^[:alnum:]]", " "))))
  
  #Miremos los cambios 
  train$description[2]
  
#Arreglar la variable property type
  
  # Se crea una nueva columna llamada property_type_2. Si "casa" está en la descripción, se asigna "Casa" a esta columna; de lo contrario, se mantiene el valor original de property_type
  train <- train %>%
    mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))
  names(train)
  # Se repite el caso anterior pero ahora buscamos apartamento o apto.
  train <- train %>%
    mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))
  table(train$property_type_2)
  table(train$property_type)
  
  
  # Se crea una nueva columna llamada property_type_2. Si "casa" está en la descripción, se asigna "Casa" a esta columna; de lo contrario, se mantiene el valor original de property_type
  test <- test %>%
    mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))
  names(test)
  # Se repite el caso anterior pero ahora buscamos apartamento o apto.
  test <- test %>%
    mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))
  table(test$property_type_2)
  table(test$property_type)
  
#. Imputacion de las variables con la nueva de propiedad
   
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  
  sapply(train[, c("rooms", "bathrooms", "bedrooms")], class) #Tipo de variable
  train <- train %>% 
    group_by(property_type_2) %>% 
    mutate(
      rooms_imp2 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp2 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp2 = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  
  ##. Imputacion media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  
  #Imputacion media
  train <- train %>% 
    group_by(property_type_2) %>% 
    mutate(
      surface_total_imp_mean2 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean2 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  #Imputacion mediana
  train <- train %>% 
    group_by(property_type_2) %>% 
    mutate(
      surface_total_median2 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median2 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  
  #Base de testeo ----------------
  
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  
  test <- test %>% 
    group_by(property_type_2) %>% 
    mutate(
      rooms_imp2 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp2 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp2 = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  ## Imputación media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  
  # Imputación media
  test <- test %>% 
    group_by(property_type_2) %>% 
    mutate(
      surface_total_imp_mean2 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean2 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  # Imputación mediana
  test <- test %>% 
    group_by(property_type_2) %>% 
    mutate(
      surface_total_median2 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median2 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  
#---------------------#
#      Pisos 
#---------------------#
  
  # Creando nuevas variables usando texto ----------------------------------------

  train <- as.data.frame(train)
  test <- as.data.frame(test)
  
# Numero de pisos : Casa ------------------------------------------------
  
  # Base train
  train <- train %>%
    mutate(n_pisos= str_extract(description, "(\\w+|\\d+) pisos")) %>%
    mutate(n_pisos= ifelse(property_type=="Casa", n_pisos, NA))
  
  #Base test
  test <- test %>%
    mutate(n_pisos= str_extract(description, "(\\w+|\\d+) pisos")) %>%
    mutate(n_pisos= ifelse(property_type=="Casa", n_pisos, NA))
  
  #Convertir los numeros escritos a numericos
  numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", 
                         "diez","once","doce","trece","catorce","quince")
  numeros_numericos <- as.character(2:15)
  
  
  train <- train %>%
    mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))
  train <- train %>%
    mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
    mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
    mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico))
  
  test <- test %>%
    mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))  
  test <- test %>%
    mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
    mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
    mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico))
  
    #Graficas 
  ggplot(train %>% filter(n_pisos_numerico>1), aes(x = factor(n_pisos_numerico))) +
    geom_bar() +
    labs(title = "", x = "Pisos", y = "Obs")+
    theme_minimal()
  
  # Numero de pisos : Apartamento------------------------------------------------
  
#Train
  
  train <- train %>%
    mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))
  numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
  numeros_numericos <- as.character(1:10)
  train <- train %>%
    mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))
  train <- train %>%
    mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))
  train <- train %>%
    mutate(piso_numerico = ifelse(piso_numerico > 35, NA, piso_numerico)) %>%
    mutate(piso_numerico = ifelse(property_type_2=="Casa", 1, piso_numerico))
  train %>%
    filter(property_type_2 == "Apartamento") %>%
    count(piso_numerico)
  train <- train %>%
    mutate(piso_numerico = replace_na(piso_numerico, 2)) #se reemplaza con la moda:2
  
#Test
  test <- test %>%
    mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))
  numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
  numeros_numericos <- as.character(1:10)
  test <- test %>%
    mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))
  test <- test %>%
    mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))
  test <- test %>%
    mutate(piso_numerico = ifelse(piso_numerico > 35, NA, piso_numerico)) %>%
    mutate(piso_numerico = ifelse(property_type_2=="Casa", 1, piso_numerico))
  test %>%
    filter(property_type_2 == "Apartamento") %>%
    count(piso_numerico)
  test <- test %>%
    mutate(piso_numerico = replace_na(piso_numerico, 2)) #se reemplaza con la moda:2
  
  
  
#------------------------------------------------------------------------------#
#  Ejercicios Adicional
#------------------------------------------------------------------------------#
  
#--------------------#
#  Agregar barrio
#--------------------#   
  
#. Pegar variable de barrio 
  
  # Establecer el directorio de trabajo para los datos espaciales de barrios
  setwd(paste0(wd, "\\Datos espaciales\\Barrios")) #Directorio datos
  
  # Leer el archivo GeoJSON con información de los barrios y cargarlo como objeto espacial
  barrios <- st_read("SECTOR.geojson")
  
  # Transformar el sistema de coordenadas de los datos de barrios al sistema CRS 4326 (WGS 84)
  barrios <- st_transform(barrios, 4326)
  
  # Convertir los dataframes `train` y `test` en objetos espaciales `sf` con coordenadas y CRS 4326
  sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
  sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
  
  # Verificar si las geometrías de los barrios son válidas y corregirlas si es necesario
  if (!all(st_is_valid(barrios))) {   # Si alguna geometría no es válida
    barrios <- st_make_valid(barrios) # Corrige las geometrías inválidas
  }
  
  # Realizar una unión espacial entre los puntos de `sf_train` y los polígonos de `barrios`
  # La unión utiliza `st_intersects`, que asigna cada punto al polígono donde está contenido
  sf_train <- st_join(sf_train, barrios, join = st_intersects)
  sf_test <- st_join(sf_test, barrios, join = st_intersects)
  
  # Agregar los nombres de los barrios (`SCANOMBRE`) como una nueva variable en `train` y `test`
  # Cada punto hereda el valor de `SCANOMBRE` del polígono donde está contenido
  train$barrio <- sf_train$SCANOMBRE
  test$barrio <- sf_test$SCANOMBRE
  
#. Crear variables nuevas
  
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  sapply(train[, c("rooms", "bathrooms", "bedrooms")], class) #Tipo de variable
  train <- train %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      rooms_imp3 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp3 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp3 = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  
  ##. Imputacion media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  #Imputacion media
  train <- train %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      surface_total_imp_mean3 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean3 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  #Imputacion mediana
  train <- train %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      surface_total_median3 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median3 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  
  #Base de testeo ----------------
  
  ##.Imputaciones moda en las variables: rooms,bathrooms y bedrooms por tipo de propiedad
  
  test <- test %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      rooms_imp3 = ifelse(is.na(rooms), as.numeric(mlv(rooms, method = "mfv", na.rm = TRUE)), rooms),
      bathrooms_imp3 = ifelse(is.na(bathrooms), as.numeric(mlv(bathrooms, method = "mfv", na.rm = TRUE)), bathrooms),
      bedrooms_imp3 = ifelse(is.na(bedrooms), as.numeric(mlv(bedrooms, method = "mfv", na.rm = TRUE)), bedrooms)
    ) %>%
    ungroup()
  
  ## Imputación media y mediana por tipo de propiedad de las variables: surface_total y surface_covered
  
  # Imputación media
  test <- test %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      surface_total_imp_mean3 = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total),
      surface_covered_imp_mean3 = ifelse(is.na(surface_covered), mean(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
  # Imputación mediana
  test <- test %>% 
    group_by(property_type, barrio) %>% 
    mutate(
      surface_total_median3 = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
      surface_covered_median3 = ifelse(is.na(surface_covered), median(surface_covered, na.rm = TRUE), surface_covered)
    ) %>%
    ungroup()
  
#------------------------------------------------------------------------------#
#  Quitar variables que no son de interes
#------------------------------------------------------------------------------#  
  
  train <- train %>% 
    dplyr::select(-n_pisos, -piso_info)
  names(train)
  test <- test %>% 
    dplyr::select(-n_pisos, -piso_info)
  names(test)
  
#------------------------------------------------------------------------------#
#  Guardar bases de datos
#------------------------------------------------------------------------------#
  setwd(paste0(wd, "\\Bases")) #Directorio datos
  saveRDS(train, "train.rds")  # Guardar el objeto `train`
  saveRDS(test, "test.rds")    # Guardar el objeto `test`
  