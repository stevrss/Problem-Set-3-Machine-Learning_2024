#------------------------------------------------------------------------------#
#---------------------- LIMPIEZA DATA - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 1. Cargar base de datos de entrenamiento y testeo ----------------------------
#-------------------------------------------------------------------------------

  setwd(paste0(wd,"/Datos")) #Directorio datos 

  ### Base de datos de entrenamiento

  train <- read.csv("train.csv") 
  length(train) # 16 variables 
  nrow(train) #38.644 observaciones   
  
  ### Base de datos de testeo
  
  test <- read.csv("test.csv") # Base de testeo
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
  
  # Eliminamos las observaciones que no tienen información de latitud o longitud
  train <- train %>%
    filter(!is.na(lat) & !is.na(lon))
  dim(train)
  
  
# Verificar la cantidad de valores NA en lon y lat (No hay NA)
    sum(is.na(train$lon))  # Número de NA en la columna lon
    sum(is.na(train$lat))  # Número de NA en la columna lat
  
# Observamos la primera visualización
    
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
  local <- st_transform(local,4626)
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
  sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4626)
  sf_test<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4626)

  # Realizar la unión espacial basada en la proximidad de coordenadas
  
    #Base train
  sf_train <- st_join(sf_train, local, join = st_intersects)
  names(sf_train)
  dim(sf_train)
  
    #Base test
  sf_test <- st_join(sf_test, local, join = st_intersects)
  names(sf_test)
  dim(sf_test)
  
  # Agregar variable a train y test  
  train$localidad <- sf_train$Nombre.de.la.localidad
  names(train)
  
  test$localidad <- sf_test$Nombre.de.la.localidad
  names(test)
  
#Mapas --------------------------------------
  
  #Base train
  ggplot()+
    geom_sf(data=local, color = "blue") + #shapefile de comunas
    geom_sf(data=sf_train,aes(color = precio_mt2) ,shape=15, size=0.3)+
    theme_bw()
  
  #Base test  
  ggplot()+
    geom_sf(data=local, color = "blue") + 
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
    parques_geometria <- st_as_sf(parques_sf$osm_polygons)
  
    # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
    centroides <- st_centroid(parques_geometria, byid = T)
    centroides <- centroides %>%
      mutate(x=st_coordinates(centroides)[, "X"]) %>%
      mutate(y=st_coordinates(centroides)[, "Y"]) 
    
    #Creacion de los mapas con los parques
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolygons(data = parques_geometria, col = "red",weight = 10,
                  opacity = 0.8, popup = parques_geometria$name) %>%
      addCircles(lng = centroides$x, 
                 lat = centroides$y, 
                 col = "darkblue", opacity = 0.5, radius = 1)
  
    #Aegurar que la proyeccion de los centroides y datos de propiedades
    centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
    sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
  
  #Calcular distancia de la propiedad al parque mas cercano -----
  
    # Esto va a ser demorado!
    dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
    dim(dist_matrix)
  
    #Encontrar la distancia minima a cualquier parque
    dist_min <- apply(dist_matrix, 1, min)  
  
  # La agregamos como variable a nuestra base de datos original ---
   train <- train %>% mutate(distancia_parque = dist_min)
  
  #Distribucion de la distancia alos pagues
   p <- ggplot(train%>%sample_n(1000), aes(x = distancia_parque, y = price)) +
     geom_point(col = "darkblue", alpha = 0.4) +
     labs(x = "Distancia mínima a un parque en metros (log-scale)", 
          y = "Valor de venta  (log-scale)",
          title = "Relación entre la proximidad a un parque y el precio del immueble") +
     scale_x_log10() +
     scale_y_log10(labels = scales::dollar) +
     theme_bw()
   ggplotly(p)
  
  
  # Ahora vamos a evaluar si el tamaño del parque más cercano influye
  posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
  # De la geometría de los parques extraemos el área
  areas <- st_area(parques_geometria)
  #Agregamos la variable a nuestra base de datos original
  train <- train %>%
    mutate(area_parque = as.numeric(areas[posicion]))
  
    
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
  

  
#--------------------#
#      Barrios
#--------------------#
  

  
#-------------------------------------------------------------------------------
# 8. Creacion variables a  parir del texto -------------------------------------
#-------------------------------------------------------------------------------  
  
#Limpieza usando texto ----------------------------------------------------
  train$description[2]
  
  #Normalizar el texto ----
  
  # Todo en minúscula
  train <- train %>%
    mutate(description = str_to_lower(description))
  # Eliminamos tildes
  train <- train %>%
    mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
  # Eliminamos caracteres especiales
  train <- train %>%
    mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
  # Eliminamos espacios extras
  train <- train %>%
    mutate(description = str_trim(gsub("\\s+", " ", description)))
  
  #Miremos los cambios 
  train$description[2]
  
#Arreglar la variable property type
  
  # Se crea una nueva columna llamada property_type_2. Si "casa" está en la descripción, se asigna "Casa" a esta columna; de lo contrario, se mantiene el valor original de property_type
  train <- train %>%
    mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))
  
  # Se repite el caso anterior pero ahora buscamos apartamento o apto.
  train <- train %>%
    mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))%>%
    select(-property_type)   
  table(train$property_type_2)
  table(train$property_type)
  
#---------------------#
#      Pisos 
#---------------------#
  
  # Creando nuevas variables usando texto ----------------------------------------

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
  
  test <- test %>%
    mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))  
  
  