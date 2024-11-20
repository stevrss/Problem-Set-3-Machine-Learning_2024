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
  stargazer(train)
  stargazer(train,type="text")
  
#-------------------------------------------------------------------------------
# 6. Mirar distribuciones de las variables numericas----------------------------
#-------------------------------------------------------------------------------   
  
  
  