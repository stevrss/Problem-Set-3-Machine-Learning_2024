#------------------------------------------------------------------------------#
#---------------------------- EN - Juliet  ------------------------------------#
#------------------------------------------------------------------------------#

setwd(paste0(wd, "\\Bases")) #Directorio datos

# Cargue base de datos ------------------------------------------------------

Train <- readRDS("train.rds")
Test  <- readRDS("test.rds")

#Quitar variables que tiene missings y que ya se imputaron
db_miss_train <- skim(Train)%>% dplyr::select(skim_variable, n_missing)
db_miss_test  <- skim(Test)%>% dplyr::select(skim_variable, n_missing)

train <- Train %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

test <- Test %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

# Procesamiento ----------------------------------------------------------------

# Calcular la moda (el valor m?s frecuente) de la columna localidad
localidad_moda <- as.character(names(sort(table(test$localidad), decreasing = TRUE)[1]))
test$localidad[is.na(test$localidad)] <- localidad_moda

# Se guardan las descripciones en un vector source
descriptions_train <- train$description
des_train_scource <- VectorSource(descriptions_train)
descriptions_test <- test$description
des_test_scource <- VectorSource(descriptions_test)

# Make a volatile corpus: coffee_corpus
des_corpus_train <- VCorpus(des_train_scource, readerControl = list( language = "es"))
des_corpus_test <- VCorpus(des_test_scource, readerControl = list( language = "es"))

# Funci?n para reemplazar n?meros por palabras
reemplazar_numeros <- function(texto) {
  palabras <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  # Reemplazar n?meros del 0 al 10 por palabras
  for (i in 0:10) {
    texto <- gsub(sprintf("\\b%d\\b", i), palabras[i + 1], texto)}
  return(texto)}

# Convertir texto a formato ASCII eliminando tildes y caracteres especiales
eliminar_tildes <- function(texto) {
  texto_sin_tildes <- iconv(texto, "UTF-8", "ASCII", sub = "")
  return(texto_sin_tildes)}

reemplazar_car_especiales <- function(texto) {
  texto_sin_espe <-str_replace_all(texto, "[^[:alnum:]]", " ")
  return(texto_sin_espe)}

## volver a las palabras a sus ra?ces
stem_espanol<-  function(texto) {
  texto_stem <- stemDocument(texto, language="spanish")
  return(texto_stem)}

# Descargamos la lista de las stopwords en espa?ol de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras<- union(lista_palabras,  c("vendo", "venta", "vende", "etc", "carrera", "calle", "casa", "apto", "apartamento",
                                          "ubicado","ubicada") )

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace) ## remover espacios en blanco
  corpus <- tm_map(corpus, removePunctuation)  ## remover puntuaci?m
  corpus <- tm_map(corpus, content_transformer(tolower)) # todo minuscula 
  corpus <- tm_map(corpus, removeWords, c(lista_palabras)) # remover stopwords y otras que se quieran a?dir
  corpus<-  tm_map(corpus, content_transformer(reemplazar_numeros))  ## incluir funciones que nosotros creamos 
  corpus<-  tm_map(corpus, content_transformer(eliminar_tildes))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(reemplazar_car_especiales))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(stem_espanol))
  corpus<-  tm_map(corpus, removeNumbers)  # remover numeros restantes
  return(corpus)}

# apliquemos nuestra funci?n de limpieza:
clean_des_train <- clean_corpus(des_corpus_train)
clean_des_test <- clean_corpus(des_corpus_test)

# crear la document - term matrix
description_dtm_train <- DocumentTermMatrix(clean_des_train)
description_dtm_test <- DocumentTermMatrix(clean_des_test)

# dejar en train solo variables que comparta con test
des_train <- as.data.frame(as.matrix(removeSparseTerms(description_dtm_train, 0.9), sparse=TRUE))
des_test <- as.data.frame(as.matrix(removeSparseTerms(description_dtm_test, 0.9), sparse=TRUE))

var_compartidas <- intersect(names(des_train), names(des_test))
des_train <- des_train[,var_compartidas]
des_test <- des_test[,var_compartidas]

# componentes principales eliminando las q tienen 90% de entradas nulas
pcdescriptions_train <- prcomp(as.matrix(des_train), scale=TRUE)
pcdescriptions_test <- prcomp(as.matrix(des_test), scale=TRUE)

# guardar componentes 
zdes_train <- as.data.frame(predict(pcdescriptions_train)) %>%
  mutate(property_id = train$property_id)

zdes_test <- as.data.frame(predict(pcdescriptions_test)) %>%
  mutate(property_id = test$property_id)

des_train <- des_train %>%
  mutate(property_id = train$property_id)

des_test <- des_test %>%
  mutate(property_id = test$property_id)

# unir bases de datos de texto y de componentes principales
train_full<-  train %>% 
  full_join(des_train, join_by(property_id)) %>%
  full_join(zdes_train, join_by(property_id))

test_full<-  test %>% 
  full_join(des_test, join_by(property_id)) %>%
  full_join(zdes_test, join_by(property_id))

#------------------------------------------------------------------------------
# Elastic Net - Validacion cruzada espacial ------------------------------------
#------------------------------------------------------------------------------

# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(5145)
n_train <- floor(0.8 * nrow(train_full))  

# Creamos los indices
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Dividimos los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full[-train_indices, ]  # Datos de prueba


# Reemplazar NA en las demas variables categóricas por su moda
property_type_2_moda <- as.character(names(sort(table(train2$property_type_2), decreasing = TRUE)[1]))
train2$property_type_2[is.na(train2$property_type_2)] <- property_type_2_moda

localidad_moda <- as.character(names(sort(table(train2$localidad), decreasing = TRUE)[1]))
train2$localidad[is.na(train2$localidad)] <- localidad_moda

barrio_moda <- as.character(names(sort(table(train2$barrio), decreasing = TRUE)[1]))
train2$barrio[is.na(train2$barrio)] <- barrio_moda


# Se especifica la metrica
maeSummary <- function(data, lev = NULL, model = NULL) {
  out <- mean(abs(data$obs - data$pred)) # Calcula el MAE
  names(out) <- "MAE"                   # Nombra la metrica
  return(out)
}


#Definimos el modelo

elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Se define la grilla
grid_values <- grid_regular(penalty(range = c(-4,2)), levels = 50) %>%
  expand_grid(mixture = seq(0, 1, by = 0.1))


# Primera receta
rec_1 <- recipe(
  price ~ distancia_parque + area_parque + distancia_policia + distancia_gym + distancia_bus +
    distancia_super + distancia_bar + distancia_hosp + distancia_cole + distancia_cc +
    distancia_rest + distancia_libreria + distancia_uni + distancia_banco + dist_avenida +
    rooms_imp2 + bedrooms + bathrooms_imp2 + property_type_2 + localidad + barrio + n_pisos_numerico +
    are + parqu + balcon + remodel + sector + surface_covered_imp_mean2 + month + year, data = train2) %>%
  step_interact(terms = ~ distancia_parque:property_type_2 + area_parque:property_type_2) %>%  
  step_interact(terms = ~ distancia_parque:n_pisos_numerico) %>%  
  step_novel(all_nominal_predictors()) %>%   
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>%   
  step_normalize(all_predictors())


# Segunda receta 
rec_2 <- recipe(
  price ~ distancia_parque + area_parque + distancia_policia + distancia_gym + distancia_bus +
    distancia_super + distancia_bar + distancia_hosp + distancia_cole + distancia_cc +
    distancia_rest + distancia_libreria + distancia_uni + distancia_banco + dist_avenida +
    rooms_imp2 + bedrooms + bathrooms_imp2 + property_type_2 + localidad + barrio + n_pisos_numerico +
    are + parqu + balcon + remodel + sector +surface_covered_imp_mean2 + month + year, data = train2) %>%
  step_interact(terms = ~ distancia_parque:property_type_2 + area_parque:property_type_2) %>%  
  step_interact(terms = ~ distancia_parque:n_pisos_numerico) %>%  
  step_poly(distancia_parque, area_parque, degree = 2) %>%  
  step_novel(all_nominal_predictors()) %>%  
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>%  
  step_normalize(all_predictors())


#Flujos de trabajo
workflow_1 <- workflow() %>% 
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

workflow_2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)


# definimos nuestra variable como sf
train_sf <- st_as_sf(
  train2,
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(5145)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)


#Entrenamiento
tune_res1 <- tune_grid(
  workflow_1,        
  resamples = block_folds,  
  grid = grid_values,        
  metrics = metric_set(mae) 
)
collect_metrics(tune_res1)

set.seed(5145)
tune_res2 <- tune_grid(
  workflow_2,       
  resamples = block_folds,  
  grid = grid_values,        
  metrics = metric_set(mae)  
)
collect_metrics(tune_res2)


# Observamos los mejores valores
best_tune_res1 <- select_best(tune_res1, metric = "mae")
best_tune_res1

best_tune_res2<- select_best(tune_res2, metric = "mae")
best_tune_res2


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final <- finalize_workflow(workflow_1, best_tune_res1)
res2_final <- finalize_workflow(workflow_2, best_tune_res2)


EN_final1_fit <- fit(res1_final, data = train2)
EN_final2_fit <- fit(res2_final, data = train2)

# Prediccion dentro de muestra 
augment(EN_final1_fit, new_data = test2) %>%
  mae(truth = price, estimate = .pred)  # presenta un mejor MAE

augment(EN_final2_fit, new_data = test2) %>%
  mae(truth = price, estimate = .pred)


# Prediccion fuera de muestra 
predic_EN_1 <- predict(EN_final1_fit, new_data = test_full)$.pred
test_EN_1 <- test_full %>%
  mutate(price = predic_EN_1) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\EN_VCEspacial"))
write.csv(test_EN_1,"EN_model1_lambda_0.0001_alpha_0.4.csv",row.names = F) 

#-------------------------------------------------------------------------------
# Modelo lineal ----------------------------------------------------------------
#-------------------------------------------------------------------------------

# Configuración inicial y división de los datos -----------------------------

set.seed(5145)  # Fija una semilla para asegurar la reproducibilidad de los resultados.
n_train <- floor(0.8 * nrow(train_full))  # Calcula el tamaño del conjunto de entrenamiento (80% del total).

# Crea índices aleatorios para dividir los datos en entrenamiento y prueba
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Divide los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <- train_full[-train_indices, ]  # Datos de prueba
# Este paso asegura que el modelo pueda entrenarse con una parte de los datos y evaluarse con otra independiente.


#Imputación de valores faltantes en variables categóricas------------------------

# En los datos de entrenamiento (train2)
property_type_2_moda <- as.character(names(sort(table(train2$property_type_2), decreasing = TRUE)[1]))
train2$property_type_2[is.na(train2$property_type_2)] <- property_type_2_moda

localidad_moda <- as.character(names(sort(table(train2$localidad), decreasing = TRUE)[1]))
train2$localidad[is.na(train2$localidad)] <- localidad_moda

barrio_moda <- as.character(names(sort(table(train2$barrio), decreasing = TRUE)[1]))
train2$barrio[is.na(train2$barrio)] <- barrio_moda

# En los datos de prueba (test2)
property_type_2_moda_test <- as.character(names(sort(table(test2$property_type_2), decreasing = TRUE)[1]))
test2$property_type_2[is.na(test2$property_type_2)] <- property_type_2_moda_test

localidad_moda_test <- as.character(names(sort(table(test2$localidad), decreasing = TRUE)[1]))
test2$localidad[is.na(test2$localidad)] <- localidad_moda_test

barrio_moda_test <- as.character(names(sort(table(test2$barrio), decreasing = TRUE)[1]))
test2$barrio[is.na(test2$barrio)] <- barrio_moda_test

# Este bloque reemplaza valores faltantes (NA) en las variables categóricas `property_type_2`, `localidad` y `barrio` por su moda (valor más frecuente). Se aplica tanto al conjunto de entrenamiento como al de prueba para mantener consistencia.

# Especificación y entrenamiento del modelo lineal ------------------------------
linear_model <- lm(
  price ~     
    distancia_parque * property_type_2 +  # Interacción entre proximidad al parque y tipo de propiedad.
    area_parque * property_type_2 +      # Interacción entre área del parque y tipo de propiedad.
    distancia_parque * n_pisos_numerico + # Interacción entre proximidad al parque y número de pisos.
    distancia_super * distancia_cc +     # Interacción entre proximidad a supermercados y centros comerciales.
    poly(distancia_parque, 2) +          # Término polinómico de grado 2 para proximidad al parque.
    poly(area_parque, 2) +               # Término polinómico de grado 2 para área del parque.
    poly(rooms_imp2, 2) +                # Término polinómico de grado 2 para número de habitaciones.
    distancia_policia + distancia_gym + distancia_bus + distancia_bar + 
    distancia_hosp + distancia_cole + distancia_rest + distancia_libreria + 
    distancia_uni + distancia_banco + dist_avenida + 
    rooms_imp2 + bedrooms + bathrooms_imp2 + are + parqu + balcon + remodel +
    property_type_2 + localidad + sector + n_pisos_numerico,
  data = train2
)
# Este modelo lineal incluye interacciones, relaciones polinómicas y variables categóricas automáticamente transformadas en dummy.

# Predicciones en entrenamiento y prueba ---------------------------------------
train2$y_pred <- predict(linear_model, newdata = train2)  # Predicciones en el conjunto de entrenamiento
test2$y_pred <- predict(linear_model, newdata = test2)    # Predicciones en el conjunto de prueba

# Estas predicciones permiten evaluar el desempeño del modelo tanto en los datos con los que fue entrenado como en los datos de prueba.


#Cálculo del MAE (Error Absoluto Medio) ---------------------------------------
mae_train <- mae(data = train2, truth = price, estimate = y_pred)
mae_train$.estimate  # MAE en el conjunto de entrenamiento

mae_test <- mae(data = test2, truth = price, estimate = y_pred)
mae_test$.estimate  # MAE en el conjunto de prueba

# El MAE mide la magnitud promedio de los errores en las predicciones. Si el MAE en `test2` es similar al de `train2`, el modelo generaliza bien.


#Predicción fuera de muestra ---------------------------------------------------
predic_RL <- predict(linear_model, newdata = test_full)  # Predicciones en datos externos (`test_full`)
test_RL_1 <- test_full %>%
  mutate(price = predic_RL) %>%
  select(property_id, price)
# Este paso aplica el modelo a un conjunto de datos externo (`test_full`), generando predicciones para ser usadas en análisis posteriores.

# Guardar predicciones en archivo CSV -----------------------------------------
setwd(paste0(wd,"\\Resultados\\RegLineal"))  # Cambia el directorio de trabajo a "Resultados/RegLineal"
write.csv(test_RL_1, "RegLineal_model2.csv", row.names = FALSE)  # Guarda las predicciones en un archivo CSV.


