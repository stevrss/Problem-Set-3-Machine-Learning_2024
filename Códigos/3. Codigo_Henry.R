
# Se establece la direccion de la base de datos
setwd(paste0(wd,"\\Bases"))

# Cargue base de datos ------------------------------------------------------
Train <- readRDS("train.rds")
Test  <- readRDS("test.rds")

db_miss_train <- skim(Train)%>% dplyr::select(skim_variable, n_missing)
db_miss_test  <- skim(Test)%>% dplyr::select(skim_variable, n_missing)

train <- Train %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

test <- Test %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

# Procesamiento ----------------------------------------------------------

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


# Elastic Net - Validacion cruzada espacial -----------------------------------------------------------

# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(1234)
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
install.packages("tidymodels")
library(tidymodels)

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
  are + parqu + balcon + remodel + sector, data = train2) %>%
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
  are + parqu + balcon + remodel + sector, data = train2) %>%
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

set.seed(86936)
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


set.seed(86937)

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
write.csv(test_EN_1,"EN_model1_lambda_0.0001_alpha_0.csv",row.names = F) 




# Superlerner ------------------------------------------------------------------

library(ranger)
library(randomForest)

# Se especifica la metrica
maeSummary <- function(data, lev = NULL, model = NULL) {
  out <- mean(abs(data$obs - data$pred)) # Calcula el MAE
  names(out) <- "MAE"                   # Nombra la metrica
  return(out)
}




# Se ajusta el superlerner

p_load("SuperLearner")

custom_ranger <- create.Learner("SL.ranger", params = list(num.trees = 1000))

custom_rf <- create.Learner("SL.randomForest",
                            tune = list(mtry = round(c(1, sqrt(4), 3))))
custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=3)))

sl.lib <- c("SL.randomForest", "SL.lm",custom_ranger$names,custom_rf$names, custon_glmnet$names)
sl.lib


# Lista de variables relevantes
variables_relevantes <- c(
  "distancia_parque", "area_parque", "distancia_policia", "distancia_gym", "distancia_bus",
  "distancia_super", "distancia_bar", "distancia_hosp", "distancia_cole", "distancia_cc",
  "distancia_rest", "distancia_libreria", "distancia_uni", "distancia_banco", "dist_avenida",
  "rooms_imp2", "bedrooms", "bathrooms_imp2", "abiert", "acab", "acces", "alcob", "ampli", 
  "are", "ascensor", "balcon", "ban", "bogot", "buen", "centr", "cerc", "cerr", "cocin", 
  "comedor", "comercial", "comunal", "cuart", "cubiert", "cuent", "deposit", "dos", 
  "edifici", "espaci", "estudi", "garaj", "gas", "gimnasi", "habit", "habitacion", "hermos", 
  "ilumin", "independient", "integral", "interior", "lavanderi", "lind", "mader", "mts", 
  "natural", "parqu", "parqueader", "pis", "principal", "priv", "remodel", "rop", "sal", 
  "salon", "sector", "segur", "servici", "social", "terraz", "tres", "ubicacion", "uno", 
  "vias", "vigil", "visit", "n_pisos_numerico", "zon", 
  "property_type_2", "localidad"
)
categoricas <- c("property_type_2", "localidad")  # Variables categóricas

# Filtrar las variables relevantes
train2_filtrado <- train2[, c("price", variables_relevantes)]
train2_filtrado[categoricas] <- lapply(train2_filtrado[categoricas], as.factor) # se convierte en factores

# Separar las variables predictoras (X) y la variable objetivo (Y)
X <- train2_filtrado[, variables_relevantes]  
Y <- train2_filtrado$price                


#Se entrena el modelo
Superlerner_model <- SuperLearner(
  Y = Y, 
  X = X, 
  method = "method.NNLS",  
  SL.library = sl.lib,    
  cvControl = list(V = 10) 
)

Superlerner_model


# Filtrar y procesar las variables relevantes en el conjunto de prueba
test2_filtrado <- test2[, c("property_id", "price", variables_relevantes)]
test2_filtrado[categoricas] <- lapply(test2_filtrado[categoricas], as.factor)

X_test <- test2_filtrado[, variables_relevantes]

# Realizar predicciones con el modelo entrenado
precio_superlerner <- predict(Superlerner_model, newdata = X_test, onlySL = TRUE)

# Calcular el MAE en el conjunto de prueba
mae <- maeSummary(data.frame(obs = test2_filtrado$price, pred = precio_superlerner$pred))
print(mae)


# Guardar los resultados con property_id y el precio predicho
results <- data.frame(
  property_id = test2_filtrado$property_id,
  price = precio_superlerner$pred
)

# Mostrar resultados finales
print(results)





# Regresion lineal ------------------------------------------------------------


# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(5678)
n_train <- floor(0.8 * nrow(train_full))  

# Creamos los indices
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Dividimos los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full[-train_indices, ]  # Datos de prueba



# Reemplazar NA en las demas variables categóricas por su moda

#En train
property_type_2_moda <- as.character(names(sort(table(train2$property_type_2), decreasing = TRUE)[1]))
train2$property_type_2[is.na(train2$property_type_2)] <- property_type_2_moda

localidad_moda <- as.character(names(sort(table(train2$localidad), decreasing = TRUE)[1]))
train2$localidad[is.na(train2$localidad)] <- localidad_moda

barrio_moda <- as.character(names(sort(table(train2$barrio), decreasing = TRUE)[1]))
train2$barrio[is.na(train2$barrio)] <- barrio_moda

#En test
property_type_2_moda_test <- as.character(names(sort(table(test2$property_type_2), decreasing = TRUE)[1]))
test2$property_type_2[is.na(test2$property_type_2)] <- property_type_2_moda_test

localidad_moda_test <- as.character(names(sort(table(test2$localidad), decreasing = TRUE)[1]))
test2$localidad[is.na(test2$localidad)] <- localidad_moda_test

barrio_moda_test <- as.character(names(sort(table(test2$barrio), decreasing = TRUE)[1]))
test2$barrio[is.na(test2$barrio)] <- barrio_moda_test


# Especificamos del modelo
linear_model <- lm(price ~ distancia_parque * property_type_2 + area_parque * property_type_2 +
              distancia_parque * n_pisos_numerico + 
              poly(distancia_parque, 2) + poly(area_parque, 2) +
              distancia_policia + distancia_gym + distancia_bus +
              distancia_super + distancia_bar + distancia_hosp + distancia_cole + distancia_cc +
              distancia_rest + distancia_libreria + distancia_uni + distancia_banco + dist_avenida +
              rooms_imp2 + bedrooms + bathrooms_imp2 + property_type_2 + localidad + 
              n_pisos_numerico + are + parqu + balcon + remodel + sector, 
            data = train2)

# Predicciones en los datos de entrenamiento
train2$y_pred <- predict(linear_model, newdata = train2)

# Predicciones en los datos de prueba
test2$y_pred <- predict(linear_model, newdata = test2)

#Calculo del MAE
mae_train <- mae(data = train2, truth = price, estimate = y_pred)
mae_train$.estimate

mae_test <- mae(data = test2, truth = price, estimate = y_pred)
mae_test$.estimate



# Prediccion fuera de muestra 
predic_RL <- predict(linear_model, newdata = test_full)
test_RL_1 <- test_full %>%
  mutate(price = predic_RL) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\RegLineal"))
write.csv(test_RL_1,"RegLineal_model1.csv",row.names = F) 




# Regresion lineal con logaritmo ------------------------------------------


# Aplicar logaritmo al precio
train2$log_price <- log1p(train2$price)
test2$log_price <- log1p(test2$price)

# Modelo
linear_model_log <- lm(log_price ~ distancia_parque * property_type_2 + area_parque * property_type_2 +
                     distancia_parque * n_pisos_numerico + 
                     poly(distancia_parque, 2) + poly(area_parque, 2) +
                     distancia_policia + distancia_gym + distancia_bus +
                     distancia_super + distancia_bar + distancia_hosp + distancia_cole + distancia_cc +
                     distancia_rest + distancia_libreria + distancia_uni + distancia_banco + dist_avenida +
                     rooms_imp2 + bedrooms + bathrooms_imp2 + property_type_2 + localidad + 
                     n_pisos_numerico + are + parqu + balcon + remodel + sector + 
                    distancia_parque * area_parque + rooms_imp2 * bedrooms, 
                   data = train2)


# Predicciones en los datos de entrenamiento
train2$log_y_pred <- predict(linear_model_log, newdata = train2)
train2$y_pred <- expm1(train2$log_y_pred)

test2$log_y_pred <- predict(linear_model_log, newdata = test2)
test2$y_pred <- expm1(test2$log_y_pred)

# Calcular el MAE en entrenamiento y prueba
mae_train2 <- mae(data = train2, truth = price, estimate = y_pred)
mae_train2$.estimate

mae_test2 <- mae(data = test2, truth = price, estimate = y_pred)
mae_test2$.estimate

# Prediccion fuera de muestra 
predic_RL_2 <- predict(linear_model_log, newdata = test_full)
test_RL_2 <- test_full %>%
  mutate(price = predic_RL_2) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\RegLineal"))
write.csv(test_RL_1,"RegLineal_model2.csv",row.names = F) 



# Red Neuronal ------------------------------------------------------------

# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(0987)
n_train <- floor(0.8 * nrow(train_full))  

# Creamos los indices
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Dividimos los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full[-train_indices, ]  # Datos de prueba


# Reemplazar NA en las demas variables categóricas por su moda

#En train
property_type_2_moda <- as.character(names(sort(table(train2$property_type_2), decreasing = TRUE)[1]))
train2$property_type_2[is.na(train2$property_type_2)] <- property_type_2_moda

localidad_moda <- as.character(names(sort(table(train2$localidad), decreasing = TRUE)[1]))
train2$localidad[is.na(train2$localidad)] <- localidad_moda

barrio_moda <- as.character(names(sort(table(train2$barrio), decreasing = TRUE)[1]))
train2$barrio[is.na(train2$barrio)] <- barrio_moda

#En test
property_type_2_moda_test <- as.character(names(sort(table(test2$property_type_2), decreasing = TRUE)[1]))
test2$property_type_2[is.na(test2$property_type_2)] <- property_type_2_moda_test

localidad_moda_test <- as.character(names(sort(table(test2$localidad), decreasing = TRUE)[1]))
test2$localidad[is.na(test2$localidad)] <- localidad_moda_test

barrio_moda_test <- as.character(names(sort(table(test2$barrio), decreasing = TRUE)[1]))
test2$barrio[is.na(test2$barrio)] <- barrio_moda_test


# Aplicar logaritmo al precio
train2$log_price <- log1p(train2$price)
test2$log_price <- log1p(test2$price)

#Formula
formula_nnet <- as.formula(
  "log_price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector"
)

# Crear receta con interacciones
recipe_nnet <- recipe(formula_nnet, data = train2) %>%
  step_novel(all_nominal_predictors()) %>%          # Maneja clases no vistas
  step_dummy(all_nominal_predictors()) %>%         # Crear variables dummy
  step_zv(all_predictors()) %>%                    # Eliminar varianza cero
  step_normalize(all_numeric_predictors())    # Normalizar predictores



#Se define la validación cruzada espacial 
train_sf_nnet <- st_as_sf(
  train2, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf_nnet, v = 5)

#Se define el modelo y la grilla
nnet_tune <- 
  mlp(hidden_units =tune(), epochs = tune(), penalty =tune() ) %>% 
  set_mode("regression") %>% 
  set_engine("nnet", trace = 0) %>%
  translate()

grid_values <- crossing( 
  hidden_units = seq(from= 10, to=30, by = 10),
  epochs =  seq(from= 100, to=200, by = 100),
  penalty = 10^seq(from=-3,to=-1, by=0.5 )
)

# Establecemos el flujo de trabajo
workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_tune) 

# Se realiza la validación cruzada espacial
set.seed(87436)

tune_nnet <- tune_grid(
  workflow_tune,        
  resamples = block_folds,
  grid = grid_values,        
  metrics = metric_set(mae)  # métrica MAE
)

#Se escogen las mejores metricas
best_tune_nnet <- select_best(tune_nnet, metric = "mae")
best_tune_nnet

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parámetros
nnet_tuned_final <- finalize_workflow(workflow_tune, best_tune_nnet)
nnet_tuned_final_fit <- fit(nnet_tuned_final, data = train2)

# Evaluación en el test
test_predictions <- augment(nnet_tuned_final_fit, new_data = test2) %>%
  mutate(price_pred = expm1(.pred))  # Devolver al valor original

# Calcular MAE
mae_result <- mae(
  data = test_predictions,
  truth = price,  # Variable objetivo transformada
  estimate = .pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_1 <- predict(nnet_tuned_final_fit, new_data = test_full) %>%
  mutate(price_pred = expm1(.pred)) # Devolver al valor original
test_nnet_1 <- test_full %>%
  mutate(price = predic_nnet_1$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_1,"NeuralNetwork_hidden_units_10_penalty_0.001_epochs_200.csv",row.names = F) 






# Red Neuronal 2 capas ------------------------------------------------------------

# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(0987)
n_train <- floor(0.8 * nrow(train_full))  

# Creamos los indices
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Dividimos los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full[-train_indices, ]  # Datos de prueba


# Reemplazar NA en las demas variables categóricas por su moda

#En train
property_type_2_moda <- as.character(names(sort(table(train2$property_type_2), decreasing = TRUE)[1]))
train2$property_type_2[is.na(train2$property_type_2)] <- property_type_2_moda

localidad_moda <- as.character(names(sort(table(train2$localidad), decreasing = TRUE)[1]))
train2$localidad[is.na(train2$localidad)] <- localidad_moda

barrio_moda <- as.character(names(sort(table(train2$barrio), decreasing = TRUE)[1]))
train2$barrio[is.na(train2$barrio)] <- barrio_moda

#En test
property_type_2_moda_test <- as.character(names(sort(table(test2$property_type_2), decreasing = TRUE)[1]))
test2$property_type_2[is.na(test2$property_type_2)] <- property_type_2_moda_test

localidad_moda_test <- as.character(names(sort(table(test2$localidad), decreasing = TRUE)[1]))
test2$localidad[is.na(test2$localidad)] <- localidad_moda_test

barrio_moda_test <- as.character(names(sort(table(test2$barrio), decreasing = TRUE)[1]))
test2$barrio[is.na(test2$barrio)] <- barrio_moda_test


#Formula
formula_nnet_2 <- as.formula(
  "price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector"
)



recipe_nnet_2 <-
  recipe(formula_nnet_2, data = train2)  %>%
  step_novel(all_nominal_predictors()) %>%          # Maneja clases no vistas
  step_dummy(all_nominal_predictors()) %>%         # Crear variables dummy
  step_zv(all_predictors()) %>%                    # Eliminar varianza cero
  step_normalize(all_numeric_predictors())    # Normalizar predictores

#Se ajusta el modelo
nn_b<-  brulee_mlp(recipe_nnet_2, 
                   train2,
                   epochs = 200, 
                   hidden_units = c(10,10),
                   activation = c("relu", "relu"),
                   learn_rate = 0.01,
                   penalty =  0.001, 
                   dropout= 0.1, 
                   stop_iter= 100, 
                   validation=0.2)



# Evaluación en el test
test_predictions_2 <- predict(nn_b, test2) %>% 
  mutate(price_pred = .pred)  # Devolver al valor original

test2 <- test2 %>%
  mutate(price_pred = test_predictions_2$price_pred)


# Calcular MAE
mae_result <- mae(
  data = test2,
  truth = price,  # Variable objetivo transformada
  estimate = price_pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_2 <- predict(nn_b, new_data = test_full) %>%
  mutate(price_pred = .pred) 
test_nnet_2 <- test_full %>%
  mutate(price = predic_nnet_2$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_2,"NeuralNetwork_2capas_hidden_units_10_penalty_0.001_epochs_200.csv",row.names = F) 





# Red Neuronal 3 capas con logaritmo del precio ------------------------------------------------------------


install.packages("recipes")  # Instala el paquete si no lo tienes
library(recipes)  

# Aplicar logaritmo al precio
train2$log_price <- log1p(train2$price)
test2$log_price <- log1p(test2$price)


#Formula
formula_nnet_2_log <- as.formula(
  "log_price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector"
)



recipe_nnet_2_log <-
  recipe(formula_nnet_2_log, data = train2)  %>%
  step_novel(all_nominal_predictors()) %>%          # Maneja clases no vistas
  step_dummy(all_nominal_predictors()) %>%         # Crear variables dummy
  step_zv(all_predictors()) %>%                    # Eliminar varianza cero
  step_normalize(all_numeric_predictors())    # Normalizar predictores

#Se ajusta el modelo
nn_b_log<-  brulee_mlp(recipe_nnet_2_log, 
                   train2,
                   epochs = 200, 
                   hidden_units = c(10,10,10),
                   activation = c("relu", "relu","relu"),
                   learn_rate = 0.01,
                   penalty =  0.001, 
                   dropout= 0.1, 
                   stop_iter= 100, 
                   validation=0.2)



# Evaluación en el test
test_predictions_2_log <- predict(nn_b_log, test2) %>% 
  mutate(price_pred = expm1(.pred))  # Devolver al valor original

test2 <- test2 %>%
  mutate(price_pred = test_predictions_2_log$price_pred)


# Calcular MAE
mae_result <- mae(
  data = test2,
  truth = price,  # Variable objetivo transformada
  estimate = price_pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_2_log <- predict(nn_b_log, new_data = test_full) %>%
  mutate(price_pred = expm1(.pred)) 
test_nnet_2_log <- test_full %>%
  mutate(price = predic_nnet_2_log$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_2_log,"NeuralNetwork_2capas_hidden_units_10_penalty_0.001_epochs_200.csv",row.names = F) 




# Red Neuronal 3 capas con logaritmo, 2do modelo ------------------------------------------------------------


#Formula
formula_nnet_2_log <- as.formula(
  "log_price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector"
)

recipe_nnet_2_log <-
  recipe(formula_nnet_2_log, data = train2)  %>%
  step_novel(all_nominal_predictors()) %>%          # Maneja clases no vistas
  step_dummy(all_nominal_predictors()) %>%         # Crear variables dummy
  step_zv(all_predictors()) %>%                    # Eliminar varianza cero
  step_normalize(all_numeric_predictors())    # Normalizar predictores

# Definimos validación cruzada espacial 
train_sf_nnet <- st_as_sf(
  train2, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf_nnet, v = 5)

#Se ajusta el modelo

nnet_tune_log <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    dropout =tune(),
    epochs = tune(),
    learn_rate =tune(),
    activation = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("brulee")

#Se define la grilla
grid_values_nnet <- crossing( 
  hidden_units = list(c(5,10,10), c(10,10,10), c(5,10,20), c(20,10,5)),
  penalty = 10^seq(from=-3,to=-2, by=0.5 ),
  dropout = 0,
  epochs = 200,
  learn_rate =0.1,
  activation = list(c("relu", "relu", "relu"))
)

#Se especifica el nuevo workflow
workflow_tune_nnet <- workflow() %>% 
  add_recipe(recipe_nnet_2_log) %>%
  add_model(nnet_tune_log) 

#Se entrena el modelo
set.seed(86936)

tune_nnet_log <- tune_grid(
  workflow_tune_nnet,         
  resamples = block_folds,  
  grid = grid_values_nnet,        
  metrics = metric_set(mae)  
)
#Se escoge el mejor
best_tune_nnet <- select_best(tune_nnet_log, metric = "mae")
best_tune_nnet$hidden_units   ## 20 10  5


set.seed(270499)
nn_tune_log<-brulee_mlp(recipe_nnet_2_log, 
                     train2,
                     hidden_units = c(20,10,5),
                     penalty = best_tune_nnet$penalty[1],
                     dropout = best_tune_nnet$dropout[1],
                     epochs = 300,
                     learn_rate =best_tune_nnet$learn_rate[1],
                     activation = c("relu", "relu","relu"),
                     validation= 0, 
) 



# Evaluación en el test
test_predictions_tune_log <- predict(nn_tune_log, test2) %>% 
  mutate(price_pred = expm1(.pred))  # Devolver al valor original

test2 <- test2 %>%
  mutate(price_pred = test_predictions_tune_log$price_pred)


# Calcular MAE
mae_result <- mae(
  data = test2,
  truth = price,  # Variable objetivo transformada
  estimate = price_pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_tune_log <- predict(nn_tune_log, new_data = test_full) %>%
  mutate(price_pred = expm1(.pred)) 
test_nnet_tune_log <- test_full %>%
  mutate(price = predic_nnet_tune_log$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_tune_log,"NeuralNetwork_3capas_hidden_units_(20,10,5)_penalty_0.01_epochs_200.csv",row.names = F) 





# Red Neuronal 3 capas con logaritmo segundo  ------------------------------------------------------------



#Formula
formula_nnet_3_log <- as.formula(
  "log_price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector + segur + estrato_imp"
)

recipe_nnet_3_log <-
  recipe(formula_nnet_3_log, data = train2)  %>%
  step_novel(all_nominal_predictors()) %>%          # Maneja clases no vistas
  step_dummy(all_nominal_predictors()) %>%         # Crear variables dummy
  step_zv(all_predictors()) %>%                    # Eliminar varianza cero
  step_normalize(all_numeric_predictors())    # Normalizar predictores

# Definimos validación cruzada espacial 
train_sf_nnet <- st_as_sf(
  train2, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf_nnet, v = 5)

#Se ajusta el modelo

nnet_tune2_log <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    dropout =tune(),
    epochs = tune(),
    learn_rate =tune(),
    activation = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("brulee")

#Se define la grilla
grid_values_nnet <- crossing( 
  hidden_units = list(c(10, 10, 10), c(30, 20, 10), c(5,10,20), c(20,10,5), c(10,5,10), c(20,10,20)),
  penalty = 10^seq(from=-4,to=-2, by=0.5 ),
  dropout = 0,
  epochs = c(200, 300),
  learn_rate =c(0.1, 0.01),
  activation = list(c("relu", "relu", "relu"))
)

#Se especifica el nuevo workflow
workflow_tune2_nnet <- workflow() %>% 
  add_recipe(recipe_nnet_3_log) %>%
  add_model(nnet_tune2_log) 

#Se entrena el modelo
set.seed(869361)

tune_nnet_log <- tune_grid(
  workflow_tune2_nnet,         
  resamples = block_folds,  
  grid = grid_values_nnet,        
  metrics = metric_set(mae)  
)
#Se escoge el mejor
best_tune_nnet <- select_best(tune_nnet_log, metric = "mae")
best_tune_nnet$hidden_units   ## 20 10  5


set.seed(270499)
nn_tune2_log<-brulee_mlp(recipe_nnet_3_log, 
                        train2,
                        hidden_units = c(20,10,5),
                        penalty = best_tune_nnet$penalty[1],
                        dropout = best_tune_nnet$dropout[1],
                        epochs = best_tune_nnet$epochs[1],
                        learn_rate =best_tune_nnet$learn_rate[1],
                        activation = c("relu", "relu","relu"),
                        stop_iter = 100, 
                        validation= 0, 
) 



# Evaluación en el test
test_predictions_tune2_log <- predict(nn_tune2_log, test2) %>% 
  mutate(price_pred = expm1(.pred))  # Devolver al valor original

test2 <- test2 %>%
  mutate(price_pred = test_predictions_tune2_log$price_pred)


# Calcular MAE
mae_result <- mae(
  data = test2,
  truth = price,  # Variable objetivo transformada
  estimate = price_pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_tune2_log <- predict(nn_tune2_log, new_data = test_full) %>%
  mutate(price_pred = expm1(.pred)) 
test_nnet_tune2_log <- test_full %>%
  mutate(price = predic_nnet_tune2_log$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_tune2_log,"NeuralNetwork_3capas_hidden_units_(20,10,5)_penalty_0.01_epochs_200.csv",row.names = F) 


























# Red Neuronal 3 capas con logaritmo, 2do modelo ------------------------------------------------------------


#Formula
formula_nnet_2_log <- as.formula(
  "log_price ~ distancia_parque + area_parque + 
                 distancia_policia + distancia_gym + distancia_bus +
                 distancia_super + distancia_bar + distancia_hosp + distancia_cole + 
                 distancia_cc + distancia_rest + distancia_libreria + distancia_uni + 
                 distancia_banco + dist_avenida + rooms_imp2 + bedrooms + bathrooms_imp2 + 
                 property_type_2 + localidad + n_pisos_numerico + are + parqu + balcon + 
                 remodel + sector + ascensor+ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                 PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 +
                 PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + 
                 PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + 
                 PC40 + PC41 + PC42 "
)


# Preparación de datos
train2 <- train2 %>%
  mutate(
    across(c(property_type_2, localidad, sector), as.factor)
  )

# Receta
recipe_nnet_2_log <- 
  recipe(formula_nnet_2_log, data = train2) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Definimos validación cruzada espacial 
train_sf_nnet <- st_as_sf(
  train2, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf_nnet, v = 5)

#Se ajusta el modelo

nnet_tune_log <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    dropout =tune(),
    epochs = tune(),
    learn_rate =tune(),
    activation = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("brulee")

#Se define la grilla
grid_values_nnet <- crossing( 
  hidden_units = list(c(20,10,5), c(10,5,10), c(5,10,15)),
  penalty = 10^seq(from=-3,to=-2, by=0.5 ),
  dropout = 0,
  epochs = 500,
  learn_rate =0.1,
  activation = list(c("relu", "relu", "relu"))
)

#Se especifica el nuevo workflow
workflow_tune_nnet <- workflow() %>% 
  add_recipe(recipe_nnet_2_log) %>%
  add_model(nnet_tune_log) 

#Se entrena el modelo
set.seed(86936)

tune_nnet_log <- tune_grid(
  workflow_tune_nnet,         
  resamples = block_folds,  
  grid = grid_values_nnet,        
  metrics = metric_set(mae)  
)
#Se escoge el mejor
best_tune_nnet <- select_best(tune_nnet_log, metric = "mae")
best_tune_nnet$hidden_units   ## 20 10  5


set.seed(270499)
nn_tune_log<-brulee_mlp(recipe_nnet_2_log, 
                        train2,
                        hidden_units = c(20,10,5),
                        penalty = best_tune_nnet$penalty[1],
                        dropout = 0,
                        epochs = 500,
                        learn_rate =best_tune_nnet$learn_rate[1],
                        activation = c("relu", "relu","relu"),
                        validation= 0, 
) 



test2 <- test2 %>%
  mutate(
    across(c(property_type_2, localidad, sector), as.factor)
  )


# Evaluación en el test
test_predictions_tune_log <- predict(nn_tune_log, test2) %>% 
  mutate(price_pred = expm1(.pred))  # Devolver al valor original

test2 <- test2 %>%
  mutate(price_pred = test_predictions_tune_log$price_pred)


# Calcular MAE
mae_result <- mae(
  data = test2,
  truth = price,  # Variable objetivo transformada
  estimate = price_pred    # Predicciones
)
mae_result


# Prediccion fuera de muestra 
predic_nnet_tune_log <- predict(nn_tune_log, new_data = test_full) %>%
  mutate(price_pred = expm1(.pred)) 
test_nnet_tune_log <- test_full %>%
  mutate(price = predic_nnet_tune_log$price_pred) %>%
  select(property_id, price)


# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\NeuralNetwork"))
write.csv(test_nnet_tune_log,"NeuralNetwork_3capas_hidden_units_(20,10,5)_penalty_0.01_epochs_200.csv",row.names = F) 




