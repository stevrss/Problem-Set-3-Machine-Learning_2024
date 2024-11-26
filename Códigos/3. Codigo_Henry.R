
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



# Se especifica la metrica
maeSummary <- function(data, lev = NULL, model = NULL) {
  out <- mean(abs(data$obs - data$pred)) # Calcula el MAE
  names(out) <- "MAE"                   # Nombra la metrica
  return(out)
}




elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Se define la grilla
grid_values <- grid_regular(penalty(range = c(-4,1)), levels = 50) %>%
  expand_grid(mixture = seq(0, 1, by = 0.1))


# Primera receta
rec_1 <- recipe(
  price ~ distancia_parque + area_parque + distancia_policia + distancia_gym + distancia_bus +
  distancia_super + distancia_bar + distancia_hosp + distancia_cole + distancia_cc +
  distancia_rest + distancia_libreria + distancia_uni + distancia_banco + dist_avenida +
  rooms_imp2 + bedrooms + bathrooms_imp2 + property_type_2 + localidad + barrio + n_pisos_numerico +
  are + parqu + balcon + remodel + sector, data = train) %>%
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
  are + parqu + balcon + remodel + sector, data = train) %>%
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
  train,
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

EN_final1_fit <- fit(res1_final, data = train)
EN_final2_fit <- fit(res2_final, data = train)

#Prediccion
augment(EN_final1_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)

augment(EN_final2_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)


# Superlerner ------------------------------------------------------------------

# Se crea una muestra train y test con referencia dek 80% del tamaÑO
set.seed(1020)
n_train <- floor(0.8 * nrow(train_full))  

# Creamos los indices
train_indices <- sample(seq_len(nrow(train_full)), size = n_train)

# Dividimos los datos
train2 <- train_full[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full[-train_indices, ]  # Datos de prueba



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

sl.lib <- c("SL.randomForest", "SL.lm",custom_ranger$names,custom_rf$names)



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
  "property_type_2", "localidad", "barrio"
)
categoricas <- c("property_type_2", "localidad", "barrio")  # Variables categóricas

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