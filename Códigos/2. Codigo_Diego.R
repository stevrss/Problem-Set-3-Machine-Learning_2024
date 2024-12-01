#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Diego  -------------------------------#
#------------------------------------------------------------------------------#

wd <- "C:\\Users\\HP\\OneDrive - Universidad Nacional de Colombia\\Documentos\\Diego\\PEG\\2024-2\\Machine learning\\Repositorios\\Problem-Set-3-Machine-Learning_2024"
setwd(paste0(wd,"\\Bases"))

# 1. Cargue base de datos ------------------------------------------------------
Train <- readRDS("train.rds")
Test  <- readRDS("test.rds")

db_miss_train <- skim(Train)%>% dplyr::select(skim_variable, n_missing)
db_miss_test  <- skim(Test)%>% dplyr::select(skim_variable, n_missing)

# 1.1. Variables de interes ----------------------------------------------------
train <- Train %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

test <- Test %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

# 2. Arreglo de datos ----------------------------------------------------------

# Calcular la moda (el valor más frecuente) de la columna localidad
localidad_moda <- as.character(names(sort(table(test$localidad), decreasing = TRUE)[1]))

# Reemplazar los valores NA con la moda
test$localidad[is.na(test$localidad)] <- localidad_moda

# Se guardan las descripciones en un vector source
descriptions_train <- train$description
des_train_scource <- VectorSource(descriptions_train)
descriptions_test <- test$description
des_test_scource <- VectorSource(descriptions_test)

# Make a volatile corpus: coffee_corpus
des_corpus_train <- VCorpus(des_train_scource, readerControl = list( language = "es"))
des_corpus_test <- VCorpus(des_test_scource, readerControl = list( language = "es"))

# Función para reemplazar números por palabras
reemplazar_numeros <- function(texto) {
  palabras <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  # Reemplazar números del 0 al 10 por palabras
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

## volver a las palabras a sus raíces
stem_espanol<-  function(texto) {
  texto_stem <- stemDocument(texto, language="spanish")
  return(texto_stem)}

# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras<- union(lista_palabras,  c("vendo", "venta", "vende", "etc", "carrera", "calle", "casa", "apto", "apartamento",
                                          "ubicado","ubicada") )

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace) ## remover espacios en blanco
  corpus <- tm_map(corpus, removePunctuation)  ## remover puntuacióm
  corpus <- tm_map(corpus, content_transformer(tolower)) # todo minuscula 
  corpus <- tm_map(corpus, removeWords, c(lista_palabras)) # remover stopwords y otras que se quieran aádir
  corpus<-  tm_map(corpus, content_transformer(reemplazar_numeros))  ## incluir funciones que nosotros creamos 
  corpus<-  tm_map(corpus, content_transformer(eliminar_tildes))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(reemplazar_car_especiales))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(stem_espanol))
  corpus<-  tm_map(corpus, removeNumbers)  # remover numeros restantes
  return(corpus)}

# apliquemos nuestra función de limpieza:
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

# 3. Boosting ------------------------------------------------------------------

## Creamos una data full con las dummys 
# Crear dummys train
library(dummy)
dummys_train <- dummy(subset(train_full, select = c(property_type_2,localidad)))
dummys_train <- as.data.frame(apply(dummys_train,2,function(x){as.numeric(x)}))
train_full_dummys <- cbind(subset(train_full, select = -c(property_type, localidad)),dummys_train)

#crear dummys test
dummys_test <- dummy(subset(test_full, select = c(property_type_2,localidad)))
dummys_test <- as.data.frame(apply(dummys_test,2,function(x){as.numeric(x)}))
test_full_dummys <- cbind(subset(test_full, select = -c(property_type, localidad)),dummys_test)

#dejar variables que comparten test y train despues de crear dummys
train_full_dummys <- train_full_dummys[c(colnames(test_full_dummys),"price")]
#Quitamos el segundo price de la train full dummys
train_full_dummys$price.1=NULL

colnames(train_full_dummys) <- make.names(colnames(train_full_dummys))
colnames(test_full_dummys) <- make.names(colnames(test_full_dummys))

train_full_dummys <- train_full_dummys[!is.na(train_full_dummys$localidad_CHAPINERO), ]

# Creamos dummy de zona g o t 
train_full_dummys$zona_g <- ifelse(grepl("zona g ", train_full_dummys$description, ignore.case = TRUE), 1, 0)
train_full_dummys$zona_t <- ifelse(grepl("zona t ", train_full_dummys$description, ignore.case = TRUE), 1, 0)
train_full_dummys$zona_g_t <- ifelse(train_full_dummys$zona_g==1|train_full_dummys$zona_t==1, 1, 0)

test_full_dummys$zona_g <- ifelse(grepl("zona g ", test_full_dummys$description, ignore.case = TRUE), 1, 0)
test_full_dummys$zona_t <- ifelse(grepl("zona t ", test_full_dummys$description, ignore.case = TRUE), 1, 0)
test_full_dummys$zona_g_t <- ifelse(test_full_dummys$zona_g==1|test_full_dummys$zona_t==1, 1, 0)

# Se crea una muestra train y test 
set.seed(1536) # Para reproducibilidad

# Tamaño del conjunto de entrenamiento
n_train <- floor(0.8 * nrow(train_full_dummys))  # 80% del tamaño total

# Crear índices aleatorios para entrenamiento
train_indices <- sample(seq_len(nrow(train_full_dummys)), size = n_train)

# Dividir los datos
train2 <- train_full_dummys[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full_dummys[-train_indices, ]  # Datos de prueba

# 3.1 XGboost 1 ----------------------------------------------------------------

maeSummary <- function(data, lev = NULL, model = NULL) {
  out <- mean(abs(data$obs - data$pred)) # Calcula el MAE
  names(out) <- "MAE"                   # Nombra la métrica
  return(out)
}

fitControl <- trainControl(
  method = "cv",             # Validación cruzada
  number = 5,                # Número de particiones
  verboseIter = TRUE,        # Muestra progreso
  summaryFunction = maeSummary # Usa la función MAE personalizada
)

#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(6), 
                            eta = c(0.05,0.1,0.15), 
                            gamma = c(0.1,0.15,0.2,0.25), 
                            min_child_weight = c(10,15,20),
                            colsample_bytree = c(0.66),
                            subsample = c(0.8))

set.seed(1536)

# Se paraleliza
library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(9)  # Usar 6
registerDoParallel(cl)
#stopCluster(cl)    # Para liberar los nucleos

library(caret)
XGBoost_model_1 <- caret::train(price ~ month+ year + distancia_parque + area_parque + distancia_policia + distancia_gym +
                          distancia_bus + distancia_super + distancia_bar + distancia_hosp + 
                          distancia_cole + distancia_cc + distancia_rest + distancia_libreria + 
                          distancia_uni + distancia_banco + dist_avenida + rooms_imp2 + bedrooms + 
                          bathrooms_imp2 + surface_total_median2 + surface_covered_median2 + abiert + acab + acces + alcob + 
                          ampli + are + ascensor + balcon + ban + bao + baos + bbq + bogot + buen + centr +
                          cerc + cerr + chimene + closet + cocin + comedor + comercial + comunal + cuart + 
                          cuatr + cubiert + cuent + deposit + dos + edifici + espaci + estudi + excelent + 
                          exterior + garaj + gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                          integral + interior + lavanderi + lind + mader + mts + natural + parqu + parqueader + pis +
                          principal + priv + remodel + rop + sal + salon + sector + segur + servici + social + terraz + 
                          tres + ubicacion + uno + vias + vigil + visit + vist + zon + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + 
                          PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + PC35 + PC36 +
                          PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + PC43 + PC44 + PC45 + property_type_2_Apartamento + 
                          surface_total_median2^2 + rooms_imp2^2 +
                          surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                          localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t+bathrooms_imp2/surface_total_median2+
                          bedrooms/surface_total_median2+rooms_imp2/surface_total_median2+property_type_2_Apartamento*ascensor+
                          bathrooms_imp2/rooms_imp2+bathrooms_imp2/bedrooms+distancia_parque*property_type_2_Apartamento+distancia_cc^2+ distancia_super^2+
                          distancia_policia^2+distancia_gym^2+distancia_bus^2+distancia_hosp^2+distancia_libreria^2+distancia_uni^2+dist_avenida^2+parqueader*property_type_2_Apartamento
                          ,data=train2[-1], #excluye variable de property_id
                          method = "xgbTree",
                          trControl = fitControl,
                          metric = "MAE", # Indica que la métrica objetivo es MAE
                          tuneGrid=grid_xbgoost)        

# Obtener los mejores hiperparámetros
best_hyperparameters <- XGBoost_model_1$bestTune
print(best_hyperparameters)

#nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 500       6       0.05  0.1      0.66               25           0.8
# 500       6       0.1   0.1      0.66               15           0.8
# 500       6       0.2   0.1      0.66               10           0.8
# 500       6       0.15  0.1      0.66               20           0.8
# 500       6       0.15  0.2      0.66               10           0.8

# Resumen del modelo
summary(XGBoost_model_1)

# Resultado
print(XGBoost_model_1$bestTune)

# Prediccion dentro de muestra 
train_XGBoost_model_1 <- train2 %>% 
  mutate(price_pred = predict(XGBoost_model_1, newdata = train2))  

mae_value_train <- mean(abs(train_XGBoost_model_1$price - train_XGBoost_model_1$price_pred))
print(mae_value_train)  # 89613366 vs 70664313 vs 70236645 vs 60465280 vs 48780882 VF

# Prediccion fuera de muestra 
test_XGBoost_model_1 <- test2 %>% 
  mutate(price_pred = predict(XGBoost_model_1, newdata = test2))  

mae_value_test <- mean(abs(test_XGBoost_model_1$price - test_XGBoost_model_1$price_pred))
print(mae_value_test)  # 117231096 vs 109446775 vs 109021105 vs 106177795 vs 106437327 vs 106804677 VF

# Importancia de variables 
p_load(DiagrammeR)
tree_plot <- xgb.plot.tree(model = XGBoost_model_1$finalModel,
                           trees = 1, plot_width = 1000, plot_height = 500)
tree_plot

# Importancia de iteraciones
importance <- varImp(XGBoost_model_1, scale = FALSE)
print(importance)

# Prediccion en test
test_XGBoost_model_1 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model_1, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\XGboost"))
write.csv(test_XGBoost_model_1,"XGBoost_model1_nr500_maxd6_eta0.15_g0.2_col0.66_min10_sub0.8.csv",row.names = F) 
#Puntaje Kaggle: 230537671.28 vs 233586592.35 vs 227872992.63 vs 228451088.75 vs 

# 3.2  XGbosst 1 - Validacion cruzada espacial ---------------------------------

# Configuaracion del modelo 
xgboost_spec <- boost_tree(
  trees = tune(),            # Número de árboles
  tree_depth = tune(),       # Profundidad del árbol
  learn_rate = tune(),       # Tasa de aprendizaje
  loss_reduction = tune(),   # Gamma (reducción mínima de pérdida)
  sample_size = tune(),      # Submuestreo de observaciones
  mtry = tune(),             # Número de predictores seleccionados
  min_n = tune()             # Mínimo de observaciones por hoja
) %>%
  set_engine("xgboost") %>%  # Especificar el motor
  set_mode("regression")     # Modo de regresión

# Definir un grid para los hiperparámetros
xgb_grid <- expand.grid(
  trees = c(500),
  tree_depth = c(6),
  learn_rate = c(0.1),
  loss_reduction = c(0.1),
  sample_size = c(0.8),
  mtry = c(140),
  min_n = c(15)
)

#xgb_grid <- expand.grid(
#  trees = 500,
#  tree_depth = c(4, 6),
#  learn_rate = c(0.05, 0.1, 0.15, 0.2),
#  loss_reduction = c(0.1, 0.2, 0.3),
#  sample_size = c(0.6, 0.8),
#  mtry = 136,
#  min_n = c(10, 15, 20, 25)
# )


# Primera receta
rec_1 <- recipe(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
                  distancia_bus + distancia_super + distancia_bar + distancia_hosp + 
                  distancia_cole + distancia_cc + distancia_rest + distancia_libreria + 
                  distancia_uni + distancia_banco + dist_avenida + rooms_imp2 + bedrooms + 
                  bathrooms_imp2 + surface_total_median2 + surface_covered_median2 + abiert + acab + acces + alcob + 
                  ampli + are + ascensor + balcon + ban + bao + baos + bbq + bogot + buen + centr +
                  cerc + cerr + chimene + closet + cocin + comedor + comercial + comunal + cuart + 
                  cuatr + cubiert + cuent + deposit + dos + edifici + espaci + estudi + excelent + 
                  exterior + garaj + gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                  integral + interior + lavanderi + lind + mader + mts + natural + parqu + parqueader + pis +
                  principal + priv + remodel + rop + sal + salon + sector + segur + servici + social + terraz + 
                  tres + ubicacion + uno + vias + vigil + visit + vist + zon + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                  PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + property_type_2_Apartamento +
                  localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                  localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                  localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t + barrio
                , data = train2[-1]) %>%
  step_interact(terms = ~ bathrooms_imp2:surface_total_median2+
                  bedrooms:surface_total_median2+rooms_imp2:surface_total_median2+property_type_2_Apartamento:ascensor+
                  bathrooms_imp2:rooms_imp2+bathrooms_imp2:bedrooms) %>% 
  step_poly(surface_total_median2, surface_covered_median2,rooms_imp2,bedrooms,distancia_policia,
            distancia_libreria, distancia_gym , distancia_bus , distancia_super , distancia_bar , distancia_hosp , 
              distancia_cole , distancia_cc , distancia_rest , distancia_libreria , 
              distancia_uni , distancia_banco , dist_avenida, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Iniciar un flujo de trabajo utilizando 'workflow()'
workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo xgboost
  add_model(xgboost_spec)

# Validacion cruzada espacial 
# definimos nuestra variable como sf
train_sf <- st_as_sf(
  train2[-1],
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

# Proceso de validacion 
set.seed(1536)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

# Se paraleliza
#library(doParallel)
#num_cores <- parallel::detectCores()
#print(num_cores)
#cl <- makeCluster(6)  # Usar 6
#registerDoParallel(cl)
#stopCluster(cl)    # Para liberar los nucleos

# Vamos a encontrar los mejores hiperparámetros 
set.seed(1536)

tune_res1 <- tune_grid(
  workflow_1,               # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = xgb_grid,          # Grilla de valores
  metrics = metric_set(mae) # métrica
)

# Ver los resultados 
collect_metrics(tune_res1)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res1 <- select_best(tune_res1, metric = "mae")
best_tune_res1

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final <- finalize_workflow(workflow_1, best_tune_res1)

# Ajustar el modelo  utilizando los datos de entrenamiento
EN_final1_fit <- fit(res1_final, data = train2[-1])

# Ver resultados en test2 
augment(EN_final1_fit, new_data = test2) %>%
  mae(truth = price, estimate = .pred)

# Ver resultados en test original
test_XGBoost_model_1_vce = augment(EN_final1_fit, new_data = test_full_dummys)

test_XGBoost_model_1 <- test_XGBoost_model_1_vce %>%
  mutate(price = .pred) %>% 
  select(property_id,price) 

# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\XGboost"))
write.csv(test_XGBoost_model_1,"XGBoost_model_espacial_nr500_maxd6_eta0.15_g0.2_col0.66_min10_sub0.8.csv",row.names = F) 
#Puntaje Kaggle: 230537671.28 vs 233586592.35 vs 227872992.63 vs 228451088.75 vs 
