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
  select(-surface_total, -surface_covered, -rooms, -bedrooms, -bathrooms)

test <- Test %>% 
  select(-surface_total, -surface_covered, -rooms, -bedrooms, -bathrooms)

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
dummys <- dummy(subset(train_full, select = c(property_type_2,localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
train_full_dummys <- cbind(subset(train_full, select = -c(property_type, localidad)),dummys)

#crear dummys test
dummys <- dummy(subset(test_full, select = c(property_type_2,localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
test_full_dummys <- cbind(subset(test_full, select = -c(property_type, localidad)),dummys)

#dejar variables que comparten test y train despues de crear dummys
train_full_dummys <- train_full_dummys[c(colnames(test_full_dummys),"price")]
#Quitamos el segundo price de la train full dummys
train_full_dummys$price.1=NULL

colnames(train_full_dummys) <- make.names(colnames(train_full_dummys))
colnames(test_full_dummys) <- make.names(colnames(test_full_dummys))

train_full_dummys <- train_full_dummys[!is.na(train_full_dummys$localidad_CHAPINERO), ]

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
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0,0.5), 
                            min_child_weight = c(30,50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))

set.seed(1536)

library(caret)
XGBoost_model_1 <- caret::train(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
                          distancia_bus + distancia_super + distancia_bar + distancia_hosp + 
                          distancia_cole + distancia_cc + distancia_rest + distancia_libreria + 
                          distancia_uni + distancia_banco + dist_avenida + property_type_2 + rooms_imp2 +
                          bathrooms_imp2 + bedrooms_imp2 + surface_total_imp_mean2 + surface_covered_imp_mean2 +
                          surface_total_median2 + surface_covered_median2 + abiert + acab + acces + alcob + 
                          ampli + are + ascensor + balcon + ban + bao + baos + bbq + bogot + buen + centr +
                          cerc + cerr + chimene + closet + cocin + comedor + comercial + comunal + cuart + 
                          cuatr + cubiert + cuent + deposit + dos + edifici + espaci + estudi + excelent + 
                          exterior + garaj + gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                          integral + interior + lavanderi + lind + mader + mts + natural + parqu + parqueader + pis +
                          principal + priv + remodel + rop + sal + salon + sector + segur + servici + social + terraz + 
                          tres + ubicacion + uno + vias + vigil + visit + vist + zon + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                          PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                          PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + 
                          PC49 + PC50 + PC51 + PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + PC61 + PC62 + 
                          PC63 + PC64 + PC65 + PC66 + PC67 + PC68 + PC69 + PC70 + PC71 + property_type_2_Apartamento + 
                          property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_covered_imp_mean2^2 + rooms_imp2^2 +
                          surface_total_imp_mean2^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + localidad_SUBA + 
                          localidad_TEUSAQUILLO + localidad_USAQUEN,
                          data=train_full_dummys[-1], #excluye variable de property_id
                          method = "xgbTree",
                          trControl = fitControl,
                          metric = "MAE", # Indica que la métrica objetivo es MAE
                          tuneGrid=grid_xbgoost)        

# Obtener los mejores hiperparámetros
best_hyperparameters <- XGBoost_model_1$bestTune
print(best_hyperparameters)

# Resumen del modelo
summary(XGBoost_model_1)

# Resultado
print(XGBoost_model_1$bestTune)

# Prediccion
train_XGBoost_model_1 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model_1, newdata = train_full_dummys))  

mae_value <- mean(abs(train_XGBoost_model_1$price - train_XGBoost_model_1$price_pred))
print(mae_value)  # 94751384

# Prediccion en test
test_XGBoost_model_1 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model_1, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

# Guardar prediccion
setwd(paste0(wd,"\\Resultados\\XGboost"))
write.csv(test_XGBoost_model_1,"XGBoost_model1_nr500_maxd4_eta0.25_col0.66_min50_sub0.4.csv",row.names = F) 
#Puntaje Kaggle: 306254365.58

# 3.2  XGbosst 1 - Validacion cruzada espacial ---------------------------------
Formula_1_XG = as.formula("price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
                          distancia_bus + distancia_super + distancia_bar + distancia_hosp + 
                          distancia_cole + distancia_cc + distancia_rest + distancia_libreria + 
                          distancia_uni + distancia_banco + dist_avenida + property_type_2 + rooms_imp2 +
                          bathrooms_imp2 + bedrooms_imp2 + surface_total_imp_mean2 + surface_covered_imp_mean2 +
                          surface_total_median2 + surface_covered_median2 + abiert + acab + acces + alcob + 
                          ampli + are + ascensor + balcon + ban + bao + baos + bbq + bogot + buen + centr +
                          cerc + cerr + chimene + closet + cocin + comedor + comercial + comunal + cuart + 
                          cuatr + cubiert + cuent + deposit + dos + edifici + espaci + estudi + excelent + 
                          exterior + garaj + gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                          integral + interior + lavanderi + lind + mader + mts + natural + parqu + parqueader + pis +
                          principal + priv + remodel + rop + sal + salon + sector + segur + servici + social + terraz + 
                          tres + ubicacion + uno + vias + vigil + visit + vist + zon + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                          PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                          PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + 
                          PC49 + PC50 + PC51 + PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + PC61 + PC62 + 
                          PC63 + PC64 + PC65 + PC66 + PC67 + PC68 + PC69 + PC70 + PC71 + property_type_2_Apartamento + 
                          property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_covered_imp_mean2^2 + rooms_imp2^2 +
                          surface_total_imp_mean2^2")
