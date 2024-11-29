# -----------------------------
# Redes Neuronales de Jorge
# -----------------------------

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024")

Train <- readRDS("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024/Bases/train.rds")
Test <- readRDS("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024/Bases/test.rds")

# Arreglo --------------------------------------

db_miss_train <- skim(Train)%>% dplyr::select(skim_variable, n_missing)
db_miss_test  <- skim(Test)%>% dplyr::select(skim_variable, n_missing)

# 1.1. Variables de interes ----------------------------------------------------
train <- Train %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

test <- Test %>% 
  select(-surface_total, -surface_covered, -rooms, -bathrooms)

# 2. Arreglo de datos ----------------------------------------------------------

# Calcular la moda (el valor m?s frecuente) de la columna localidad
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

# Redes neuronales -----------------------------------
# No convolusionales
# 1 Capa ---------------------------------------------

p_load("ISLR2","keras3","keras")

# Convertir variables categóricas a algo entendible para las redes ------------

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

# Tama?o del conjunto de entrenamiento
n_train <- floor(0.8 * nrow(train_full_dummys))  # 80% del tama?o total

# Crear ?ndices aleatorios para entrenamiento
train_indices <- sample(seq_len(nrow(train_full_dummys)), size = n_train)

# Dividir los datos
train2 <- train_full_dummys[train_indices, ]  # Datos de entrenamiento
test2 <-  train_full_dummys[-train_indices, ]  # Datos de prueba


# -----------------------------------------------------------------------------
install.packages("keras")
library(keras)

# Instalar TensorFlow
install_keras()

library(reticulate)
reticulate::py_config()
# Crear y usar un nuevo entorno virtual
reticulate::virtualenv_create(envname = "r-reticulate")
reticulate::use_virtualenv("r-reticulate", required = TRUE)

# Reinstalar TensorFlow en el nuevo entorno
install_keras()

y <- train2$price
x <- scale(model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                          PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                          PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                          property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                          surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                          localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = train2))

modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

x <- scale(model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                          PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                          PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                          property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                          surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                          localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = train2))
x <- model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                    PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                    PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                    property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                    surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                    localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                    localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = train2) %>% scale()

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error")
)

# Preparar los datos de prueba (test2)

x_test <- scale(model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                                    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                                    PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                                    PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                                    property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                                    surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                                    localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                                    localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = test2))
y_test <- test2$price

# Ajustar el modelo y validar en test2
history <- modnn %>% fit(
  x, y, epochs = 600, batch_size = 32,
  validation_data = list(x_test, y_test) # Usar test2 como validation_data
)

# Evaluar en test2
evaluation <- modnn %>% evaluate(x_test, y_test)
print(paste("Loss (MSE):", evaluation["loss"]))
print(paste("Mean Absolute Error (MAE):", evaluation["mean_absolute_error"]))

# Predicciones y métricas manuales
npred_test2 <- predict(modnn, x_test)

mse_test2 <- mean((npred_test2 - y_test)^2)
mae_test2 <- mean(abs(npred_test2 - y_test))

print(paste("MSE en test2:", mse_test2))
print(paste("MAE en test2:", mae_test2))

# Realizar modelo con mas de una capa --------------------


modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 50, activation = "softmax", input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)


x <- scale(model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                          PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                          PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                          property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                          surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                          localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = train2))
x <- model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                    PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                    PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                    property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                    surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                    localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                    localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = train2) %>% scale()

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error")
)

# Preparar los datos de prueba (test2)

x_test <- scale(model.matrix(price ~ distancia_parque + area_parque + distancia_policia + distancia_gym +
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
                               PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
                               PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + 
                               PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + property_type_2_Apartamento + 
                               property_type_2_Casa + n_pisos_numerico + piso_numerico + surface_total_median2^2 + rooms_imp2^2 +
                               surface_covered_median2^2+bedrooms^2+localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                               localidad_ENGATIVA + localidad_PUENTE.ARANDA + localidad_SANTA.FE + localidad_SUBA + 
                               localidad_TEUSAQUILLO + localidad_USAQUEN+estrato_imp +zona_g_t - 1, data = test2))
y_test <- test2$price

# Ajustar el modelo y validar en test2
history <- modnn %>% fit(
  x, y, epochs = 100, batch_size = 32,
  validation_data = list(x_test, y_test) # Usar test2 como validation_data
)

# Evaluar en test2
evaluation <- modnn %>% evaluate(x_test, y_test)
print(paste("Loss (MSE):", evaluation["loss"]))
print(paste("Mean Absolute Error (MAE):", evaluation["mean_absolute_error"]))

# Predicciones y métricas manuales
npred_test2 <- predict(modnn, x_test)

mse_test2 <- mean((npred_test2 - y_test)^2)
mae_test2 <- mean(abs(npred_test2 - y_test))

print(paste("MSE en test2:", mse_test2))
print(paste("MAE en test2:", mae_test2))


#######################################################
#######################################################
#######################################################

# One-Hot Encoding usando model.matrix
encoded <- model.matrix(~ localidad - 1, data = train_full)

train$city <- as.factor(train$city)
train$barrio <- as.factor(train$barrio)

set.seed(123)
train_index <- sample(1:nrow(train), size = 0.8 * nrow(train))
train_set <- train[train_index, ]
test_set <- train[-train_index, ]

# Separar variables predictoras y objetivo
x_train <- as.matrix(train_set %>% select(-price))
y_train <- train_set$price
x_test <- as.matrix(test_set %>% select(-price))
y_test <- test_set$price



# Entrenando red

modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

x <- scale(model.matrix(price ~ . - 1, data = train))
x <- model.matrix(price ~ . - 1, data = train) %>% scale()

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error")
)

history <- modnn %>% fit(
  x[-testid, ], y[-testid], epochs = 600, batch_size = 32,
  validation_data = list(x[testid, ], y[testid])
)

npred <- predict(modnn, x[testid, ])

