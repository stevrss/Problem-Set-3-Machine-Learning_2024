# ----------------------------------------------------------------
# -------------- Variables adicionales espaciales ----------------
# ----------------------------------------------------------------

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024")

train <- readRDS("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024/Bases/train.rds")
test <- readRDS("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-3-Machine-Learning_2024/Bases/test.rds")

# Generacion de algunas variables simples de texto ----------------


# Bag of words y PCA (BoW) ----------------------------------------

# Instalar y cargar las librerías necesarias
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

#Crear un corpus a partir de las descripciones
corpus <- Corpus(VectorSource(train$description))

#Limpiar el texto
corpus <- tm_map(corpus, content_transformer(tolower))       # Convertir a minúsculas
corpus <- tm_map(corpus, removePunctuation)                 # Eliminar puntuación
corpus <- tm_map(corpus, removeNumbers)                     # Eliminar números
corpus <- tm_map(corpus, removeWords, stopwords("es"))      # Eliminar stopwords en español
corpus <- tm_map(corpus, stripWhitespace)                   # Eliminar espacios extra

#Crear la matriz Document-Term (DTM)
dtm <- DocumentTermMatrix(corpus)

#Reducir términos según frecuencia mínima (manejo de sparse terms)
dtm <- removeSparseTerms(dtm, 0.99)  # Mantén términos presentes en al menos 1% de los documentos

#Ponderar con TF-IDF
dtm_tfidf <- weightTfIdf(dtm)

#Convertir la matriz TF-IDF a un DataFrame
dtm_df <- as.data.frame(as.matrix(dtm_tfidf))

# Ajustar nombres de columnas para asegurarse de que sean válidos
colnames(dtm_df) <- make.names(colnames(dtm_df))

#Combinar con las demás variables del dataset
train_combined <- cbind(train, dtm_df)
#Verificar las dimensiones del dataset combinado
print(dim(train_combined))

# Componentes principales --------------------------


# Seleccionar las columnas a partir de la 60
tfidf_matrix <- as.matrix(train_combined[, 60:ncol(train_combined)])

# Aplicar PCA a estas columnas
pca_result <- prcomp(tfidf_matrix, scale. = TRUE)

# Verificar la varianza explicada por los componentes principales
summary(pca_result)

# Calcular la varianza acumulada
cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# Determinar el número de componentes necesarios para explicar al menos el 95% de la varianza
num_components <- which(cumulative_variance >= 0.95)[1]
num_components

# Reducir la matriz a los primeros componentes principales
pca_data <- as.data.frame(pca_result$x[, 1:num_components])
colnames(pca_data) <- paste0("PC", 1:num_components)

# Combinar los Componentes Principales con las Variables Originales
train_reduced <- cbind(train_combined[, 1:59], pca_data)

# Verificar las dimensiones del nuevo dataset reducido
print(dim(train_reduced))

