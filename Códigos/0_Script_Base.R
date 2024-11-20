#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:
# 1: Cambiar el directorio entre cada uno de los colaboradores del proyecto
# 2: Correr cada uno de los scripts utilizados en la resolucion del Problem set 2.

# 0. Se borra la memoria y se cargan los paquetes ------------------------------
rm(list = ls())   # Borra la memoria

# Se cargan los paquetes de interes
library(pacman)
p_load(
  # Funciones básicas para manejo y análisis de datos
  arrow,         # Lectura y escritura de datos en formatos eficientes como Apache Parquet y Feather
  sfheaders,     # Manipulación eficiente de objetos espaciales 'sf' en R
  VIM,           # Manejo y visualización de valores faltantes en datasets
  tidyverse,     # Colección de paquetes para manipulación de datos (dplyr, ggplot2, tidyr, etc.)
  janitor,       # Limpieza y organización de datos (e.g., nombres de columnas, tablas cruzadas)
  here,          # Gestión de rutas relativas para proyectos en R
  rio,           # Importación y exportación sencilla de archivos (CSV, Excel, SPSS, etc.)
  stringi,       # Manipulación avanzada de cadenas de texto
  readr,         # Lectura rápida de archivos de texto y CSV
  skimr,         # Resúmenes estadísticos rápidos y detallados de datasets
  # Paquetes para manejo de datos espaciales
  sf,            # Manejo y análisis de datos espaciales en formato simple (e.g., shapefiles, GeoJSON)
  leaflet,       # Visualización interactiva de mapas en R
  spatialsample, # Creación de muestras espaciales para análisis y modelado
  osmdata,       # Obtención de datos geoespaciales de OpenStreetMap (OSM)
  tmaptools,     # Herramientas de apoyo para crear mapas con 'tmap'
  caret,         # Conjunto de herramientas para entrenamiento y evaluación de modelos predictivos
  # Paquetes de aprendizaje automático (Machine Learning)
  tidymodels,    # Framework para construir y evaluar modelos de machine learning en un entorno 'tidy'
  xgboost,       # Algoritmo de gradient boosting optimizado para tareas de clasificación y regresión
  gbm,           # Modelado de boosting basado en árboles de decisión (Gradient Boosting Machines)
  SuperLearner,   # Meta-modelo para combinar múltiples algoritmos de machine learning y mejorar la predicción
  #Redes neuronales
  nnet, # redes neuronales de una sola capa
  Brulee # Redes neuronales mas complejas
)


# 1. Definicion del directorio -------------------------------------------------

ifelse(grepl("Usuario", getwd()), # Diego
       wd <- "C:\\Users\\Usuario\\OneDrive - Universidad de los andes\\Documentos\\GitHub\\Problem-Set-3-Machine-Learning_2024",
       ifelse(grepl("Usuario", getwd()), # Julieth1
              wd <- "C:\\Users\\Usuario\\OneDrive - Universidad de los andes\\Escritorio\\Taller 2 Big data",
              ifelse(grepl("hncar", getwd()),
                     wd <- "C:\\Users\\hncar\\Documents\\GitHub\\Problem-Set-2_Machine-Learning_2024",
                     ifelse(grepl("C:\\Users\\User", getwd()),  # Henry
                            wd <- "C:\\Users\\User\\OneDrive - Universidad de los Andes\\Big Data y Machine Learning\\Problem_set_1\\Problem_set_1",
                            ifelse(grepl("\\\\Users\\\\aleja\\\\", getwd()), 
                                   wd <- "Directorio",  # Jorge
                                   ifelse(grepl("Steven Ramirez", getwd()), 
                                          wd <- "C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024",

