#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:
# 1: Cambiar el directorio entre cada uno de los colaboradores del proyecto 3

# 0. Se borra la memoria y se cargan los paquetes ------------------------------
rm(list = ls())   # Borra la memoria

# Se cargan los paquetes de interes
library(pacman)
p_load(
  # Funciones basicas para manejo y analisis de datos
  arrow,         # Lectura y escritura de datos en formatos eficientes como Apache Parquet y Feather
  sfheaders,     # Manipulacion eficiente de objetos espaciales 'sf' en R
  VIM,           # Manejo y visualizacion de valores faltantes en datasets
  tidyverse,     # Coleccion de paquetes para manipulacion de datos (dplyr, ggplot2, tidyr, etc.)
  janitor,       # Limpieza y organizacion de datos (e.g., nombres de columnas, tablas cruzadas)
  here,          # Gestion de rutas relativas para proyectos en R
  rio,           # Importacion y exportacion sencilla de archivos (CSV, Excel, SPSS, etc.)
  stringi,       # Manipulacion avanzada de cadenas de texto
  readr,         # Lectura rapida de archivos de texto y CSV
  skimr,         # Resumenes estadadisticos rapidos y detallados de datasets
  stargazer,
  
  # Paquetes para manejo de datos espaciales
  sf,            # Manejo y analisis de datos espaciales en formato simple (e.g., shapefiles, GeoJSON)
  leaflet,       # Visualizacion interactiva de mapas en R
  spatialsample, # Creacion de muestras espaciales para analisis y modelado
  osmdata,       # Obtencion de datos geoespaciales de OpenStreetMap (OSM)
  tmaptools,     # Herramientas de apoyo para crear mapas con 'tmap'
  caret,         # Conjunto de herramientas para entrenamiento y evaluacion de modelos predictivos
  
  # Paquetes de aprendizaje automatico (Machine Learning)
  tidymodels,    # Framework para construir y evaluar modelos de machine learning en un entorno 'tidy'
  xgboost,       # Algoritmo de gradient boosting optimizado para tareas de clasificacion y regresion
  gbm,           # Modelado de boosting basado en arboles de decision (Gradient Boosting Machines)
  SuperLearner,   # Meta-modelo para combinar múltiples algoritmos de machine learning y mejorar la predicción
  
  #Redes neuronales
  nnet,   # redes neuronales de una sola capa
  Brulee, # Redes neuronales mas complejas

  
  skimr,     # datos de resumen
  visdat,    # visualizacion de datos faltantes
  corrplot,  # graficos de correlacion
  stargazer, # tablas/salida a TEX.
  modeest,   #Moda
  ggplot2,    #Graficos
  plotly
)


# 1. Definicion del directorio -------------------------------------------------

ifelse(grepl("Usuario", getwd()), # Julieth1
       wd <- "C:\\Users\\Usuario\\OneDrive - Universidad de los andes\\Documentos\\GitHub\\Problem-Set-3-Machine-Learning_2024",
       ifelse(grepl("HP", getwd()), # Diego
              wd <- "C:\\Users\\HP\\OneDrive - Universidad Nacional de Colombia\\Documentos\\Diego\\PEG\\2024-2\\Machine learning\\Repositorios\\Problem-Set-3-Machine-Learning_2024",
              ifelse(grepl("hncar", getwd()),
                     wd <- "C:\\Users\\hncar\\Documents\\GitHub\\Problem-Set-2_Machine-Learning_2024",
                     ifelse(grepl("C:\\Users\\User", getwd()),  # Henry
                            wd <- "C:\\Users\\User\\OneDrive - Universidad de los Andes\\Big Data y Machine Learning\\Problem_set_1\\Problem_set_1",
                            ifelse(grepl("\\\\Users\\\\aleja\\\\", getwd()), 
                                   wd <- "Directorio",  # Jorge
                                   ifelse(grepl("Steven Ramirez", getwd()), 
                                          wd <- "C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024",
                                          "otro_directorio"))))))

