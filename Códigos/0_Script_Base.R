#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:

# 1: Prepara el entorno y argar los paquetes necesarios para el desarrollo del taller
# 2: Cambiar el directorio entre cada uno de los colaboradores del proyecto 3


#------------------------------------------------------------------------------#
# 1: Prepara el entorno y argar los paquetes necesarios para el desarrollo del taller
#------------------------------------------------------------------------------#


# Limpia toda la memoria del entorno de trabajo para evitar conflictos con variables previas.
rm(list = ls())

# Se cargan los paquetes necesarios usando 'pacman', que facilita la gestión de paquetes.
library(pacman)

# 'p_load' instala (si no están ya instalados) y carga los paquetes necesarios.
p_load(
  # ------------------------- Manejo y análisis de datos -------------------------
  arrow,         # Lectura y escritura de datos en formatos optimizados (Parquet, Feather).
  sfheaders,     # Manipulación eficiente de objetos espaciales tipo 'sf' (data frames con geometrías espaciales).
  VIM,           # Manejo y visualización de valores faltantes en datasets.
  tidyverse,     # Conjunto de paquetes para manipulación de datos (e.g., dplyr, ggplot2, tidyr).
  janitor,       # Limpieza de datos (normaliza nombres de columnas, crea tablas resumidas, etc.).
  here,          # Facilita la gestión de rutas relativas para proyectos reproducibles.
  rio,           # Simplifica la importación/exportación de diversos formatos de datos (CSV, Excel, SPSS, etc.).
  stringi,       # Ofrece herramientas avanzadas para manipulación de cadenas de texto.
  readr,         # Lectura eficiente de archivos planos como CSV y TSV.
  skimr,         # Proporciona resúmenes detallados y rápidos de los datos (estadísticas descriptivas).
  stargazer,     # Produce tablas de resultados en formato LaTeX, texto y HTML para informes reproducibles.
  
  # ---------------------- Manejo de datos espaciales ----------------------
  sf,            # Manejo y análisis de datos espaciales en formato 'sf' (e.g., shapefiles, GeoJSON).
  leaflet,       # Crea mapas interactivos para visualización de datos espaciales.
  spatialsample, # Muestras espaciales para modelado y análisis.
  osmdata,       # Obtención de datos geográficos de OpenStreetMap (como edificios, carreteras).
  tmaptools,     # Herramientas complementarias para crear mapas con el paquete 'tmap'.
  caret,         # Framework para entrenamiento y validación de modelos predictivos.
  
  # -------------------- Aprendizaje automático (Machine Learning) -------------------
  tidymodels,    # Framework para construir modelos predictivos en un entorno "tidy".
  xgboost,       # Algoritmo avanzado para clasificación y regresión basado en gradient boosting.
  gbm,           # Máquinas de boosting (gradient boosting) basadas en árboles de decisión.
  SuperLearner,  # Combina múltiples algoritmos de aprendizaje automático para mejorar la predicción.
  tm,
  
  # --------------------------- Redes neuronales --------------------------------
  nnet,          # Redes neuronales simples de una capa (útil para regresión/clasificación básica).
  Brulee,        # Implementación moderna para redes neuronales más complejas.
  
  # ---------------------- Herramientas de visualización ----------------------
  skimr,         # Resúmenes de datos detallados.
  visdat,        # Visualización de valores faltantes y estructura de datos.
  corrplot,      # Gráficos para representar matrices de correlación.
  ggplot2,       # Creación de gráficos personalizables y profesionales.
  plotly,        # Gráficos interactivos basados en 'ggplot2'.
  
  # ------------------------- Estadística y análisis básico ---------------------
  modeest,       # Cálculo de la moda (estadística descriptiva).
  stargazer      # Tablas en formato LaTeX, HTML y texto para reportar resultados estadísticos.
)


#------------------------------------------------------------------------------#
# 2: Definir directorios
#------------------------------------------------------------------------------#

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

