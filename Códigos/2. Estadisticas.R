#------------------------------------------------------------------------------#
#---------------------- Estadisticas - PROBLEM SET 3 -------------------------#
#------------------------------------------------------------------------------#

# Cargue base de datos ---------------------------------------------------------
setwd(paste0(wd, "\\Bases")) #Directorio datos
train <- readRDS("train.rds")
test  <- readRDS("test.rds")


# Convertir bases train y test en datos espaciales -----------------------------
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) # Homogeneizar proyección
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)


#Estadisticas de las bases de datos --------------------------------------------

  #.  Tabla de estadisticas
    
      train <- as.data.frame(train)
      test <- as.data.frame(test)
      
      stargazer(train)
      stargazer(train,type="text")

  #.  Correlacion de las variables
      
      setwd(paste0(wd, "/Graficas"))
      png("graf_corr_completa.png", width = 1400, height = 1200) # Ajustar tamaño de la imagen
      
      # Selección y renombramiento de todas las variables
      corr <- train %>%
        dplyr::select(
          # Variables base
          price,
          bedrooms,
          rooms_imp,
          bathrooms_imp,
          surface_total_imp_mean,
          surface_covered_imp_mean,
          dist_avenida,
          n_pisos_numerico,
          piso_numerico,
          estrato_imp,
          # Variables espaciales y de texto
          distancia_parque,
          area_parque,
          distancia_policia,
          distancia_gym,
          distancia_bus,
          distancia_super,
          distancia_bar,
          distancia_hosp,
          distancia_cole,
          distancia_cc,
          distancia_rest,
          distancia_libreria,
          distancia_uni,
          distancia_banco
        ) %>%
        dplyr::rename(
          # Renombrar variables base
          Precio = price,
          Dormitorios = bedrooms,
          Habitaciones_Imp = rooms_imp,
          Baños_Imp = bathrooms_imp,
          Superficie_Total_Imp = surface_total_imp_mean,
          Superficie_Cubierta_Imp = surface_covered_imp_mean,
          Distancia_Avenida = dist_avenida,
          Numero_Pisos_casa = n_pisos_numerico,
          Piso_Apto = piso_numerico,
          Estrato_Imp = estrato_imp,
          # Renombrar variables espaciales
          Distancia_Parque = distancia_parque,
          Area_Parque = area_parque,
          Distancia_Policia = distancia_policia,
          Distancia_Gimnasio = distancia_gym,
          Distancia_Transporte = distancia_bus,
          Distancia_Supermercado = distancia_super,
          Distancia_Bar = distancia_bar,
          Distancia_Hospital = distancia_hosp,
          Distancia_Colegio = distancia_cole,
          Distancia_CC = distancia_cc,
          Distancia_Restaurante = distancia_rest,
          Distancia_Libreria = distancia_libreria,
          Distancia_Universidad = distancia_uni,
          Distancia_Banco = distancia_banco
        )
      
      # Calcular la matriz de correlación
      datos_numericos <- corr[sapply(corr, is.numeric)]
      matriz_correlacion <- cor(datos_numericos, use = "pairwise.complete.obs")
      
      # Imprimir matriz de correlación
      print(matriz_correlacion)
      
      # Graficar la matriz de correlación
      corrplot(
        matriz_correlacion,
        method = "color",
        type = "upper",
        tl.cex = 1.3, # Tamaño de las etiquetas
        tl.col = "black",
        addCoef.col = "black", # Agregar coeficientes en el gráfico
        number.cex = 1.1, # Tamaño de los números
        title = "",
        mar = c(0, 0, 4, 0) # Margen para el título
      )
      
      # Calcular significancia de las correlaciones
      cor_signif <- rcorr(as.matrix(datos_numericos))
      print(cor_signif)
      
      # Guardar el archivo
      dev.off()
      
      
      
      
  #. Distribucion de las propiedades por localidad.
    
    ##. Base train
    distribucion_absoluta <- table(train$localidad)
    distribucion_porcentual <- prop.table(distribucion_absoluta) * 100
    distribucion_porcentual_df <- as.data.frame(distribucion_porcentual)
    colnames(distribucion_porcentual_df) <- c("Localidad", "Porcentaje")
    distribucion_porcentual_df <- distribucion_porcentual_df[order(-distribucion_porcentual_df$Porcentaje), ]
    print(distribucion_porcentual_df)
    
    ##. Base test
    distribucion_absoluta <- table(test$localidad)
    distribucion_porcentual <- prop.table(distribucion_absoluta)*100
    distribucion_porcentual_df <- as.data.frame(distribucion_porcentual)
    colnames(distribucion_porcentual_df) <- c("Localidad", "Porcentaje")
    distribucion_porcentual_df <- distribucion_porcentual_df[order(-distribucion_porcentual_df$Porcentaje), ]
    print(distribucion_porcentual_df)
    
  #. Mpapa de distribucion de las propiedades por localidad.
    
    ##.  Base train
      g1 <- ggplot() +
        geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
        geom_sf(data = sf_train %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 1.2, alpha = 0.6) +
        geom_sf(data = sf_train %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 1.2, alpha = 0.8) +
        scale_fill_manual(
          name = "Localidad",
          values = c(
            "CHAPINERO" = "#ADD8E6", # Azul claro para Chapinero
            .default = "#D3D3D3"     # Gris claro elegante para otras localidades
          )) + 
        scale_color_manual(
          name = "Tipo de Propiedad",
          values = c(Apartamento = "#1E90FF", Casa = "#d73027") # Azul y rojo
        ) +   labs(
          title = "Entrenamiento",
          x = "Longitud", y = "Latitud"
        ) +
        theme_minimal()
      g1

    ##.  Base test
      g2 <- ggplot() +
        geom_sf(data = local, aes(fill = Nombre.de.la.localidad), color = "black", lwd = 0.3) +
        geom_sf(data = sf_test %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 16, size = 1.2, alpha = 0.6) +
        geom_sf(data = sf_test %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 16, size = 1.2, alpha = 0.8) +
        scale_fill_manual(
          name = "Localidad",
          values = c(
            "CHAPINERO" = "#ADD8E6", # Azul claro para Chapinero
            .default = "#D3D3D3"     # Gris claro elegante para otras localidades
          )
        ) +
        scale_color_manual(
          name = "Tipo de Propiedad",
          values = c(Apartamento = "#1E90FF", Casa = "#d73027") # Azul y rojo
        ) +
        labs(
          title = "Testeo",
          x = "Longitud", y = "Latitud"
        ) +
        theme_minimal()
      g2
      
      # Combinar los mapas en una sola imagen
      combined_plot <- g1 | g2  
      print(combined_plot)
      
      # Guardar el gráfico combinado
      setwd(paste0(wd,"/Graficas")) # Establecer el directorio de salida para gráficos
      ggsave("map_combined.png", plot = combined_plot)
  
  #.   Distribucon del precio de las propiedad 
      
      # Forma 1
      p1 <- ggplot(train, aes(x = price / 1000000, y = ..count../sum(..count..)*100)) +
        geom_histogram(fill = "#87CEFA",color = "white", alpha = 0.7, bins = 30) + # Barras con color azul claro (HEX: #ADD8E6) y mayor transparencia
        labs(x = "Precio (Millones)", y = "Porcentaje") + # Etiquetas de los ejes
        theme_bw() + # Tema básico en blanco y negro
        scale_y_continuous(labels = scales::percent_format(scale = 1)) # Escalar eje Y como porcentaje
      ggplotly(p) # Hacer el gráfico interactivo con ggplotly
      setwd(paste0(wd,"/Graficas")) # Establecer el directorio de salida para gráficos
      ggsave("Dis_precios.png", plot = p1)
      
      # Forma 2
      p2 <- ggplot(train, aes(x = price / 1000000)) +
        geom_histogram(fill = "#87CEFA",color = "white", alpha = 0.4) + # Barras con color azul oscuro y transparencia
        labs(x = "Precio (Millones)", y = "Cantidad") + # Etiquetas de los ejes
        theme_bw() # Tema básico en blanco y negro
      ggplotly(p) # Hacer el gráfico interactivo con ggplotly
      setwd(paste0(wd,"/Graficas")) # Establecer el directorio de salida para gráficos
      ggsave("Dis_precios2.png", plot = p2)
      
  # Distribucon del precio por tipo de propiedad-----------------------------------
      media_apto <- mean((train$price[train$property_type == "Apartamento"]) / 1000000, na.rm = TRUE)
      media_casa <- mean((train$price[train$property_type == "Casa"]) / 1000000, na.rm = TRUE)
      
      tipo <- ggplot(train, aes(x = (price / 1000000), fill = property_type)) +
        geom_histogram(color = "white", alpha = 0.8, position = "identity", binwidth = 50) + # Histogramas
        geom_vline(aes(xintercept = media_apto), color = "#A4D3EE", linetype = "dashed", size = 1) + # Línea media apartamento
        geom_vline(aes(xintercept = media_casa), color = "#C4C4C4", linetype = "dashed", size = 1) + # Línea media casa
        labs(
          title = "", # Título del gráfico
          subtitle = "", # Subtítulo del gráfico
          x = "Precio de Viviendas (Millones)", # Etiqueta del eje X
          y = "Frecuencia" # Etiqueta del eje Y
        ) +
        scale_fill_manual(values = c("Apartamento" = "#A4D3EE", "Casa" = "#C4C4C4"), guide = "none") + # Colores por tipo
        scale_x_continuous(
          breaks = seq(0, max(train$price / 1000000, na.rm = TRUE), by = 100), # Escala con intervalos cada 100 millones
          labels = scales::comma_format(scale = 1) # Formato con comas para mayor legibilidad
        ) +
        facet_wrap(~property_type) + # Dividir por tipo de propiedad
        theme_minimal(base_size = 8) + # Tema minimalista
        theme(
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"), # Tamaño y estilo del título principal
          plot.subtitle = element_text(size = 12, hjust = 0.5), # Tamaño y posición del subtítulo
          axis.title.x = element_text(size = 8), # Tamaño del título del eje X
          axis.title.y = element_text(size = 8), # Tamaño del título del eje Y
          axis.text.x = element_text(size = 8, angle = 90, hjust = 1), # Tamaño y orientación de las etiquetas del eje X
          axis.text.y = element_text(size = 8) # Tamaño de las etiquetas del eje Y
        )
      # Mostrar el gráfico
      tipo
      # Guardar el gráfico en un archivo
      setwd(paste0(wd, "/Graficas")) # Establecer el directorio de salida para gráficos
      ggsave("Tipo_propiedad.png", plot = tipo)
      
      
      
    
# Distribucion del tipo de propiedad por localidad-----------------------------------
      
      # Agrupamos por localidad y tipo de propiedad para calcular el porcentaje
      data_porcentajes <- train %>%
        group_by(localidad, property_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(localidad) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Creamos la gráfica de barras apiladas con etiquetas de porcentaje
      g1 <- ggplot(data_porcentajes, aes(x = localidad, y = percentage, fill = property_type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(
          aes(label = sprintf("%.1f%%", percentage)),
          position = position_stack(vjust = 0.5), # Posiciona las etiquetas dentro del segmento
          angle = 90, # Etiquetas verticales
          size = 3, # Tamaño de la etiqueta
          color = "black" # Etiquetas en blanco para contraste
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        scale_fill_manual(
          values = c("Apartamento" = "#87CEFA", "Casa" = "#E0E0E0"),  # Colores azul y gris
          labels = c("Apartamento", "Casa")
        ) +
        labs(
          title = "",
          x = "Localidad",
          y = "Porcentaje",
          fill = "Tipo de Propiedad"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),  # Labels en 90 grados
          legend.position = "top"  # Ubicación de la leyenda
        )
      
      # Guardar el gráfico
      setwd(paste0(wd, "/Graficas")) # Establecer el directorio de salida para gráficos
      g1
      ggsave("Tipo_propiedad_local.png", plot = g1)
      
      
      
    