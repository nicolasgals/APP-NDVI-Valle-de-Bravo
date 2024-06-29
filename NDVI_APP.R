# Análisis de imágenes raster: VDB

#------------------------------------------------------------------------------#
# Proyecto: VDB

# Fecha de creación:          28 de junio de 2024

# Última actualización:       28 de junio de 2024
#------------------------------------------------------------------------------#

# Cargar librerías y preparación inicial----------------------------------------

# ---- Limpiar espacio de trabajo 
rm(list = ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Cargar librerias

library(shiny)
library(shinythemes)
library(sf)        
library(terra)      
library(tidyverse)  
library(tidyterra) 
library(cptcity)
library(leaflet)
library(leafsync)   
library(viridis)
library(leaflet.minicharts)
library(manipulateWidget)

# Cargar bases y datos espaciales-----------------------------------------------

#------------------------------------------------------------------------------#

# Área de estudio---------------------------------------------------------------
study_area <- st_read('vdb_area_interes.kml') %>%
  st_zm(drop = TRUE) %>%
  st_transform(crs = 4326)

study_area_bbox <- st_bbox(study_area) %>%
  as.vector()

# Datos raster Sentinel2 L2A----------------------------------------------------

  # ---- 2019-05-12-00

sen2_l2a_2019_05_12_singleband_files <- list.files(path = "Sentinel2-L2A/2019-05-12-00", pattern = "\\(Raw).tiff$", full.names = TRUE)

sen2_l2a_2019_05_12_bands <- lapply(sen2_l2a_2019_05_12_singleband_files, rast)

sen2_l2a_2019_05_12_composite <- rast(sen2_l2a_2019_05_12_bands)

names(sen2_l2a_2019_05_12_composite) <- c("B3", "B4", "B8", "B12")

setMinMax(sen2_l2a_2019_05_12_composite)

  # ---- 2024-04-30-00

sen2_l2a_2024_04_30_singleband_files <- list.files(path = "Sentinel2-L2A/2024-04-30-00", pattern = "\\(Raw).tiff$", full.names = TRUE)

sen2_l2a_2024_04_30_bands <- lapply(sen2_l2a_2024_04_30_singleband_files, rast)

sen2_l2a_2024_04_30_composite <- rast(sen2_l2a_2024_04_30_bands)

names(sen2_l2a_2024_04_30_composite) <- c("B3", "B4", "B8", "B12")

setMinMax(sen2_l2a_2024_04_30_composite)

  # ---- 2024-05-15-00

sen2_l2a_2024_05_15_singleband_files <- list.files(path = "Sentinel2-L2A/2024-05-15-00", pattern = "\\(Raw).tiff$", full.names = TRUE)

sen2_l2a_2024_05_15_bands <- lapply(sen2_l2a_2024_05_15_singleband_files, rast)

sen2_l2a_2024_05_15_composite <- rast(sen2_l2a_2024_05_15_bands)

names(sen2_l2a_2024_05_15_composite) <- c("B3", "B4", "B8", "B12")

setMinMax(sen2_l2a_2024_05_15_composite)

  # ---- 2024-05-30-00

sen2_l2a_2024_05_30_singleband_files <- list.files(path = "Sentinel2-L2A/2024-05-30-00", pattern = "\\(Raw).tiff$", full.names = TRUE)

sen2_l2a_2024_05_30_bands <- lapply(sen2_l2a_2024_05_30_singleband_files, rast)

sen2_l2a_2024_05_30_composite <- rast(sen2_l2a_2024_05_30_bands)

names(sen2_l2a_2024_05_30_composite) <- c("B3", "B4", "B8", "B12")

setMinMax(sen2_l2a_2024_05_30_composite)

  # ---- 2024-06-19-00

sen2_l2a_2024_06_19_singleband_files <- list.files(path = "Sentinel2-L2A/2024-06-19-00", pattern = "\\(Raw).tiff$", full.names = TRUE)

sen2_l2a_2024_06_19_bands <- lapply(sen2_l2a_2024_06_19_singleband_files, rast)

sen2_l2a_2024_06_19_composite <- rast(sen2_l2a_2024_06_19_bands)

names(sen2_l2a_2024_06_19_composite) <- c("B3", "B4", "B8", "B12")

setMinMax(sen2_l2a_2024_06_19_composite)

# Geoprocesamiento de datos raster----------------------------------------------

mdndvi <- c(-2,-0.5,-0.250, -0.100, 0.1, 0.250, 0.5, 2)

ndvi_palette <- c("#440154", "#fde725","#addc30", "#1c7959")

m <- c(-1,-0.1,0.1,0.4,1)

diff_palette <- cpt("ncl_BlWhRe", n = 10)

# Calcular ndvi de cada imágen y reclasificar-----------------------------------

# ---- 2019-05-12-00

ndvi_sen2_l2a_2019_05_12_composite <- (sen2_l2a_2019_05_12_composite$B8 - sen2_l2a_2019_05_12_composite$B4)/(sen2_l2a_2019_05_12_composite$B8 + sen2_l2a_2019_05_12_composite$B4)

setMinMax(ndvi_sen2_l2a_2019_05_12_composite)

reclasificado_ndvi_sen2_l2a_2019_05_12_composite <- classify(ndvi_sen2_l2a_2019_05_12_composite, m, include.lowest=TRUE,brackets = TRUE)

names(reclasificado_ndvi_sen2_l2a_2019_05_12_composite) <- 'ndvi'

reclasificado_ndvi_sen2_l2a_2019_05_12_composite <- terra::as.factor(reclasificado_ndvi_sen2_l2a_2019_05_12_composite)

# ---- 2024-04-30-00

ndvi_sen2_l2a_2024_04_30_composite <- (sen2_l2a_2024_04_30_composite$B8 - sen2_l2a_2024_04_30_composite$B4)/(sen2_l2a_2024_04_30_composite$B8 + sen2_l2a_2024_04_30_composite$B4)

setMinMax(ndvi_sen2_l2a_2024_04_30_composite)

reclasificado_ndvi_sen2_l2a_2024_04_30_composite <- classify(ndvi_sen2_l2a_2024_04_30_composite, m, include.lowest=TRUE,brackets = TRUE)

names(reclasificado_ndvi_sen2_l2a_2024_04_30_composite) <- 'ndvi'

reclasificado_ndvi_sen2_l2a_2024_04_30_composite <- terra::as.factor(reclasificado_ndvi_sen2_l2a_2024_04_30_composite)

# ---- 2024-05-15-00

ndvi_sen2_l2a_2024_05_15_composite <- (sen2_l2a_2024_05_15_composite$B8 - sen2_l2a_2024_05_15_composite$B4)/(sen2_l2a_2024_05_15_composite$B8 + sen2_l2a_2024_05_15_composite$B4)

setMinMax(ndvi_sen2_l2a_2024_05_15_composite)

reclasificado_ndvi_sen2_l2a_2024_05_15_composite <- classify(ndvi_sen2_l2a_2024_05_15_composite, m, include.lowest=TRUE,brackets = TRUE)

names(reclasificado_ndvi_sen2_l2a_2024_05_15_composite) <- 'ndvi'

reclasificado_ndvi_sen2_l2a_2024_05_15_composite <- terra::as.factor(reclasificado_ndvi_sen2_l2a_2024_05_15_composite)

# ---- 2024-05-30-00

ndvi_sen2_l2a_2024_05_30_composite <- (sen2_l2a_2024_05_30_composite$B8 - sen2_l2a_2024_05_30_composite$B4)/(sen2_l2a_2024_05_30_composite$B8 + sen2_l2a_2024_05_30_composite$B4)

setMinMax(ndvi_sen2_l2a_2024_05_30_composite)

reclasificado_ndvi_sen2_l2a_2024_05_30_composite <- classify(ndvi_sen2_l2a_2024_05_30_composite, m, include.lowest=TRUE,brackets = TRUE)

names(reclasificado_ndvi_sen2_l2a_2024_05_30_composite) <- 'ndvi'

reclasificado_ndvi_sen2_l2a_2024_05_30_composite <- terra::as.factor(reclasificado_ndvi_sen2_l2a_2024_05_30_composite)

# ---- 2024-06-19-00

ndvi_sen2_l2a_2024_06_19_composite <- (sen2_l2a_2024_06_19_composite$B8 - sen2_l2a_2024_06_19_composite$B4)/(sen2_l2a_2024_06_19_composite$B8 + sen2_l2a_2024_06_19_composite$B4)

setMinMax(ndvi_sen2_l2a_2024_06_19_composite)

reclasificado_ndvi_sen2_l2a_2024_06_19_composite <- classify(ndvi_sen2_l2a_2024_06_19_composite, m, include.lowest=TRUE,brackets = TRUE)

names(reclasificado_ndvi_sen2_l2a_2024_06_19_composite) <- 'ndvi'

reclasificado_ndvi_sen2_l2a_2024_06_19_composite <- terra::as.factor(reclasificado_ndvi_sen2_l2a_2024_06_19_composite)

# ---- Todos

ndvi_raster_list <- list(
  "2019-05-12" = reclasificado_ndvi_sen2_l2a_2019_05_12_composite,
  "2024-04-30" = reclasificado_ndvi_sen2_l2a_2024_04_30_composite,
  "2024-05-15" = reclasificado_ndvi_sen2_l2a_2024_05_15_composite,
  "2024-05-30" = reclasificado_ndvi_sen2_l2a_2024_05_30_composite,
  "2024-06-19" = reclasificado_ndvi_sen2_l2a_2024_06_19_composite
)

ndvi_raster_list <- ndvi_raster_list[order(names(ndvi_raster_list))]

ndvi_composite_raster_list <- list(
  "2019-05-12" = ndvi_sen2_l2a_2019_05_12_composite,
  "2024-04-30" = ndvi_sen2_l2a_2024_04_30_composite,
  "2024-05-15" = ndvi_sen2_l2a_2024_05_15_composite,
  "2024-05-30" = ndvi_sen2_l2a_2024_05_30_composite,
  "2024-06-19" = ndvi_sen2_l2a_2024_06_19_composite
)

# Maoear colores a los valores del ndvi-----------------------------------------

map_colors <- function(raster, palette) {
  levels <- levels(raster)[[1]]
  n <- nrow(levels)
  cols <- colorFactor(palette, domain = 1:n)(1:n)
  list(colors = cols, levels = levels)
}

color_mapping <- map_colors(reclasificado_ndvi_sen2_l2a_2019_05_12_composite, ndvi_palette)

# UI----------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage(
                  "Explorador de imágenes satelitales:", id = "variables",
                  
                  # ---- UI NDVI
                  
                  tabPanel("NDVI",
                           
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput(inputId = "fechaInput1ndvi", label = "Seleccione una fecha de inicio:", 
                                           choices = names(ndvi_raster_list)),
                               selectInput(inputId = "fechaInput2ndvi", label = "Seleccione una fecha final:", 
                                           choices = names(ndvi_raster_list)),
                               sliderInput(inputId = "opacity", label = "Opacity:", min = 0, max = 1, value = 1, step = 0.1),
                               
                               div(style = "margin-top: 40px;"),
                               
                               uiOutput("textoNDVI"),
                               
                             ),
                             
                             mainPanel(
                               
                               combineWidgetsOutput('MapasSyncNDVI', width = "100%", height = "1000px")
                               
                             )
                           ),
                           
                           div(style = "margin-top: 40px;"),
                           
                           div(style = "margin-top: 40px;")
                  )
                  
                )
)


server <- function(input, output) {
  
  # ---- Server NDVI
  
  image1ndvi <- reactive({
    ndvi_raster_list[[input$fechaInput1ndvi]]
  })
  
  image2ndvi <- reactive({
    ndvi_raster_list[[input$fechaInput2ndvi]]
  })
  
  map1ndvi <- reactive({
    
    mapa1 <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE, minZoom = 6)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Base") %>%
      addRasterImage(
        image1ndvi(),
        colors = color_mapping$colors,
        opacity = input$opacity,
        project = TRUE,
        group = "NDVI"
      ) %>%
      addLegend(
        title = "NDVI",
        colors = ndvi_palette,
        values = color_mapping$levels$ID,
        labels = c("(-1 a -0.1)", "(-0.1 a 0.1)", "(0.1 a 0.4)", "(0.4 a 1)"),
        opacity = 1
      ) %>%
      setMaxBounds(study_area_bbox[1]-1, study_area_bbox[2]-1, study_area_bbox[3]+1, study_area_bbox[4]+1) %>%
      syncWith("mapas_p1")
    
    mapa1
  })
  
  map2ndvi <- reactive({
    
    mapa2 <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE, minZoom = 6)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Base") %>%
      addRasterImage(
        image2ndvi(),
        colors = color_mapping$colors,
        opacity = 1,
        project = TRUE,
        group = "NDVI"
      ) %>%
      addLegend(
        title = "NDVI",
        colors = ndvi_palette,
        values = color_mapping$levels$ID,
        labels = c("(-1 a -0.1)", "(-0.1 a 0.1)", "(0.1 a 0.4)", "(0.4 a 1)"),
        opacity = 1
      ) %>%
      setMaxBounds(study_area_bbox[1]-1, study_area_bbox[2]-1, study_area_bbox[3]+1, study_area_bbox[4]+1) %>%
      syncWith("mapas_p1")
    
    mapa2
    
  })
  
  image3ndvi <- reactive({
    
    ndvi_difference <- ndvi_composite_raster_list[[input$fechaInput1ndvi]] - ndvi_composite_raster_list[[input$fechaInput2ndvi]]
    
    reclasificado_ndvi_difference <- classify(ndvi_difference, mdndvi, include.lowest=TRUE,brackets = TRUE)
    
    reclasificado_ndvi_difference <- terra::as.factor(reclasificado_ndvi_difference)
    
    reclasificado_ndvi_difference
    
  })
  
  map3ndvi <- reactive({
    
    ndvi_diff <- image3ndvi()
    ndvi_diff_min <- min(values(ndvi_diff), na.rm = TRUE)
    ndvi_diff_max <- max(values(ndvi_diff), na.rm = TRUE)
    
    dndvi_palette <- viridis_pal(option = "F", direction = -1)(ndvi_diff_max + 1)
    
    map_diff <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE, minZoom = 6)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Base") %>%
      addRasterImage(
        ndvi_diff,
        colors = dndvi_palette,
        opacity = 1,
        project = TRUE,
        group = "ndvi Difference"
      ) %>%
      setMaxBounds(study_area_bbox[1]-1, study_area_bbox[2]-1, study_area_bbox[3]+1, study_area_bbox[4]+1) %>%
      syncWith("mapas_p1")
    
    map_diff
    
  })
  
  output$MapasSyncNDVI = renderCombineWidgets({
    
    combineWidgets(map1ndvi(), map2ndvi(), map3ndvi(), ncol = 1, title = "", footer = "Cálculo de NDVI con imágenes de Sentinel 2 - L2A.")
    
  })
  
  output$textoNDVI <- renderUI({
    HTML("
<p>
El <b>Índice de Vegetación de Diferencia Normalizada (NDVI)</b> es un indicador utilizado para analizar la densidad y salud de la vegetación mediante imágenes satelitales. El NDVI se calcula a partir de la diferencia entre las bandas 8 y 4, normalizada por la suma de ambas bandas (<b>NDVI = (B8-B4)(B8+B4)</b>). Su valor oscila entre -1 y 1. Los valores negativos de NDVI (valores cercanos a -1) corresponden a cuerpos de agua. Los valores cercanos a cero (-0.1 a 0.1) generalmente corresponden a áreas desérticas de roca, arena o nieve. Los valores positivos bajos representan áreas de arbustos y pastizales (aproximadamente 0.2 a 0.4), mientras que los valores altos indican bosques templados y tropicales (valores cercanos a 1). (<a>https://custom-scripts.sentinel-hub.com/sentinel-2/ndvi/ </a>)
<br>
<br>
Este proyecto utiliza imágenes satelitales de <b>Sentinel-2-L2A con cobertura máxima de nubes del 10%</b> a través de Sentinel Hub para crear mapas de NDVI de <b>Valle de Bravo</b> en las fechas de interés (principalmente antes y después de los incenciods del 6 de mayo 2024 y una imágen del 12 de mayo 2019 como referencia). Los mapas de NDVI son cruciales para monitorear y gestionar la salud de la vegetación en áreas naturales. Permiten identificar cambios en la cobertura vegetal debido a factores como la deforestación, el cambio climático o la actividad humana. (<a>https://www.sentinel-hub.com/ </a>)
<br>
<br>
Con este propósito, se genera un tercer mapa que muestra la diferencia en NDVI entre las dos fechas seleccionadas, resaltando los cambios durante este período.
</p>
<ol>
      <li><b>Las zonas marcadas en morado oscuro o negro</b> generalmente reflejan áreas donde la densidad y salud de la vegetación disminuyeron (como bosques transformados en áreas desérticas o en áreas de arbustos y pastizales). El impacto de los incendios se ve reflejado en este color - particularmente entre las fechas 2024-04-30 y 2024-05-15. 
      <li><b>Las zonas marcadas en naranja o blanco</b> pueden representar: 1) áreas donde la densidad y salud de la vegetación aumentaron (como áreas desérticas o áreas de arbustos y pastizales transformados en bosques), y 2) áreas donde hubo desecación o pérdida de cuerpos de agua (como ríos, lagos o superficies de agua que se convirtieron en áreas desérticas, áreas de arbustos y pastizales, o bosques).
</ol>
  ")
  })
  
}

shinyApp(ui = ui, server = server)




