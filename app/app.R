# Paquetes
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(shiny)
library(shinydashboard)


# Datos

# Lectura de una capa vectorial (GeoJSON) de distritos de Montes de Oca
distritos <-
    st_read(
        "https://raw.githubusercontent.com/MontserratJB/Proyecto-2021/master/Distritos.geojson",
        quiet = TRUE
    )

# Lectura de un archivo CSV con datos de Presupuestos participativos en Montes de Oca
presupuesto <-
    st_read(
        "/vsicurl/https://raw.githubusercontent.com/MontserratJB/tarea03-shinyapp/master/Pres_part.csv",
        options = c(
            "X_POSSIBLE_NAMES=decimalLongitude",
            "Y_POSSIBLE_NAMES=decimalLatitude"
        ),
        quiet = TRUE
    )

# Asignación de un CRS al objeto felidae
st_crs(presupuesto) <- 4326

# Lectura de una capa raster de altitud
altitud <-
    rast(
        "/vsicurl/https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/master/datos/worldclim/altitud.tif"
    )


# Lista ordenada de distritos + "Todas"
lista_distritos <- unique(presupuesto$Distrito)
lista_distritos <- sort(lista_distritos)
lista_distritos <- c("Todas", lista_distritos)

# Lista ordenada de numero de proyecto por distrito + "Todas"
lista_proyectos <- unique(presupuesto$No_Proyecto)
lista_proyectos <- sort(lista_proyectos)
lista_proyectos <- c("Todas", lista_proyectos)


# Componentes de la aplicación Shiny
# Definición del objeto ui
ui <-
    dashboardPage(
        dashboardHeader(title = "Presupuestos participativos en el cantón de Montes de Oca"),
        dashboardSidebar(sidebarMenu(
            menuItem(
                text = "Filtros",
                selectInput(
                    inputId = "distrito",
                    label = "Distrito",
                    choices = lista_distritos,
                    selected = "Todas"
                ),
                selectInput(
                    inputId = "num_proyecto",
                    label = "Numero de proyecto por distrito",
                    choices = lista_proyectos,
                    selected = "Todas"
                ),
                startExpanded = TRUE
                ) 
              )),
        dashboardBody(fluidRow(
            box(
                title = "Mapa de Presupuestos Participativos",
                leafletOutput(outputId = "mapa"),
                width = 6
            ),
            box(
                title = "Tabla de proyectos",
                DTOutput(outputId = "tabla"),
                width = 6
            )
        ),
        fluidRow(
            box(
                title = "Grafico por Distrito",
                plotlyOutput(outputId = "grafico_distritos"),
                width = 12
            )      
        ))
    )

# Definición de la función server
server <- function(input, output, session) {
    filtrarRegistros <- reactive({
        # Remoción de geometrías y selección de columnas
        presupuesto_filtrado <-
            presupuesto %>%
            dplyr::select(Distrito, No_Proyecto)
        
        # Filtrado de felidae por especie
        if (input$distrito != "Todas") {
            presupuesto_filtrado <-
                presupuesto_filtrado %>%
                filter(Distrito == input$distrito)
        }
        # Filtrado de felidae por provincia
        if (input$num_proyecto != "Todas") {
            presupuesto_filtrado <-
                presupuesto_filtrado %>%
                filter(No_Proyecto == input$num_proyecto)
        }
        
        return(presupuesto_filtrado)
    })
    
    output$mapa <- renderLeaflet({
        registros <-
            filtrarRegistros()
        
        # Conversión del objeto altitud a la clase RasterLayer
        altitud_rl <- raster::raster(altitud)
        
        # Mapa Leaflet con capas de distritos y registros de presupuestos participativos en Montes de Oca
        leaflet() %>%
            setView(lng = -84.01705,
                    lat = 9.940166,
                    zoom = 13) %>%
            addTiles() %>%
            addRasterImage(altitud_rl,
                           opacity = 0.6) %>%
            addPolygons(
                data = distritos,
                color = "black",
                fillColor = "transparent",
                stroke = TRUE,
                weight = 1.0,
            ) %>%
            addCircleMarkers(
                data = registros,
                stroke = TRUE,
                radius = 4,
                fillColor = 'red',
                fillOpacity = 1,
                label = paste0(
                    registros$No_Proyecto,
                    ", ",
                    registros$Distrito,
                    ", ",
                    registros$Inversion
                )
            )
    })
    
    output$tabla <- renderDT({
        registros <- filtrarRegistros()
        
        registros %>%
            st_drop_geometry() %>%
            datatable()
    })
    
    output$grafico_distritos <- renderPlotly({
        registros <- filtrarRegistros()
        
        registros %>%
            st_drop_geometry() %>%
            group_by(Distrito) %>%
            summarize(suma_proyectos = n()) %>%
            plot_ly(x = ~ Distrito,
                    y = ~ suma_proyectos,
                    type="bar", mode="markers", fill = "tozeroy", fillcolor = "green") %>%
            layout(title = "Proyectos por Distrito",
                   xaxis = list(title = "Distritos"),
                   yaxis = list(title = "Número de proyectos")) %>%
            config(locale = "es")
            
    })
}

# Llamado a la función shinyApp()
shinyApp(ui, server)
