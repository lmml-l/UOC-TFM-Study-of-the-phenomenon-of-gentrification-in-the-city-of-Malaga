setwd("C:\\code\\TFM\\RStudio\\TFM")

install.packages('dplyr')
install.packages('shiny')
install.packages('leaflet')
install.packages('shinyjs')
install.packages('DT')
install.packages('sf')
install.packages("promises")
install.packages("fastmap")
library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(DT)
library(sf)
library(ggplot2)


temp_dir <- tempdir()
unzip("Datos/da_cartografiaSeccionCensal-4326.zip", exdir = temp_dir)
secciones <- st_read(temp_dir) %>%
   st_zm(data_sf) %>%
  st_transform(crs = 4326) 

secciones <- secciones[!is.na(st_geometry(secciones)), ]

print(secciones)

data_2011 <- read.csv("Datos/df_2011.csv",sep = ";")
data_2021_rf <- read.csv("Datos/df_2021_rf.csv",sep = ";")

data <- data.frame()

#print(data_2011)
#print(data_2021_rf)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Visualización gentrificación en Málaga"),
  sidebarLayout(
    sidebarPanel(
      # Aquí puedes añadir elementos de entrada como selectores, botones, etc.
      selectInput("date", "Selecciona Año:", choices = c(2011,2021)),
      selectInput("variable", "Selecciona variable de gentrificación:", choices = c("SES","RANK","Predicted_SES"))
    ),
    mainPanel(
      # Aquí puedes añadir elementos de salida como gráficos, tablas, texto, etc.
      leafletOutput(outputId = "map", height = "500px")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$date) # Ensure required inputs are available
    data <- if (input$date == 2011) data_2011 else data_2021_rf
    req(input$variable)
    data %>%
      mutate(valor = .data[[input$variable]])
  })
  output$map <- renderLeaflet({
    data_for_map <-filtered_data()
    print(names(data_for_map))
    print(names(secciones))
    secciones$valor <- secciones %>% left_join(data_for_map, by = "NUMSECCENS") %>% .$valor
    pal <- if (input$variable == "RANK") {
      colorNumeric(palette = "YlOrRd", domain = data_for_map$valor, na.color = "#FFFFFF")
    } else {
      colorNumeric(palette = "plasma", domain = data_for_map$valor, na.color = "#FFFFFF")
    }
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = secciones, fillColor = ~pal(valor), weight = 1, color = "#000000", fillOpacity = 0.5) %>%
      addLegend(pal = pal, values = secciones$valor, title = input$variable, position = "bottomright")
  })
}


shinyApp(ui = ui, server = server)