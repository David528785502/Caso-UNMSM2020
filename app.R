# Cargar librerías
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Cargar Datasets
dataset1 <- read.csv("Datasets/Data1.csv")
dataset2 <- read.csv("Datasets/Data2.csv")
dataset3 <- read.csv("Datasets/Data3.csv")

# UI
ui <- fluidPage(
  titlePanel("CASO UNMSM 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Selecciona un dataset:", 
                  choices = c("Repitencias", "Calificaciones", "Tutorias")),
      uiOutput("faculty_selector")
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Función para cargar el dataset seleccionado
  data <- reactive({
    switch(input$dataset,
           "Repitencias" = dataset1,
           "Calificaciones" = dataset2,
           "Tutorias" = dataset3)
  })
  
  # UI para selector de código de facultad
  output$faculty_selector <- renderUI({
    if (input$dataset == "Tutorias") {
      selectInput("cod_facultad", "Selecciona un código de facultad:",
                  choices = unique(data()$cod_facultad))
    } else {
      NULL
    }
  })
  
  # Calcular la cantidad de AM y TO por cod_facultad
  cantidad_AM_TO <- reactive({
    if (input$dataset == "Tutorias" && !is.null(input$cod_facultad)) {
      data_filtered <- data() %>%
        filter(cod_facultad == input$cod_facultad) %>%
        mutate(AM = sum(cod_tipo_autorizacion == "AM "),
               TO = sum(cod_tipo_autorizacion == "TO "))
      
      return(data_filtered)
    } else {
      return(NULL)
    }
  })
  
  # Crear gráficos
  
  # Crear gráfico para Repitencias
  output$plot <- renderPlot({
    if (input$dataset == "Repitencias") {
      
      # Calcular el total de repeticiones por cod_facultad
      repeticiones_por_facultad <- aggregate(num_rep ~ cod_facultad, data = data(), sum)
      
      ggplot(repeticiones_por_facultad, aes(x = factor(cod_facultad), y = num_rep)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Total de Repeticiones por Facultad",
             x = "Código de Facultad",
             y = "Total de Repeticiones") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje x para mejor visualización
    }
    
    # Crear gráfico para Calificaciones
    else if (input$dataset == "Calificaciones") {
      
      # Calcular el promedio de la calificación final por código de facultad
      promedio_calificacion <- data() %>%
        group_by(cod_facultad) %>%
        summarise(promedio_calificacion = mean(val_calific_final))
      
      ggplot(promedio_calificacion, aes(x = factor(cod_facultad), y = promedio_calificacion)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Promedio de Calificación Final por Facultad",
             x = "Código de Facultad",
             y = "Promedio de Calificación Final") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje x para mejor visualización
    }
    
    # Crear gráfico para Tutorías
    else if (input$dataset == "Tutorias") {
      if (!is.null(cantidad_AM_TO())) {
        datos <- cantidad_AM_TO() %>%
          gather(key = "Tipo", value = "Cantidad", AM, TO)
        
        ggplot(datos, aes(x = "", y = Cantidad, fill = Tipo)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y") +
          labs(title = "Distribución de Autorizaciones por Tipo",
               fill = "Tipo de Autorización") +
          theme_void() +
          theme(legend.position = "right")
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No hay datos para mostrar", size = 5) +
          theme_void()
      }
    }
    
  })
  
  # Mostrar tabla de datos
  output$table <- renderDT({
    datatable(data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)