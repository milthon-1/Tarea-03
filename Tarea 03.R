library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Comparación de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Cargar archivo (.csv o .xlsx):",
                accept = c(".csv", ".xlsx")),
      uiOutput("variableSelect"),
      checkboxGroupInput("extraFeatures", "Opciones adicionales:",
                         choices = list("Correlación de variables" = "correlation",
                                        "Series de tiempo" = "timeSeries")),
      actionButton("analyze", "Ejecutar análisis"),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Vista Previa",
                 h4("Previsualización de los datos cargados:"),
                 tableOutput("dataPreview")
        ),
        tabPanel("Estadísticas Básicas",
                 h4("Cálculo de estadísticas descriptivas:"),
                 tableOutput("summaryStats")
        ),
        tabPanel("Visualización",
                 h4("Gráficos para analizar las variables:"),
                 plotOutput("variablePlot")
        ),
        tabPanel("Opciones Avanzadas",
                 h4("Resultados de correlación:"),
                 verbatimTextOutput("correlationOutput"),
                 h4("Análisis de series de tiempo:"),
                 plotOutput("timeSeriesPlot")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Cargar datos
  data <- reactive({
    req(input$fileInput)
    ext <- tools::file_ext(input$fileInput$name)
    if (ext == "csv") {
      read.csv(input$fileInput$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$fileInput$datapath)
    } else {
      validate("Formato no soportado. Usa .csv o .xlsx.")
    }
  })
  
  # Vista previa de los datos
  output$dataPreview <- renderTable({
    head(data(), 10)
  })
  
  # Selección de variables
  output$variableSelect <- renderUI({
    req(data())
    selectInput("variables", "Elige las variables numéricas:",
                choices = names(data()), multiple = TRUE)
  })
  
  # Estadísticas descriptivas
  output$summaryStats <- renderTable({
    req(input$variables)
    selectedData <- data()[, input$variables, drop = FALSE]
    selectedData %>%
      summarise(across(everything(), list(
        media = mean,
        mediana = median,
        desv = sd
      ), na.rm = TRUE))
  })
  
  # Histograma de variables seleccionadas
  output$variablePlot <- renderPlot({
    req(input$variables)
    ggplot(data(), aes_string(x = input$variables[1])) +
      geom_histogram(fill = "blue", color = "white", bins = 20) +
      labs(title = paste("Histograma de", input$variables[1]),
           x = input$variables[1], y = "Frecuencia") +
      theme_minimal()
  })
  
  # Correlación de variables
  output$correlationOutput <- renderText({
    req(input$extraFeatures)
    if ("correlation" %in% input$extraFeatures && length(input$variables) > 1) {
      var1 <- input$variables[1]
      var2 <- input$variables[2]
      cor(data()[[var1]], data()[[var2]], use = "complete.obs")
    } else {
      "No se seleccionaron variables suficientes para calcular correlación."
    }
  })
  
  # Análisis de series de tiempo
  output$timeSeriesPlot <- renderPlot({
    req(input$extraFeatures)
    if ("timeSeries" %in% input$extraFeatures) {
      req(input$variables)
      var <- input$variables[1]
      data_selected <- data()[[var]]
      
      # Validar que la variable es numérica
      validate(
        need(is.numeric(data_selected), "La variable seleccionada debe ser numérica.")
      )
      
      ggplot(data(), aes_string(x = 1:nrow(data()), y = var)) +
        geom_line(color = "green") +
        labs(title = "Análisis de serie de tiempo",
             x = "Índice", y = var) +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
