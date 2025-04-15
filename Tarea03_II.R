library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(report)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Análisis de Ejercicios"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Cargar archivo (.csv o .xlsx):",
                accept = c(".csv", ".xlsx")),
      uiOutput("variableSelect"),
      uiOutput("groupVarSelect"),  # Cambiado para variable de grupo
      uiOutput("numericVarSelect"),
      checkboxGroupInput("extraFeatures", "Opciones adicionales:",
                         choices = list("Correlación de variables" = "correlation",
                                        "Series de tiempo" = "timeSeries",
                                        "Interpretación ANOVA" = "anovaReport")),
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
                 plotOutput("timeSeriesPlot"),
                 h4("Resultados numéricos del ANOVA:"),
                 verbatimTextOutput("anovaResults"),
                 h4("Interpretación del ANOVA:"),
                 verbatimTextOutput("anovaReport")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Cargar datos con manejo mejorado de tipos
  data <- reactive({
    req(input$fileInput)
    ext <- tools::file_ext(input$fileInput$name)
    df <- if (ext == "csv") {
      read.csv(input$fileInput$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      read_excel(input$fileInput$datapath)
    } else {
      validate("Formato no soportado. Usa .csv o .xlsx.")
    }
    
    # Convertir solo columnas que puedan convertirse completamente a numéricas
    df <- df %>% mutate(across(where(function(x) {
      if (!is.character(x)) return(FALSE)
      converted <- suppressWarnings(as.numeric(x))
      !anyNA(converted) && all(!is.na(converted))
    }), ~as.numeric(.)))
    
    return(df)
  })
  
  # Vista previa de los datos
  output$dataPreview <- renderTable({
    head(data(), 10)
  })
  
  # Selector para todas las variables (vista previa)
  output$variableSelect <- renderUI({
    req(data())
    selectInput("variables", "Seleccionar variables para vista previa:",
                choices = names(data()), multiple = TRUE)
  })
  
  # Selector para variable de grupo (factores)
  output$groupVarSelect <- renderUI({
    req(data())
    selectInput("groupVar", "Seleccionar variable de grupo (factores):",
                choices = names(data()))
  })
  
  # Selector solo para variables numéricas (análisis)
  output$numericVarSelect <- renderUI({
    req(data())
    numeric_cols <- names(data())[sapply(data(), is.numeric)]
    selectInput("numericVars", "Seleccionar variable numérica para análisis:",
                choices = numeric_cols)
  })
  
  # Estadísticas descriptivas (solo para numéricas)
  output$summaryStats <- renderTable({
    req(input$numericVars)
    selectedData <- data()[, input$numericVars, drop = FALSE]
    
    # Verificar que la variable sea numérica
    validate(
      need(is.numeric(selectedData[[1]]), "La variable seleccionada debe ser numérica")
    )
    
    selectedData %>%
      summarise(across(everything(), 
                       list(
                         media = ~mean(., na.rm = TRUE),
                         mediana = ~median(., na.rm = TRUE),
                         desv = ~sd(., na.rm = TRUE),
                         min = ~min(., na.rm = TRUE),
                         max = ~max(., na.rm = TRUE)
                       ), .names = "{.col}_{.fn}"))
  })
  
  # Histograma de variables seleccionadas
  output$variablePlot <- renderPlot({
    req(input$numericVars)
    
    # Verificar que la variable sea numérica
    validate(
      need(is.numeric(data()[[input$numericVars]]), 
           "Solo se pueden graficar variables numéricas")
    )
    
    ggplot(data(), aes(x = .data[[input$numericVars]])) +
      geom_histogram(fill = "blue", color = "white", bins = 20, na.rm = TRUE) +
      labs(title = paste("Histograma de", input$numericVars),
           x = input$numericVars, y = "Frecuencia") +
      theme_minimal()
  })
  
  # Correlación de variables
  output$correlationOutput <- renderText({
    req(input$extraFeatures)
    if ("correlation" %in% input$extraFeatures) {
      if (length(input$variables) > 1) {
        var1 <- data()[[input$variables[1]]]
        var2 <- data()[[input$variables[2]]]
        
        # Verificar que ambas variables sean numéricas
        if (is.numeric(var1) && is.numeric(var2)) {
          cor_value <- cor(var1, var2, use = "complete.obs")
          paste("Coeficiente de correlación:", round(cor_value, 4))
        } else {
          "Ambas variables deben ser numéricas para calcular correlación."
        }
      } else {
        "Se necesitan dos variables para calcular correlación."
      }
    }
  })
  
  # Análisis de series de tiempo
  output$timeSeriesPlot <- renderPlot({
    req(input$extraFeatures, input$numericVars)
    if ("timeSeries" %in% input$extraFeatures) {
      validate(
        need(is.numeric(data()[[input$numericVars]]), 
             "Solo se pueden graficar series de tiempo con variables numéricas")
      )
      
      ggplot(data(), aes(x = seq_along(.data[[input$numericVars]]), 
                         y = .data[[input$numericVars]])) +
        geom_line(color = "green") +
        labs(title = "Análisis de serie de tiempo",
             x = "Índice", y = input$numericVars) +
        theme_minimal()
    }
  })
  
  # Resultados numéricos del ANOVA
  output$anovaResults <- renderPrint({
    req(input$extraFeatures, input$numericVars, input$groupVar)
    if ("anovaReport" %in% input$extraFeatures) {
      # Verificar que tenemos variable numérica y de grupo
      validate(
        need(is.numeric(data()[[input$numericVars]]), "La variable de respuesta debe ser numérica"),
        need(nlevels(factor(data()[[input$groupVar]])) > 1, "La variable de grupo debe tener al menos 2 niveles")
      )
      
      # Preparar fórmula para ANOVA
      formula <- as.formula(paste(input$numericVars, "~", input$groupVar))
      
      # Realizar ANOVA con manejo de errores
      anova_result <- tryCatch(
        {
          aov(formula, data = data())
        }, 
        error = function(e) {
          return(NULL)
        }
      )
      
      if (is.null(anova_result)) {
        return("Error al realizar ANOVA. Verifica los datos seleccionados.")
      }
      
      # Mostrar tabla ANOVA tradicional
      summary(anova_result)
    }
  })
  
  # Interpretación del ANOVA usando report
  output$anovaReport <- renderText({
    req(input$extraFeatures, input$numericVars, input$groupVar)
    if ("anovaReport" %in% input$extraFeatures) {
      # Verificar que tenemos variable numérica y de grupo
      validate(
        need(is.numeric(data()[[input$numericVars]]), "La variable de respuesta debe ser numérica"),
        need(nlevels(factor(data()[[input$groupVar]])) > 1, "La variable de grupo debe tener al menos 2 niveles")
      )
      
      # Preparar fórmula para ANOVA
      formula <- as.formula(paste(input$numericVars, "~", input$groupVar))
      
      # Realizar ANOVA con manejo de errores
      anova_result <- tryCatch(
        {
          aov(formula, data = data())
        }, 
        error = function(e) {
          return(NULL)
        }
      )
      
      if (is.null(anova_result)) {
        return("Error al realizar ANOVA. Verifica los datos seleccionados.")
      }
      
      # Usar report para generar interpretación
      capture.output(report(anova_result))
    }
  })
}

shinyApp(ui, server)