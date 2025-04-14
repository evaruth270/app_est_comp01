library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(report)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Se mantiene el tema "cosmo"
  
  # Personalización con CSS
  tags$style(HTML("
    /* Color de fondo principal */
    body {
      background-color: #ecf0f1; /* Gris claro */
    }
    
    /* Botones */
    .btn-custom {
      background-color: #3498db;  /* Azul brillante */
      color: white;
      border: none;
      padding: 12px 25px;
      font-size: 16px;
      border-radius: 8px;
      text-align: center;
      width: 100%;
    }
    .btn-custom:hover {
      background-color: #2980b9;
    }
    
    /* Contenedores de texto y gráficos */
    .container-custom {
      background-color: #ffffff;  /* Fondo blanco para los contenedores */
      border-radius: 12px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      margin-top: 20px;
    }
    
    /* Estilos de tabla */
    table {
      width: 100%;
      border-collapse: collapse;
    }
    table, th, td {
      border: 1px solid #ddd;
    }
    th, td {
      padding: 10px;
      text-align: center;
    }
    th {
      background-color: #f1f1f1;
    }
    
    /* Espaciado general */
    .content {
      padding: 20px;
    }
  ")),
  
  # Interfaz de usuario
  titlePanel("✨ Análisis Estadístico de Datos"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "container-custom",
          fileInput("archivo", "📥 Sube un archivo Excel (.xlsx):", accept = ".xlsx"),
          actionButton("analizar", "📊 Analizar Archivo", icon = icon("chart-bar"), class = "btn-custom"),
          actionButton("guardar", "💾 Guardar Reporte", icon = icon("download"), class = "btn-custom")
      )
    ),
    
    mainPanel(
      div(class = "container-custom",
          h4("📋 Vista previa de los datos:"),
          tableOutput("vista_datos"),
          h4("📌 Recomendación de prueba estadística:"),
          verbatimTextOutput("recomendacion"),
          h4("📈 Gráfico:"),
          plotOutput("grafico"),
          h4("🧪 Resultado del test:"),
          verbatimTextOutput("resultado_test"),
          h4("🧠 Interpretación del resultado:"),
          htmlOutput("interpretacion"),
          h4("📃 Reporte interpretativo (package report):"),
          verbatimTextOutput("reporte_report")
      )
    )
  )
)

server <- function(input, output) {
  datos <- eventReactive(input$analizar, {
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })
  
  output$vista_datos <- renderTable({
    head(datos(), 10)
  })
  
  output$recomendacion <- renderText({
    df <- datos()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    cat_cols <- cat_cols[sapply(df[cat_cols], function(x) length(unique(x)) <= 10)]
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(cat_cols) == 0 || length(num_cols) == 0) return("⚠️ No se encontraron variables adecuadas.")
    
    grupo <- as.factor(df[[cat_cols[1]]])
    n_grupos <- length(unique(grupo))
    
    if (n_grupos == 2) {
      "✅ Recomendación: usar prueba **t de Student** (2 grupos)."
    } else if (n_grupos > 2) {
      "✅ Recomendación: usar prueba **ANOVA** (más de 2 grupos)."
    } else {
      "⚠️ No hay suficientes grupos para una comparación."
    }
  })
  
  output$grafico <- renderPlot({
    df <- datos()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    cat_cols <- cat_cols[sapply(df[cat_cols], function(x) length(unique(x)) <= 10)]
    num_cols <- names(df)[sapply(df, is.numeric)]
    req(length(cat_cols) > 0, length(num_cols) > 0)
    
    ggplot(df, aes_string(x = cat_cols[1], y = num_cols[1], fill = cat_cols[1])) +
      geom_boxplot() +
      theme_minimal() +
      labs(
        x = cat_cols[1],
        y = num_cols[1],
        title = paste("Comparación de", num_cols[1], "según", cat_cols[1])
      ) +
      theme(legend.position = "none")
  })
  
  output$resultado_test <- renderPrint({
    df <- datos()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    cat_cols <- cat_cols[sapply(df[cat_cols], function(x) length(unique(x)) <= 10)]
    num_cols <- names(df)[sapply(df, is.numeric)]
    req(length(cat_cols) > 0, length(num_cols) > 0)
    
    grupo <- as.factor(df[[cat_cols[1]]])
    valor <- df[[num_cols[1]]]
    n_grupos <- length(unique(grupo))
    
    if (n_grupos == 2) {
      print(t.test(valor ~ grupo))
    } else if (n_grupos > 2) {
      print(summary(aov(valor ~ grupo)))
    } else {
      cat("⚠️ No hay suficientes grupos para aplicar una prueba.")
    }
  })
  
  output$interpretacion <- renderUI({
    df <- datos()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    cat_cols <- cat_cols[sapply(df[cat_cols], function(x) length(unique(x)) <= 10)]
    num_cols <- names(df)[sapply(df, is.numeric)]
    req(length(cat_cols) > 0, length(num_cols) > 0)
    
    grupo <- as.factor(df[[cat_cols[1]]])
    valor <- df[[num_cols[1]]]
    n_grupos <- length(unique(grupo))
    p <- NA
    
    if (n_grupos == 2) {
      p <- t.test(valor ~ grupo)$p.value
    } else if (n_grupos > 2) {
      p <- summary(aov(valor ~ grupo))[[1]][["Pr(>F)"]][1]
    }
    
    if (is.na(p)) {
      HTML("⚠️ No se pudo calcular el valor p.")
    } else if (p < 0.05) {
      HTML(paste0("🧠 <b>Interpretación:</b> Existe una <span style='color:green'><b>diferencia significativa</b></span> entre los grupos (p = ", round(p, 4), ")."))
    } else {
      HTML(paste0("🧠 <b>Interpretación:</b> <span style='color:red'><b>No</b></span> se encontró una diferencia significativa entre los grupos (p = ", round(p, 4), ")."))
    }
  })
  
  output$reporte_report <- renderPrint({
    df <- datos()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    cat_cols <- cat_cols[sapply(df[cat_cols], function(x) length(unique(x)) <= 10)]
    num_cols <- names(df)[sapply(df, is.numeric)]
    req(length(cat_cols) > 0, length(num_cols) > 0)
    
    grupo <- as.factor(df[[cat_cols[1]]])
    valor <- df[[num_cols[1]]]
    n_grupos <- length(unique(grupo))
    
    if (n_grupos == 2) {
      modelo <- t.test(valor ~ grupo)
      print(report(modelo))
    } else if (n_grupos > 2) {
      modelo <- aov(valor ~ grupo)
      print(report(modelo))
    } else {
      cat("⚠️ No hay suficientes grupos para generar un reporte interpretativo.")
    }
  })
}

shinyApp(ui = ui, server = server)


