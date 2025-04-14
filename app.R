library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("✨ Recomendador de Prueba Estadística"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "📥 Sube un archivo Excel (.xlsx):", accept = ".xlsx"),
      actionButton("analizar", "📊 Analizar Archivo", icon = icon("chart-bar"))
    ),
    
    mainPanel(
      h4("📋 Vista previa de los datos:"),
      tableOutput("vista_datos"),
      h4("📌 Recomendación de prueba estadística:"),
      verbatimTextOutput("recomendacion"),
      h4("📈 Gráfico:"),
      plotOutput("grafico"),
      h4("🧪 Resultado del test:"),
      verbatimTextOutput("resultado_test"),
      h4("🧠 Interpretación del resultado:"),
      htmlOutput("interpretacion")
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
}

shinyApp(ui = ui, server = server)


