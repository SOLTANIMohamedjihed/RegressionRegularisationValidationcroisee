library(shiny)
library(tidyverse)
library(caret)
library(MLmetrics)
library(MASS)
# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Regression , Regularisation et Validation croisee"),
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Type de régression",
                  choices = c("Régression Logistique", "Régression Ordinale")),
      selectInput("regularization", "Type de régularisation",
                  choices = c("Aucune", "L1", "L2")),
      sliderInput("crossval", "validation croisée",
                  min = 2, max = 10, value = 5, step = 1)
    ),
    mainPanel(
      tableOutput("results"),
      plotOutput("plot")
    )
  )
)

# Définir le serveur
server <- function(input, output) {

  # Charger les données
  data <- reactive({
    iris  # Remplacez "iris" par vos données
  })

  # Effectuer la régression logistique
  logistic_regression <- reactive({
    formula <- as.formula(paste("Species ~", paste(names(data())[-5], collapse = "+")))
    control <- trainControl(method = "cv", number = input$crossval)

    if (input$regularization == "Aucune") {
      glm(formula, data = data(), family = binomial)
    } else if (input$regularization == "L1") {
      train(formula, data = data(), method = "glmnet", family = binomial,
            trControl = control, tuneGrid = expand.grid(alpha = 1, lambda = 0:10))
    } else if (input$regularization == "L2") {
      train(formula, data = data(), method = "glmnet", family = binomial,
            trControl = control, tuneGrid = expand.grid(alpha = 0, lambda = 0:10))
    }
  })

  # Effectuer la régression ordinale
  ordinal_regression <- reactive({
    formula <- as.formula(paste("Species ~", paste(names(data())[-5], collapse = "+")))
    control <- trainControl(method = "cv", number = input$crossval)

    if (input$regularization == "Aucune") {
      polr(formula, data = data())
    } else if (input$regularization == "L1") {
      train(formula, data = data(), method = "glmnet", family = multinomial,
            trControl = control, tuneGrid = expand.grid(alpha = 1, lambda = 0:10))
    } else if (input$regularization == "L2") {
      train(formula, data = data(), method = "glmnet", family = multinomial,
            trControl = control, tuneGrid = expand.grid(alpha = 0, lambda = 0:10))
    }
  })

  # Afficher les résultats
  output$results <- renderTable({
    if (input$method == "Régression Logistique") {
      logistic_regression()
    } else if (input$method == "Régression Ordinale") {
      ordinal_regression()
    }
  })

  # Afficher le graphique
  output$plot <- renderPlot({
    if (input$method == "Régression Logistique") {
      plot(logistic_regression())
    } else if (input$method == "Régression Ordinale") {
      plot(ordinal_regression())
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)