library(shiny)
library(DALEX)
library(ranger)
library(gbm)
library(xgboost)
library(rms)
library(mlr)

# some additional cleaning
small_data <- readRDS("small_data.rds")
colnames(small_data)[8] = "Zgon"
colnames(small_data)[9] = "Hospitalisation"
small_data$Hospitalisation[small_data$Hospitalisation == -100] = NA

colnames(small_data)[7] = "Przebieg"
levels(small_data$Przebieg)[1] = "Brak"
small_data$Zgon <- ifelse(small_data$Zgon, "Tak", "Nie")

ui <- fluidPage(
    # Application title
    titlePanel("Model przeżycia dla C19"),
    fluidRow(
        column(4, p("")),
        column(4, textOutput("mperf")),
        column(4, uiOutput("selectVar")
               )
    ),
    fluidRow(
        column(4, 
               selectInput("model", "Rodzaj modelu:", c("xgboost", "ranger", "gbm", "rms"), "ranger"),
               selectInput("target", "Zmienna celu:", c("Zgon", "Gorączka", "Kaszel", "Duszność", "Hospitalisation"), "Zgon"),
               checkboxInput("monotonicity", "Monotonicity", TRUE),
               selectInput("dependent", "Zmienne objaśniające:", 
                           c("Wiek", "Płeć", "Środowisko", "Gorączka", "Kaszel", "Duszność", 
                             "Przebieg", "Zgon"), 
                           multiple = TRUE, selected = c("Wiek", "Płeć", "Środowisko", 
                                                         "Przebieg"))),
        column(4, plotOutput("FI", width = 400, height = 400)),
        column(4, plotOutput("PDP", width = 400, height = 400)
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    train_model <- reactive({
        if (input$target == "Hospitalisation") {
            small_data <- na.omit(small_data)
            y <- small_data[,input$target] 
        } else {
            y <- small_data[,input$target] == "Tak"
        }
        x <- small_data[,input$dependent]
        formul <- paste("y ~ ", paste(input$dependent, collapse = " + "))
        model_ex <- switch (input$model,
               ranger = {
                   model <- ranger(y = y, x = x, 
                                   probability = (input$target != "Hospitalisation"), 
                                   classification = (input$target != "Hospitalisation"))   
                   explain(model, data = x, y = y)
               },
               gbm = {
                   model <- gbm(as.formula(formul), data = data.frame(y = y, x),
                                var.monotone = as.numeric(colnames(x) == "Wiek") * input$monotonicity)   
                   explain(model, data = x, y = y)
               },
               xgboost = {
                   dat <- as.matrix(createDummyFeatures(x))
                   model <- xgboost(dat,  y, nrounds = 10,
                                    monotone_constraints = as.numeric(colnames(dat) == "Wiek") * input$monotonicity)
                   explain(model, data = x, y = y,
                           predict_function = function(m,x){
                               x = as.matrix(createDummyFeatures(x))
                               predict(m, x, type="response")
                           })
               },
               rms = {
                   formul <- gsub(formul, pattern = "Wiek", replacement = "rcs(Wiek)")
                   if (input$target == "Hospitalisation") {
                       model <- ols(as.formula(formul), data = data.frame(y = y, x))   
                   } else {
                       model <- lrm(as.formula(formul), data = data.frame(y = y, x))   
                   }
                   explain(model, data = x, y = y)
               }
        )
        model_ex
    })
    
    output$mperf <- renderText({
        model_exp <- train_model()
        mes <- model_performance(model_exp, cutoff = 0.15)$measures
        paste0(names(mes),": ", signif(unlist(mes), 2))
    })
    
    output$selectVar <- renderUI({
        selectInput("selectedVar", "Zmienna do PDP", input$dependent)
    })
    
    output$ROC <- renderPlot({
        model_exp <- train_model()
        mp <- model_performance(model_exp)
        plot(mp, geom = "roc")
    }, width = 400, height = 300)
    
    output$FI <- renderPlot({
        model_exp <- train_model()
        mp <- model_parts(model_exp, N = NULL, B = 1)
        plot(mp)
    }, width = 400, height = 300)
    
    output$PDP <- renderPlot({
        model_exp <- train_model()
        mp <- model_profile(model_exp, variables = input$selectedVar)
        plot(mp)
    }, width = 400, height = 300)
}

# Run the application 
shinyApp(ui = ui, server = server)
