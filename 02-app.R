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
    titlePanel("Mortality modeling for covid-C19"),
    fluidRow(
        column(4, h3("Dataset level explanations")),
        column(4, strong("Model performance"), textOutput("mperf")),
        column(4, uiOutput("selectVar")
               )
    ),
    fluidRow(
        column(4, 
               selectInput("model", "Model:", c("gbm", "xgboost", "ranger", "rms"), "gbm"),
               selectInput("target", "Target variable:", c("Zgon", "Gorączka", "Kaszel", "Duszność", "Hospitalisation"), "Zgon"),
               checkboxInput("monotonicity", "Monotonicity (only gbm and xgboost)", TRUE),
               selectInput("dependent", "Variables:", 
                           c("Wiek", "Płeć", "Środowisko", "Gorączka", "Kaszel", "Duszność", 
                             "Przebieg", "Zgon"), 
                           multiple = TRUE, selected = c("Wiek", "Płeć")),
               dateRangeInput("date_range", "Range of dates:", start = "2020-03-04", min = "2020-03-04", end = "2020-05-20", max = "2020-05-20")),
        column(4, plotOutput("FI", width = 400, height = 400)),
        column(4, plotOutput("PDP", width = 400, height = 400)
        )
    ),
    fluidRow(
        column(4, 
               h3("Instance level explanations"), 
               conditionalPanel(condition = 'input.dependent.includes("Płeć")',
                                selectInput("instance_gender", "Płeć", levels(small_data$Płeć)) ),
               conditionalPanel(condition = 'input.dependent.includes("Wiek")',
                                sliderInput("instance_age", "Wiek", min = 0, max = 100, value = 50) ),
               conditionalPanel(condition = 'input.dependent.includes("Środowisko")',
                                selectInput("instance_habitat", "Środowisko", levels(small_data$Środowisko)) ),
               conditionalPanel(condition = 'input.dependent.includes("Przebieg")',
                                selectInput("instance_severity", "Przebieg", levels(small_data$Przebieg)) ),
               conditionalPanel(condition = 'input.dependent.includes("Gorączka")',
                                selectInput("instance_heat", "Gorączka", levels(small_data$Gorączka)) ),
               conditionalPanel(condition = 'input.dependent.includes("Kaszel")',
                                selectInput("instance_cough", "Kaszel", levels(small_data$Kaszel)) ),
               conditionalPanel(condition = 'input.dependent.includes("Duszność")',
                                selectInput("instance_breath", "Duszność", levels(small_data$Duszność))) ),
        column(4, strong("Model prediction"), 
               textOutput("calc_pred"), 
               plotOutput("BD", width = 400, height = 400)),
        column(4,
               plotOutput("CP", width = 400, height = 400)
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    pred_inst <- reactive({
        data.frame(Płeć = factor(input$instance_gender, levels = levels(small_data$Płeć)),
                   Wiek = input$instance_age,
                   Środowisko = factor(input$instance_habitat, levels = levels(small_data$Środowisko)),
                   Przebieg = factor(input$instance_severity, levels = levels(small_data$Przebieg)),
                   Gorączka = factor(input$instance_heat, levels = levels(small_data$Gorączka)),
                   Kaszel = factor(input$instance_cough, levels = levels(small_data$Kaszel)),
                   Duszność = factor(input$instance_breath, levels = levels(small_data$Duszność)))[,input$dependent]
    })
    
    train_model <- reactive({
        small_data <- small_data[small_data$data_start >= input$date_range[1] &
                                     small_data$data_start <= input$date_range[2],]
        
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
                                    objective = ifelse(input$target != "Hospitalisation", "binary:logistic", "reg:squarederror" ),
                                    monotone_constraints = as.numeric(colnames(dat) == "Wiek") * input$monotonicity)
                   explain(model, data = x, y = y,
                           predict_function = function(m,x){
                               x = as.matrix(createDummyFeatures(x))
                               predict(m, x)
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
    
    output$calc_pred <- renderText({
        model_exp <- train_model()
        inst <- pred_inst()
        paste(signif(predict(model_exp, inst)*100, 3), "%")
    })
    
    output$mperf <- renderText({
        model_exp <- train_model()
        mes <- model_performance(model_exp, cutoff = 0.15)$measures
        paste0(names(mes),": ", signif(unlist(mes), 2))
    })
    
    output$selectVar <- renderUI({
        selectInput("selectedVar", "Variable for PDP/CP", input$dependent)
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
    
    output$BD <- renderPlot({
        vcolors <- DALEX::colors_breakdown_drwhy()[c(3,2,1,4)]
        names(vcolors) <- c("-1", "0",  "1",  "X")
        
        model_exp <- train_model()
        inst <- pred_inst()
        bd <- predict_parts(model_exp, inst)
        plot(bd, vcolors = vcolors) + 
            ggplot2::scale_y_continuous(expand = c(0, 0), 
                                        name = "")
    }, width = 400, height = 300)
    
    output$CP <- renderPlot({
        model_exp <- train_model()
        inst <- pred_inst()
        bd <- predict_profile(model_exp, inst, variables = input$selectedVar)
        plot(bd)
    }, width = 400, height = 300)
    
    output$PDP <- renderPlot({
        model_exp <- train_model()
        mp <- model_profile(model_exp, variables = input$selectedVar)
        plot(mp)
    }, width = 400, height = 300)
}

# Run the application 
shinyApp(ui = ui, server = server)
