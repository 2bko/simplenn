library(shiny)
library(neuralnet)
library(NeuralNetTools)
library(ROCR)
library(caret)
library(DT)

shinyServer(function(input, output, session) {
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    }

    input$file
  })

  dataset <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    }

    data <-
      read.csv(
        inFile()$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    
    data <- na.omit(data)
    data[complete.cases(data),]
    
    output$dataPreview <- renderTable(data, server = TRUE)
    output$summary <- renderPrint(summary(data))

    data
  })
  
  observeEvent(input$nLayers, {
    layer <- input$nLayers

    output$layersSliderInputs <- renderUI({
      lapply(1:layer, function(i) {
        label <- paste0("Layer ", i, " Hidden Nodes:")
        name <- paste0("nNodesL", i)
        sliderInput(name, label, min = 1, max =20, value =2)
      })
    })
  })


  observe({
    updateSelectInput(session, "input_fields", choices = names(dataset()))
    updateSelectInput(session, "output_fields", choices = names(dataset()))
  })

  observeEvent(input$btnTrain, {
    set.seed(123)

    dataset <- dataset()

    dataset[] <- lapply(dataset, function(x) {
      if (is.factor(x))
        as.numeric(x)
      else
        x
    })
    
    # sapply(dataset, class)
    summary(dataset)

    # hiddenNodesL1 <- as.numeric(input$nNodesL1)
    # hiddenNodesL2 <- as.numeric(input$nNodesL2)
    
    layer <- input$nLayers
    hidden <- c()
    
    for (i in 1:layer) {
      name <- paste0("nNodesL", i)
      hidden <- c(hidden, as.numeric(input[[name]]))
    }
    
    repetitions <- input$nRepetitions
    stepmax <- input$nStepMax
    
    inputFields <- input$input_fields
    outputFields <- input$output_fields
    
    smp_size <- floor(0.75 * nrow(dataset))
    train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
    
    train <- dataset[train_ind, ]
    test <- dataset[-train_ind, ]
    
    leftFormula <- paste(inputFields, collapse = " + ")
    rightFormula <- paste(outputFields, collapse = " + ")

    formula <- as.formula(paste(rightFormula, "~", leftFormula))
    
    withProgress(message = 'Training Neural Net',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   incProgress(1 / 3)
                   model_nn <-
                     neuralnet(
                       formula,
                       data = train,
                       hidden = hidden,
                       linear.output = F,
                       rep = repetitions,
                       stepmax = stepmax,
                       lifesign = "minimal",
                       threshold = 0.01,
                       act.fct = "tanh"
                     )
                   prob <- neuralnet::compute(model_nn, test[, model_nn$model.list$variables])
                   prob.result <- prob$net.result
                   
                   output$net <- renderPlot({
                     plot(model_nn, rep="best")
                   })
                   output$exportBtn <- NULL
                   incProgress(2 / 3)
                   
                   tt <- test[, c(names(test)[1], model_nn$model.list$variables, input$output_fields)]
                   tt$Prediction <- prob$net.result
                   
                   output$prediction_table <- renderTable({
                     tt
                   }, row.names = TRUE)
                   output$exportResult <- downloadHandler(filename = "result.csv",
                                                          content = function(file) {
                                                            if (is.null(tt)) {
                                                              return()
                                                            }
                                                            write.csv(tt, file, row.names = FALSE)
                                                          })
                   output$exportBtn <- renderUI({
                     if (!is.null(tt)) {
                       downloadButton("exportResult", "Export")
                     }
                   })
                   print("Tst")
                   prob <- neuralnet::compute(model_nn, test[,model_nn$model.list$variables])
                   pred <- ifelse(prob$net.result > 0.5, 1, 0)
                   
                   output$cm <- renderPrint({
                     confusionMatrix(factor(pred), factor(test[[outputFields]]))
                   })
                   
                   nn.pred = ROCR::prediction(prob.result, test[[outputFields]])
                   pref <- performance(nn.pred, "tpr", "fpr")
                   pref2 <- performance(nn.pred, "prec", "rec")
                   pref3 <- performance(nn.pred, "sens", "spec")
                   
                   AUCTestNN <- performance(nn.pred, "auc")
                   AUCTestNN <- as.numeric(AUCTestNN@y.values)
                   output$auc <- renderText({
                     paste0("AUC: ", AUCTestNN)
                   })
                   output$roc <- renderPlot({
                     plot(pref, main="ROC Curve")
                   })
                   output$precision_recall <- renderPlot({
                     plot(pref2, main="Precision/recall")
                   })
                   output$sensivity_specifity <- renderPlot({
                     plot(pref3, main="Sensitivity/specificity plots")
                   })
                   # garson plot working with 1 hidden layer only
                   if (layer == 1) {
                     output$garson <- renderPlot({
                      garson(model_nn)
                     })
                   }
                   output$df <- renderTable(df)
                   incProgress(3 / 3)
                 })
  })
})
