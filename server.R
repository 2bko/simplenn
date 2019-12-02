library(shiny)
library(neuralnet)
library(NeuralNetTools)

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

    output$dataPreview <- renderTable(data)
    output$summary <- renderPrint(summary(data))

    data
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
    sapply(dataset, class)
    summary(dataset)

    hiddenNodesL1 <- as.numeric(input$nNodesL1)
    hiddenNodesL2 <- as.numeric(input$nNodesL2)

    inputFields <- input$input_fields
    outputFields <- input$output_fields

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
                       data = dataset,
                       hidden = c(hiddenNodesL1, hiddenNodesL2),
                       linear.output = FALSE,
                       rep = 50,
                       stepmax = 10000
                     )
                   incProgress(2 / 3)
                   output$mainPlot <- renderPlot({
                     plotnet(model_nn)
                   })
                   output$df <- renderTable(df)
                   incProgress(3 / 3)
                 })
  })

})
