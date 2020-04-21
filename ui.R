library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Neural Network"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Inputs"),
      div(
        fileInput("file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),

        # Horizontal line ----
        tags$hr()
      ),
      div(
        numericInput("nStepMax", "Maximum steps for the training", min = 1000, max = 10000, value = 100),
        numericInput("nRepetitions", "The number of repetitions", min = 1, value = 10),
        sliderInput("nLayers", "Number of Hidden Layers:", min = 1, max =10, value =2),
        uiOutput("layersSliderInputs"),
        # sliderInput("nNodesL1", "Layer 1 Hidden Nodes:", min = 10, max =20, value =10),
        # sliderInput("nNodesL2", "Layer 2 Hidden Nodes:", min = 5, max =10, value =5),
        selectInput(
          "input_fields",
          label = h5("Input fields"),
          "",
          multiple = TRUE
        ),
        selectInput(
          "output_fields",
          label = h5("Output fields"),
          "",
          multiple = TRUE
        ),
        actionButton("btnTrain","Train Model and Visualize")
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",  verbatimTextOutput("summary")),
        tabPanel("Preview",  tableOutput("dataPreview")),
        tabPanel("Exploration",  plotOutput("net"), plotOutput("roc"))
      )

    )
  )
))
