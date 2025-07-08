# Load packages ----
list.of.packages <- c("readr", "caret", "e1071", "Metrics", "dplyr", 
                      "shiny", "bslib")
#install.packages(list.of.packages, quiet = TRUE)

suppressMessages(suppressWarnings(invisible(sapply(list.of.packages, require, 
                                                   character.only = TRUE))))

# Source helpers ----
source("different_bones_sorting.R")

# User interface ----
ui <- page_sidebar(
  title = "Sorting Different Long Bones",
  theme = bs_theme(
    bg = "#faf4ff",
    fg = "black",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  sidebar = sidebar(
    # fileInput(
    #   "file1", "Choose CSV file", accept = ".csv"
    # ),
    helpText(
      "Select the first long bone to work with:"
    ),
    selectInput(
      inputId = "bone_1",
      label = "First bone",
      choices = c("femur", "tibia", "humerus")
    ),
    helpText(
      "Select the second long bone to work with:"
    ),
    selectInput(
      inputId = "bone_2",
      label = "Second bone",
      choices = c("femur", "tibia", "humerus")
    ),
    helpText(
      "Select the type of you working dataset:"
    ),
    selectInput(
      inputId = "side",
      label = "Dataset",
      choices = c("left", "right", "pooled", "full")
    ),
    helpText(
      "Select a Distance to work with."
    ),
    selectInput(
      inputId = "distance",
      label = "Distance",
      choices = c("euclidean", 
                  "maximum", 
                  "manhattan",
                  "canberra",
                  "minkowski")
    ),
    br(),
    sliderInput("thr_id", "Threshold value:",
                min = 2, max = 3,
                value = 2.5, step=0.25),
    br(),
    helpText(
      "If you know the ground truth and wish to evaluate
      the sorting method:"
    ),
    checkboxInput(
      "gT",
      "Validation check",
      value = FALSE
    ),
    card(
      actionButton("go", "Choose your data files.")
    )
  ),
  card(
    card_header("Hello! Please confirm the parameters you wish to work with:"),
    textOutput("text"), max_height = 100
  ),
  card(
    card_header("The predicted pairs from the 1st bone are:"),
    tableOutput("res_1")
    ),
  card(
    card_header("The predicted pairs from the 2nd bone are:"),
    tableOutput("res_2")
  )
)
# Server logic
server <- function(input, output) {
  
  bone_1 <- reactive({
    input$bone_1
  })

  bone_2 <- reactive({
    input$bone_2
  })
  
  side <- reactive({
    input$side
  })
  
  distance <- reactive({
    input$distance
  })
  
  
  threshold_value <- reactive({
                      if (input$thr_id == 2)
                        {threshold_value = 2} else if (input$thr_id == 2.25)
                        {threshold_value = 225} else if (input$thr_id == 2.5)
                        {threshold_value = 25} else if (input$thr_id == 2.75)
                        {threshold_value = 275} else if (input$thr_id == 3)
                        {threshold_value = 3}

                  })
  
  my_text <- renderText({
     paste0("You are working with ", bone_1(), " and ", bone_2(), " bones,
            while using the ", distance(), " distance.")
        })


   output$text <- reactive({
     my_text()
   })
   
   
  gt <- reactive({
    input$gT
  })

  
   observeEvent(input$go, {

     res <- db_sorting(bone_1(), bone_2(), side(), distance(), threshold_value(), ground_truth = gt())
     results_1 <- renderTable({
       capture.output(res[[1]], type="output")
       })# <-- Modified the call to the script
    
     results_2 <- renderTable({
       capture.output(res[[2]], type="output")
     })
     
     output$res_1 <- results_1
     output$res_2 <- results_2
     })

}

# Run the app
shinyApp(ui, server)
