# Made by Cole Jackson
# kelvin was here

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
TREE_DATA <- read_csv("~/TreeApp/S22-Tree-App/hbTree/treedata/All_logs.csv")
colnames(TREE_DATA)

# Define UI
ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Tree Title"),
  sidebarLayout(
    sidebarPanel(
      
      # Select type of trend to plot
      selectInput( 
        inputId = "type", label = strong("Select Log"),
        choices = unique(TREE_DATA$Log), selected = "1"),
      
      # Select variable to plot
      selectInput( 
        inputId = "var", label = strong("Select Variable"),
        choices = colnames(TREE_DATA), selected = "air_temp"),
      
      # Select date range to be plotted
      dateRangeInput( 
        "datetime0", strong("Date range"),  min = "2020-07-02", max = "2020-10-27",
        start = "2020-07-02 12:45:00 UTC", end = "2020-10-27 08:00:00 UTC")
    ),
    
    # Output: Lineplot
    mainPanel(
      plotOutput(outputId = "lineplot", height = "300px")
    )
  )
)

# Define server function
server <- function(input, output) {
  # Subset data
  selected_range <- reactive({
    req(input$datetime0)
    req(input$var)
    validate(need(!is.na(input$datetime0[1]) & !is.na(input$datetime0[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$datetime0[1] < input$datetime0[2],
                  "Error: Start date should be earlier than end date."))
    
    TREE_DATA$yvar <- TREE_DATA[[as.character(input$var)]]
    TREE_DATA %>%
      filter(
        Log == input$type, #!is.na(yvar),
        datetime0 > as.POSIXct(input$datetime0[1])& datetime0 < as.POSIXct(input$datetime0[2])
      )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_range()$datetime0, y = selected_range()$yvar, type = "l",
         xlab = "Date", ylab = "___VARIABLE NAME___", col = color, 
         fg = color, col.lab = color, col.axis = color)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)



#Insert NAs in areas where missing data exists
#look for function select multiple




# # Define UI
# ui <- fluidPage(
#   theme = shinytheme("lumen"),
#   titlePanel("Tree Title"),
#   sidebarLayout(
#     sidebarPanel(
#       # Select type of trend to plot
#       selectInput(
#         inputId = "type", label = strong("Select Log"),
#         choices = unique(TREE_DATA$Log),selected = "1"),
#       
#       # Select variable to plot
#       selectInput(
#         inputId = "var", label = strong("Select Variable"),
#         choices = colnames(TREE_DATA),selected = "air_temp"),
#       
#       # Select date range to be plotted
#       dateRangeInput(
#         "datetime0", strong("Date range"), min = "2020-07-02", max = "2020-10-27",
#         start = "2020-07-02 12:45:00 UTC", end = "2020-10-27 08:00:00 UTC"),
#       
#       # Select whether to overlay smooth trend line
#       checkboxInput(
#         inputId = "smoother", label = strong("checkboxInput label"), value = FALSE),
#       
#       # Display only if the smoother is checked
#       conditionalPanel(condition = "input.smoother == true",
#                        sliderInput(inputId = "f", label = "Smoother span:",
#                                    min = 0.01, max = 1, value = 0.67, step = 0.01,
#                                    animate = animationOptions(interval = 100)),
#                        HTML("Higher values give more smoothness.")
#       )
#     ),
#     # Output: Description, lineplot, and reference
#     mainPanel(
#       plotOutput(outputId = "lineplot", height = "300px")
#       #textOutput(outputId = "desc"),
#     )
#   )
# )
# 
# # Define server function
# server <- function(input, output) {
# 
#   # Subset data
#   selected_range <- reactive({
#     req(input$datetime0)
#     req(input$var)
#     validate(need(!is.na(input$datetime0[1]) & !is.na(input$datetime0[2]), "Error: Please provide both a start and an end date."))
#     validate(need(input$datetime0[1] < input$datetime0[2], "Error: Start date should be earlier than end date."))
#     TREE_DATA$yvar <- TREE_DATA[[as.character(input$var)]]
#     TREE_DATA %>%
#       filter(
#         Log == input$type,
#         !is.na(yvar),
#         datetime0 > as.POSIXct(input$datetime0[1]) & datetime0 < as.POSIXct(input$datetime0[2])
#         )
#   })
# 
#   # Create scatterplot object the plotOutput function is expecting
#   output$lineplot <- renderPlot({
#     color = "#434343"
#     par(mar = c(4, 4, 1, 1))
#     plot(x = selected_range()$datetime0, y = selected_range()$yvar, type = "l",
#          xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
#     # Display only if smoother is checked
#     if(input$smoother){
#       smooth_curve <- lowess(x = as.numeric(selected_range()$datetime0), y = selected_range()$yvar, f = input$f)
#       lines(smooth_curve, col = "#E6553A", lwd = 3)
#     }
#   })
# 
#   # # Pull in description of trend
#   # output$desc <- renderText({
#   #   trend_text <- filter(trend_description, type == input$type) %>% pull(text)
#   #   paste(trend_text, "I don't know how to do this yet")
#   # })
# }
# 
# # Create Shiny object
# shinyApp(ui = ui, server = server)
