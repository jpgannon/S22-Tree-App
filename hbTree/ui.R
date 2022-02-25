# Made by Cole Jackson
# kelvin was here
# trey was also here

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)

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
        choices = unique(TREE_DATA$Log), selected = "1",
        multiple = TRUE),

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
        Log %in% input$type, #!is.na(yvar),
        datetime0 > as.POSIXct(input$datetime0[1])& datetime0 < as.POSIXct(input$datetime0[2])
      )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    ggplot(selected_range(), aes(x = datetime0, y = yvar, color = as.factor(Log)))+
      geom_line()+
      labs(title="Title\n",x="Date",y=as.character((input$var)),color="Log\n")+
      theme_bw() +
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))
    
    #color = "#434343"
    #par(mar = c(4, 4, 1, 1))
    # plot(x = selected_range()$datetime0, y = selected_range()$yvar, type = "l",
    #      xlab = "Date", ylab = "___VARIABLE NAME___", col = color, 
    #      fg = color, col.lab = color, col.axis = color)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)