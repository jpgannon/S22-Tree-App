# Load packages
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

# Load data
TREE_DATA <- read_csv("~/TreeApp/S22-Tree-App/hbTree/treedata/All_logs.csv")

treedata <- TREE_DATA %>%
  select(!c(date,...1,...2,datetime,date2,time2,time3,datetime2))%>%
  pivot_longer(cols = !c(datetime0,station,Log,Canopy,Block,SilvTreat,LogTreat),names_to = "name",values_to = 'value')

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
  tabsetPanel(
    
    tabPanel("Home Page",
       sidebarLayout(
         
         sidebarPanel(
           
           width = 1
         ),
         
         mainPanel(
           
           "Lorem ipsum"
         )
       ),
     titlePanel("Label A")
    ),

    tabPanel('Time Based Analysis',
       sidebarLayout(
         
         sidebarPanel(
           
           # Select which Log(s) to plot
           selectInput("logs", strong("Select Log(s)"),
                       choices = unique(treedata$Log), 
                       selected = unique(treedata$Log)[1], 
                       multiple = TRUE),
           
           # Select variable for plot1
           selectInput("var1", strong("Select Variable 1"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[2]),
           # Select variable for plot2
           selectInput("var2", strong("Select Variable 2"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[3]),
           
           # Select date range to be plotted
           dateRangeInput("date", strong("Date range"),  
                          min = "2020-07-02", max = "2020-10-27",
                          start = "2020-07-02 12:45:00 UTC", 
                          end = "2020-10-27 08:00:00 UTC")
         ),
         
         mainPanel(
           
           # Create brushing functionality 
           plotOutput("plot1", width="100%", height = "215px", dblclick = "plot1_click",
                      brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
           plotOutput("plot2", width="100%", height = "215px", dblclick = "plot2_click",
                      brush = brushOpts(id = "plot2_brush", resetOnNew = TRUE))
         )
       )
    ),
    # Create Multivariable Analysis tab
    tabPanel('Multivariable Analysis',
       sidebarLayout(
         
         sidebarPanel(
           
         ),
         
         mainPanel(
           
         )
       )
    )
  )
)


# Define server function
server <- function(input, output) {
  
  daterange <- reactiveValues(x = ymd(c("2020-07-02", "2020-10-27")))
  
  output$plot1 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var1 & daterange$x[1] < datetime0 & datetime0 < daterange$x[2]) %>%
      ggplot(aes(x = datetime0, y = value, color = as.factor(Log))) +
      geom_line()+
      theme_bw()+
      labs(title="Title", x="Date", y=as.character((input$var1)), color="Log\n")+
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold", color = "black"))
    
  })
  
  output$plot2 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var2 & daterange$x[1] < datetime0 & datetime0 < daterange$x[2]) %>%
      ggplot(aes(x = datetime0, y = value, color = as.factor(Log))) +
      geom_line()+
      theme_bw()+
      labs(title="Title", x="Date", y=as.character((input$var2)), color="Log\n")+
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold", color = "black"))
    
  })
  
  observeEvent(input$date[1], {daterange$x[1] <- ymd(input$date[1])})
  observeEvent(input$date[2], {daterange$x[2] <- ymd(input$date[2])})
  
  observeEvent(input$plot1_click, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      daterange$x <- c(brush$xmin, brush$xmax)
    } 
    else {
      daterange$x <- c(ymd(input$date[1], input$date[2]))
    }
  })
  
  observeEvent(input$plot2_click, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      daterange$x <- c(brush$xmin, brush$xmax)
    } 
    else {
      daterange$x <- c(ymd(input$date[1], input$date[2]))
    }
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
