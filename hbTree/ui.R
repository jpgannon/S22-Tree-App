# Load packages
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(formulaic)

# Load data
TREE_DATA <- read_csv("~/TreeApp/S22-Tree-App/hbTree/treedata/All_logs.csv")

new_names <- c()
for (name in names(TREE_DATA)) {
  new_name <- ''
  for (word in strsplit(name,'_')[[1]]) {
    new_word <- paste(toupper(substr(word,1,1)), substr(word,2,100), sep = '')
    new_name <- paste(new_name, new_word)
    if (word == 'vwc') {
      new_name <- 'VWC'
    }
    new_name <- trimws(new_name)
  }
  new_names <- c(new_names, new_name)
}
setnames(TREE_DATA, old = names(TREE_DATA), new = new_names)

treedata <- TREE_DATA %>%
  select(!c(Date,...1,...2,Datetime,Date2,Time2,Time3,Datetime2))%>%
  pivot_longer(cols = !c(Datetime0,Station,Log,Canopy,Block,SilvTreat,LogTreat),
               names_to = "name",values_to = 'value')

TREE_DATA$Log <- as.factor(TREE_DATA$Log)
treedata <- treedata %>% filter(!is.na(value))

print('Done')

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
  tabsetPanel(
    
    tabPanel("Home Page",
         titlePanel("Tree App Title"),
         fluidRow(
           column(10, 
              mainPanel(width=12,
                htmlOutput("text")
              )
            )
         )
     ),

    tabPanel('Time Based Analysis',
       titlePanel("Time Based Analysis"),
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
       titlePanel("Multivariable Analysis"),
       sidebarLayout(
         
         sidebarPanel(
           
           # Select which Log(s) to plot
           selectInput("multilogs", strong("Select Log(s)"),
                       choices = unique(treedata$Log), 
                       selected = unique(treedata$Log)[1],
                       multiple = TRUE),
           
           # Select variable for plot1
           selectInput("multi1", strong("Select X Axis"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[2]),
           # Select variable for plot2
           selectInput("multi2", strong("Select Y Axis"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[3])
         ),
         
         mainPanel(
           
           # Create brushing functionality 
           plotOutput("plotm", width="100%", height = "430px")
         )
       )
    )
  )
)


# Define server function
server <- function(input, output) {
  
  output$text <- renderText({
    HTML(paste(
      '',
      'Welcome',
      '',
      'Lorem ipsum dolor sit amet, zril legere ei cum. At vis electram imperdiet hendrerit, cu diam liber electram usu, ei ius nobis civibus legendos. Ne impetus argumentum mei, pri dissentiet philosophia definitionem eu. Augue primis tacimates in sed, ut mollis maiorum mea. Appareat neglegentur mel an, vitae libris equidem ius in, brute nostrud forensibus mel ne.', 
      sep="<br/>"))
  })
  
  daterange <- reactiveValues(x = ymd(c("2020-07-02", "2020-10-27")))
  
  output$plot1 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var1 & daterange$x[1] < Datetime0 & Datetime0 < daterange$x[2]) %>%
      ggplot(aes(x = Datetime0, y = value, color = as.factor(Log))) +
      geom_line()+
      theme_bw()+
      labs(title="Title", x="Date", y=as.character((input$var1)), color="Log\n")+
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold", color = "black"))
    
  })
  
  output$plot2 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var2 & daterange$x[1] < Datetime0 & Datetime0 < daterange$x[2]) %>%
      ggplot(aes(x = Datetime0, y = value, color = as.factor(Log))) +
      geom_line()+
      theme_bw()+
      labs(title="Title", x="Date", y=as.character((input$var2)), color="Log\n")+
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold", color = "black"))
    
  })
  
  output$plotm <- renderPlot({
    TREE_DATA %>% filter(Log %in% input$multilogs) %>%
      ggplot(aes_string(x = as.character(add.backtick(input$multi1)), y = as.character(add.backtick(input$multi2)), color = quote(Log))) +
      geom_point()+
      theme_bw()+
      labs(title="Title", x=as.character((input$multi1)), y=as.character((input$multi2)), color="Log\n")+
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
shinyApp(ui = ui, server = server, options = list(height = 800))
