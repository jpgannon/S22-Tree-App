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

insertNA_at_timejumps <- function(dataframe, sensorNumber, recordInterval, dateVarName) {
  dataframe$date <- dataframe[[dateVarName]]
  dataframe <- dataframe %>%
    filter(Log == sensorNumber) %>%
    arrange(date)

  time <- dataframe %>% select(date)
  time <- rbind(time[-1,], time[nrow(time),] + recordInterval)
  dataframe$Time_Lag <- as.double(time$date - dataframe$date)

  blanks <- dataframe %>%
    filter(Time_Lag > (recordInterval/3600)) %>%
    select(date, station, Log, Canopy, Block, SilvTreat, LogTreat, Time_Lag)
  blanks$date <- blanks$date + recordInterval

  new_df <- dataframe %>%
    full_join(blanks,) %>%
    select(-c(Time_Lag))

  return(new_df)
}

insertNA_at_timejumps_all_logs <- function(dataframe, sensors, recordInterval, dateVarName) {
  new_df = data.frame()
  for (sensorNumber in 1:sensors) {
    output_df <-
      insertNA_at_timejumps(
        dataframe = dataframe,
        sensorNumber = sensorNumber,
        recordInterval = recordInterval,
        dateVarName = dateVarName)
    new_df <-
      rbind(new_df, output_df)
  }
  return(new_df)
}

clean_up_names <- function(dataframe) {
  new_names <- c()
  for (name in names(dataframe)) {
    new_name <- ''
    for (word in strsplit(name,'_')[[1]]) {
      new_word <- paste(toupper(substr(word,1,1)), substr(word,2,100), sep = '')
      new_name <- paste(new_name, new_word)
      new_name <- trimws(new_name)
    }
    new_names <- c(new_names, new_name)
  }
  setnames(dataframe, old = names(dataframe), new = new_names)
  return(dataframe)
}

pivot_clean <- function(dataframe) {
  new_df <- dataframe %>%
    select(-c(...1, ...2, FileSource)) %>% 
    pivot_longer(cols = !c(Date, Station, Log, Canopy, Block, SilvTreat, LogTreat),
                 names_to = "name",values_to = 'value')
  return(new_df)
}

TREE_DATA <- read_csv("treedata/All_logs.csv")

TREE_DATA <- insertNA_at_timejumps_all_logs(dataframe = TREE_DATA, sensors = 12,
                                            recordInterval = 3600, dateVarName = 'date')
TREE_DATA <- clean_up_names(TREE_DATA)

treedata <- pivot_clean(TREE_DATA)

TREE_DATA$Log <- as.factor(TREE_DATA$Log)

treedata$Log <- as.factor(treedata$Log)

MIN_DATE = ymd("2020-07-02")

MAX_DATE = ymd("2020-10-27")

print('Starting Up App')


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
  tabsetPanel(
    
    tabPanel("Home Page",
         titlePanel("Microclimate Visualization within an Adaptive Silviculture Experiment in New England"),
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
           selectInput("independent", strong("Select Independent Variable"), choices = c('Log','Canopy','SilvTreat','LogTreat','Station')),
           
           #selectInput("groups", strong("Select Group(s)"), "", multiple = TRUE),
           
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
                          min = MIN_DATE, max = MAX_DATE,
                          start = "2020-07-02 12:45:00 UTC", 
                          end = "2020-10-27 08:00:00 UTC")
         ),
         
         mainPanel(
           
           # Create brushing functionality 
           plotOutput("plot1", width="100%", height = "215px", dblclick = "plot1_click",
                      brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
           plotOutput("plot2", width="100%", height = "215px", dblclick = "plot2_click",
                      brush = brushOpts(id = "plot2_brush", resetOnNew = TRUE)),
           h4('Drag Across Plot, then Double Click to Zoom In on Section')
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
           plotOutput("plotm", width="100%", height = "430px"),
           h4('Drag Across Plot, then Double Click to Zoom In Section')
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
      '',
      'Silviculture is the practice of applied forest ecology, where vegetation is manipulated and designed to suit the landowner’s objectives across space and time. These designs are planned with the context of different forest stressors, such as insects and disease, winter losses, urbanization, and poor management. The main goal of all silviculture practices is to increase the overall resiliency of the forest.
        <br/>
        <br/>
        An experiment was conducted in New England where forest and other natural resource managers integrated different types of climate changes into silviculture planning. The main goal of this experiment was to populate a multi-region study with ecosystem-specific climate change and forest health adaptation treatments. Key variables from 4 different adaptation treatments across 3-5 forest types were measured and analyzed.
        <br/>
        <br/>
        This application is the data visualization of the microclimates within the adaptive silviculture experiment in New England. A time-based and multivariate analysis is displayed for the user.
        <br/>
        <br/>
        <br/>
        How to use:
        <br/>
        <br/>
        For time based analysis first select the independent variable from the drop down then select which log(s) you want to graph then select variable 1 for the top graph and variable 2 for the bottom graph finally specify the date range you want to be graphed. If you want to zoom into a specific range within the graph you are able to by clicking and dragging to the desired range and double clicking, you will zoom into the desired date range.
        For multivariable analysis you just select the log(s) you want to graph and the variables you want on the x and y axis.', 
      sep="<br/>"))
  })
  
  daterange <- reactiveValues(x = ymd(c("2020-07-02", "2020-10-27")))
  
  output$plot1 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var1 & daterange$x[1] < Date & Date < daterange$x[2]) %>%
      ggplot(aes_string(x = 'Date', y = 'value', color = as.character(input$independent))) +
      geom_point()+
      theme_bw()+
      labs(y=as.character((input$var1)), color=as.character(input$independent))+
      theme(axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
    
  })
  
  output$plot2 <- renderPlot({
    treedata %>% filter(Log %in% input$logs & name %in% input$var2 & daterange$x[1] < Date & Date < daterange$x[2]) %>%
      ggplot(aes(x = Date, y = value, color = as.factor(Log))) +
      geom_line()+
      theme_bw()+
      labs(y=as.character((input$var2)), color="Log\n")+
      theme(axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
    
  })
  
  output$plotm <- renderPlot({
    TREE_DATA %>% filter(Log %in% input$multilogs) %>%
      ggplot(aes_string(x = as.character(add.backtick(input$multi1)), y = as.character(add.backtick(input$multi2)), color = quote(Log))) +
      geom_point()+
      theme_bw()+
      labs(title="Multivariable Relationship", x=as.character((input$multi1)), y=as.character((input$multi2)), color="Log\n")+
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold", color = "black"))
    
  })
  
  # outvar = reactive({
  #   myvar = input$independent
  #   unique(treedata[[myvar]])
  # })
  # 
  # observe({
  #   updateSelectInput(inputId = 'groups', choices = outvar())
  # })
  
  observeEvent(input$date[1], {daterange$x[1] <- ymd(input$date[1])})
  observeEvent(input$date[2], {daterange$x[2] <- ymd(input$date[2])})
  
  observeEvent(input$plot1_click, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      daterange$x <- c(as_datetime(brush$xmin), as_datetime(brush$xmax))
      updateDateRangeInput(inputId = 'date', start = as_datetime(daterange$x[1]), end = as_datetime(daterange$x[2]), min = MIN_DATE, max = MAX_DATE)
    } 
    else {
      daterange$x <- c(ymd(input$date[1], input$date[2]))
    }
  })
  
  observeEvent(input$plot2_click, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      daterange$x <- c(as_datetime(brush$xmin), as_datetime(brush$xmax))
      updateDateRangeInput(inputId = 'date', start = as_datetime(daterange$x[1]), end = as_datetime(daterange$x[2]), min = MIN_DATE, max = MAX_DATE)
    } 
    else {
      daterange$x <- c(ymd(input$date[1], input$date[2]))
    }
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server, options = list(height = 800))
