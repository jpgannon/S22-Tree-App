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
  # Gets the time-lag between subsequent observations.
  # Creates copies of any observations with time-lag over an hour
  # Change any numeric values to NA's for copies, excluding date
  # Shifts the date value of copies by an hour
  # Adds NA filled, time shifted, copies to original dataframe
  
  dataframe$date <- dataframe[[dateVarName]]
  # Select a single sensor and sort date ascending
  dataframe <- dataframe %>%
    filter(Log == sensorNumber) %>%
    arrange(date)
  time <- dataframe %>% select(date)
  # Remove first row, duplicate final row and add an (hour) to it
  time <- rbind(time[-1,], time[nrow(time),] + recordInterval)
  # Get the time lag for subsequent observations
  dataframe$Time_Lag <- as.double(time$date - dataframe$date)
  # Select any rows with a time lag of more than an (hour)
  blanks <- dataframe %>%
    filter(Time_Lag > (recordInterval/3600)) %>%
    select(date, station, Log, Canopy, Block, SilvTreat, LogTreat, Time_Lag)
  # Create blank rows with date shifted by an (hour)
  blanks$date <- blanks$date + recordInterval
  # Join the blank time shifted rows to dataframe
  new_df <- dataframe %>%
    full_join(blanks,) %>%
    select(-c(Time_Lag))

  return(new_df)
}


insertNA_at_timejumps_all_logs <- function(dataframe, sensors, recordInterval, dateVarName) {
  # For each sensor in dataframe run insertNA_at_timejumps
  # The purpose of this function is to remove any 'bridges' 
  #   between disjointed portions of data when line plotting
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


get_daterange <- function(dataframe, perlog = FALSE) {
  # Get earliest and latest date in general (Default)
  # Get earliest and latest date shared in common by all logs
  if (perlog == FALSE) {
    dat <- dataframe %>%
      select(date) %>%
      summarise_at(vars(date), list(min=min, max=max))
    return(c(date(ymd_hms(dat$min)), date(ymd_hms(dat$max))) )
  } else {
    dat <- dataframe %>%
      group_by(Log) %>%
      select(Log, date) %>%
      summarise_at(vars(date), list(min=min, max=max))
    earliest <- dat %>%
      summarise_at(vars(min), list(date=max))
    earliest_date <- ymd_hms(earliest$date)
    latest <- dat %>%
      summarise_at(vars(max), list(date=min))
    latest_date <- ymd_hms(latest$date)
    return(c(date(earliest_date), date(latest_date)))
  }
  
}


clean_up_names <- function(dataframe, acronym_dict) {
  # For each variable name in dataframe make the following changes:
  #   Replace underscores with spaces
  #   Capitalize first letter in each word
  # Note: Does not mess with any existing capitalization
  new_names <- c()
  for (name in names(dataframe)) {
    new_name <- ''
    for (word in strsplit(name,'_')[[1]]) {
      new_word <- paste(toupper(substr(word,1,1)), substr(word,2,100), sep = '')
      new_name <- paste(new_name, new_word)
      new_name <- trimws(new_name)
      
      for (i in c(1:length(acronym_dict$Acronym))) {
        new_name <- sub(acronym_dict$Acronym[i], acronym_dict$Meaning[i], new_name)
      }
    }
    new_names <- c(new_names, new_name)
  }
  setnames(dataframe, old = names(dataframe), new = new_names)
  
  return(dataframe)
}


pivot_clean <- function(dataframe) {
  new_df <- dataframe %>%
    select(-c(...1, ...2, FileSource, Station)) %>% 
    pivot_longer(cols = !c(Date, Log, Canopy, Block, SilvTreat, LogTreat),
                 names_to = "name",values_to = 'value')
  return(new_df)
}


TREE_DATA <- read_csv("treedata/All_logs.csv")
TREE_DATA <- insertNA_at_timejumps_all_logs(dataframe = TREE_DATA, sensors = 12,
                                            recordInterval = 3600, dateVarName = 'date')
date_range <- get_daterange(TREE_DATA)
MIN_DATE <- ymd(date_range[1])
MAX_DATE <- ymd(date_range[2])

acronym_dict <- read_csv("treedata/Acronyms.csv")
TREE_DATA <- clean_up_names(TREE_DATA, acronym_dict)
treedata <- pivot_clean(TREE_DATA)

TREE_DATA$Log <- as.factor(TREE_DATA$Log)
treedata$Log <- as.factor(treedata$Log)


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

    #Create Time Based Analysis Tab
    tabPanel('Time Based Analysis',
       titlePanel("Time Based Analysis"),
       sidebarLayout(
         
         sidebarPanel(
           
           # Select which Log(s) to plot
           selectInput("group", strong("Color Points By:"), choices = c('Log','Canopy','SilvTreat','LogTreat'), selected = 'Canopy'),
           
           selectInput("subgroup1", strong("Select group(s)"), "", multiple = TRUE),
           
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
                          start = MIN_DATE, end = MAX_DATE),
           
           actionButton('resetdate',strong('Reset Date')),
           
           # Select opacity for plot
           sliderInput("slider1",strong("Opacity"),
                       min = 0, max = 1, value = 0.5)
         ),
         
         mainPanel(
           
           # Create brushing functionality 
           plotOutput("plot1", width="100%", height = "300px", dblclick = "plot1_click",
                      brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
           plotOutput("plot2", width="100%", height = "300px", dblclick = "plot2_click",
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
           
           selectInput("group2", strong("Color Points By:"), choices = c('Log','Canopy','SilvTreat','LogTreat')),
           
           selectInput("subgroup2", strong("Select group(s)"), "", multiple = TRUE),
           
           # Select variable 1 for plot
           selectInput("multi1", strong("Select X Axis"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[2]),
           # Select variable 2 for plot
           selectInput("multi2", strong("Select Y Axis"),
                       choices = unique(treedata$name), 
                       selected = unique(treedata$name)[3]),
           # Select opacity of points
           sliderInput("slider",strong("Opacity"),
                       min = 0, max = 1, value = 0.5)
           
         ),
         
         mainPanel(
           
           # Create brushing functionality 
           plotOutput("plotm", width="600px", height = "600px")
         )
       )
    ),
    
    # Create Summary Analysis tab
    tabPanel('Summary Analysis',
             titlePanel("Summary Analysis"),
             sidebarLayout(
               
               sidebarPanel(
                 
                 # Select which Log(s) to plot
                 selectInput("group3", strong("Color Points By:"), choices = c('Log','Canopy','SilvTreat','LogTreat'), selected = 'Canopy'),
                 
                 selectInput("subgroup3", strong("Select group(s)"), "", multiple = TRUE),
                 
                 # Select variable for plotsummary
                 selectInput("var3", strong("Select Variable"),
                             choices = unique(treedata$name), 
                             selected = unique(treedata$name)[2])
                 
               ),
               
               mainPanel(
                 plotOutput("plotsummary")
               )
             )
        )
  )
)


# Define server function
server <- function(input, output) {
  
  #Home Page
  
  #Write introduction and how to use text
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
        For time based analysis first select the group variable from the drop down then select which log(s) you want to graph then select variable 1 for the top graph and variable 2 for the bottom graph finally specify the date range you want to be graphed. If you want to zoom into a specific range within the graph you are able to by clicking and dragging to the desired range and double clicking, you will zoom into the desired date range.
        For multivariable analysis you just select the log(s) you want to graph and the variables you want on the x and y axis. For the summary analysis, select the desired catagories, and you will be able to view and compare boxplots.
        <br/>
        <br/>
        <br/>
        <br/>
        Created by Trey Valenta, Kelvin Rivas, Cole Jackson, and Augene Lee', 
      sep="<br/>"))
  })
  
  #Time Based Analysis Page 1
  
  #create daterange for plot1 and plot2
  daterange <- reactiveValues(x = ymd(c(MIN_DATE, MAX_DATE)))
  
  #create plot 1 of timeseries graph
  output$plot1 <- renderPlot({
    
    if (as.character(input$group) == 'Log') {
      
      subdat <- treedata %>% 
        rename('group' = as.character(input$group)) %>%
        filter(group %in% input$subgroup1 & name %in% input$var1 & 
                 daterange$x[1] < Date & Date < daterange$x[2])
      
      subdat %>%
        ggplot(mapping = aes(x = Date, y = value, color = group)) +
        geom_line() + theme_bw() +
        labs(x = '', y = input$var1, color = input$group) +
        theme(axis.text.y = element_text(angle = 90, hjust=0.5, size = 14), 
              axis.title.y = element_text(size = 14), legend.position="right")
    } else {
      
      subdat <- treedata %>% 
        rename('group' = as.character(input$group)) %>%
        mutate(combined_variables = paste(group,Log,sep = '-')) %>%
        filter(group %in% input$subgroup1 & name %in% input$var1 & 
                 daterange$x[1] < Date & Date < daterange$x[2])
      
      subdat %>%
        ggplot(mapping = aes(x = Date, y = value, color = factor(combined_variables))) +
        geom_line(aes(linetype = group)) + theme_bw() +
        labs(x = '', y = input$var1, color = input$group) +
        theme(axis.text.y = element_text(angle = 90, hjust=0.5, size = 14), 
              axis.title.y = element_text(size = 14), legend.position="right")
    }

  })
  
  #create plot2 of timeseries graph
  output$plot2 <- renderPlot({
    
    if (as.character(input$group) == 'Log') {
      
      subdat <- treedata %>% 
        rename('group' = as.character(input$group)) %>%
        filter(group %in% input$subgroup1 & name %in% input$var2 & 
                 daterange$x[1] < Date & Date < daterange$x[2])
      
      subdat %>% 
        ggplot(mapping = aes(x = Date, y = value, color = group)) +
        geom_line() + theme_bw() +
        labs(x = '', y = input$var2, color = input$group) +
        theme(axis.text.y = element_text(angle = 90, hjust=0.5, size = 14), 
              axis.title.y = element_text(size = 14), legend.position="right")
    } else {
      
      subdat <- treedata %>% 
        rename('group' = as.character(input$group)) %>%
        mutate(combined_variables = paste(group,Log,sep = '-')) %>%
        filter(group %in% input$subgroup1 & name %in% input$var2 & 
                 daterange$x[1] < Date & Date < daterange$x[2])
      
      subdat %>% 
        ggplot(mapping = aes(x = Date, y = value, color = factor(combined_variables))) +
        geom_line(aes(linetype = group), size = 0.6) + theme_bw() +
        scale_linetype_manual(values=c("solid", "longdash", "dotted", "dotdash","77")) +
        labs(x = '', y = input$var2, color = input$group) +
        theme(axis.text.y = element_text(angle = 90, hjust=0.5, size = 14), 
              axis.title.y = element_text(size = 14), legend.position="right")
    }
  
  })
  
  #Multivariable Analysis Page 2
  
  #create multivariable plot
  output$plotm <- renderPlot({
    TREE_DATA %>% 
      filter(!!rlang::sym(as.character(input$group2)) %in% input$subgroup2) %>%
      ggplot(aes_string(x = as.character(add.backtick(input$multi1)), 
                        y = as.character(add.backtick(input$multi2)), 
                        color = as.character(input$group2))) +
      geom_point(alpha = input$slider) + theme_bw() +
      labs(title = "Multivariable Relationship", 
           x = as.character((input$multi1)), 
           y = as.character((input$multi2)), 
           color = as.character(input$group2)) +
      theme(legend.position="bottom",
            axis.text.x = element_text(size = 14), 
            axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), 
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, 
                                      face = "bold", 
                                      color = "black"))
    
  })
  
  #Summary Analysis Page 3 - Boxplots
  
  #create summary  boxplot
  output$plotsummary <- renderPlot({
    TREE_DATA %>%
      filter(!!rlang::sym(as.character(input$group3)) %in% input$subgroup3) %>%
      ggplot(aes_string(x = as.character(add.backtick(input$group3)),
                        y = as.character(add.backtick(input$var3)), 
                        fill = as.character(input$group3))) +
      geom_boxplot() + 
      theme_bw() +
      labs(title = "Summary", 
           color = as.character(input$group3)) +
      theme(legend.position="bottom",
            axis.text.x = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18,
                                      face = "bold",
                                      color = "black"))
  })
  
  # Page 1
  
  observeEvent(input$date[1], {daterange$x[1] <- ymd(input$date[1])})
  
  observeEvent(input$date[2], {daterange$x[2] <- ymd(input$date[2])})
  
  observeEvent(input$resetdate, { # "RESET DATERANGE"
    daterange$x <- c(as_datetime(MIN_DATE), 
                     as_datetime(MAX_DATE))
    updateDateRangeInput(inputId = 'date', 
      start = as_datetime(daterange$x[1]), 
      end = as_datetime(daterange$x[2]), 
      min = MIN_DATE, max = MAX_DATE)
    })
  
  #Create brush tool
  observeEvent(input$plot1_click, { # "BRUSH PLOT 1"
    brush <- input$plot1_brush
    daterange$x <- c(as_datetime(input$date[1]), 
                     as_datetime(input$date[2]))
    if (!is.null(brush)) {
      daterange$x <- c(as_datetime(brush$xmin), 
                       as_datetime(brush$xmax))
      updateDateRangeInput(inputId = 'date',
        start = as_datetime(daterange$x[1]), 
        end = as_datetime(daterange$x[2]), 
        min = MIN_DATE, max = MAX_DATE)}
  })
  
  observeEvent(input$plot2_click, { # "BRUSH PLOT 2"
    brush <- input$plot2_brush
    daterange$x <- c(as_datetime(input$date[1]), 
                     as_datetime(input$date[2]))
    if (!is.null(brush)) {
      daterange$x <- c(as_datetime(brush$xmin), 
                       as_datetime(brush$xmax))
      updateDateRangeInput(inputId = 'date', 
        start = as_datetime(daterange$x[1]), 
        end = as_datetime(daterange$x[2]), 
        min = MIN_DATE, max = MAX_DATE)}
  })
  
  
  getsubgroup1 = reactive({
    myvar = input$group
    unique(treedata[[myvar]])
  })
  
  observe({ # "UPDATE GROUPS PAGE 1"
    updateSelectInput(
      inputId = 'subgroup1', 
      choices = getsubgroup1())
  })
  
  # Page 2
  
  getsubgroup2 = reactive({
    myvar2 = input$group2
    unique(treedata[[myvar2]])
  })
  
  observe({ # "UPDATE GROUPS PAGE 2"
    updateSelectInput(
      inputId = 'subgroup2', 
      choices = getsubgroup2())
  })
  
  #Page 3
  
  getsubgroup3 = reactive({
    myvar3 = input$group3
    unique(treedata[[myvar3]])
  })
  
  observe({ # "UPDATE GROUPS PAGE 3"
    updateSelectInput(
      inputId = 'subgroup3',
      choices = getsubgroup3())
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server, options = list(height = 800))
