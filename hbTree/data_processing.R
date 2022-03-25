library(tidyverse)
library(dplyr)
library(lubridate)

cleanup <- function(dataframe){
  #rename fields that start with a number...crashes a function later on
  names(dataframe)[names(dataframe) == "4m_VWC"] <- "VWC_4m"
  names(dataframe)[names(dataframe) == "2m_VWC"] <- "VWC_2m"
  names(dataframe)[names(dataframe) == "6m_VWC"] <- "VWC_6m"
  names(dataframe)[names(dataframe) == "6m_log_temp"] <- "log_temp_6m"
  names(dataframe)[names(dataframe) == "4m_log_temp"] <- "log_temp_4m"
  names(dataframe)[names(dataframe) == "2m_log_temp"] <- "log_temp_2m"
  
  #Create New Variables
  dataframe$Avg_log_vwc <- rowMeans(subset(dataframe,select = c('VWC_2m', 'VWC_4m','VWC_6m'), na.rm = TRUE))
  dataframe$Avg_log_temp <- rowMeans(subset(dataframe,select = c('log_temp_2m', 'log_temp_4m','log_temp_6m'), na.rm = TRUE))
  dataframe$up_matric <- dataframe$up_matric*(-1)
  dataframe$down_matric <- dataframe$down_matric*(-1)
  
  #More data cleaning to remove null VP and airtemp and log attributes
  dataframe <- subset(dataframe,VP != 'NA'|air_temp !='NA'| battery>0 | Avg_log_vwc !='NA')
  
  #Calculate vpd:   y1 = ... # T [deg C]    y2 = ... # RH
  dataframe$vpsat <- (6.112*exp((17.67*dataframe$air_temp)/(dataframe$air_temp+243.5)))*0.1 # [kPa]
  dataframe$vp <- dataframe$vpsat * (dataframe$RH/100)
  dataframe$vpd <- dataframe$vpsat - dataframe$vp
  
  # Take apart datetime stamp and replace midnight with one second past midnight
  #dataframe$datetime <- as.character(dataframe$date)
  #dataframe$date2 <- substr(dataframe$datetime,1,10)
  #dataframe$time2 <- substr(dataframe$datetime,12,19)
  #dataframe$time3 <- str_replace(dataframe$time2,'00:00:00','00:00:01')
  #dataframe$datetime2 <- paste(dataframe$date2,dataframe$time3,sep=" ")
  
  #dataframe$datetime0 <- ymd_hms(dataframe$datetime2) ####
  
  return(dataframe)
}

average_by_hour <- function(dataframe){
  original_dataframe <- dataframe
  vars_to_avg <- colnames(select_if(dataframe, is.numeric)) #get all numeric columns from dataframe that can be averaged
  dataframe <- mutate(dataframe, hour = as.POSIXct(substr(as.character(date), 1, 13), format="%Y-%m-%d %H")) #chop off minute and second, save to new column
  dataframe <- dataframe %>%
    group_by(hour) %>%
    summarise_at(vars(vars_to_avg), mean, na.rm=TRUE)
  dataframe <- mutate(dataframe, date = hour + hours(1)) #this is the START of the hour, adding an hour changes it to end of hour
  dataframe <- subset(dataframe, select = -c(hour)) #drop hour column
  return(dataframe)
}

setwd("~/R/S22-Tree-App/hbTree/treedata_unprocessed")
file_list = list.files(path="~/R/S22-Tree-App/hbTree/treedata_unprocessed")

all_logs <- data.frame()

for (i in 1:length(file_list)){
  if(file_list[i] == "Log 3 2020 summer.csv"){
    temp_data <- read_csv(file_list[i], col_types = cols(date = col_datetime(format = "%m/%d/%Y %H:%M")))
  } else {
    temp_data <- read_csv(file_list[i])
  }
  temp_data$file_source <- file_list[i]
  temp_data <- average_by_hour(temp_data)
  all_logs <- bind_rows(all_logs, temp_data)
}

write.csv(all_logs, "~/R/S22-Tree-App/hbTree/treedata/All_logs_unclean.csv")
write.csv(cleanup(all_logs), "~/R/S22-Tree-App/hbTree/treedata/All_logs.csv")