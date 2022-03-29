library(tidyverse)
library(dplyr)
library(lubridate)


create_dataset <- function(folder_location) {
  all_logs <- data.frame()
  file_list = list.files(path=folder_location)
  
  for (i in 1:length(file_list)){
    if(file_list[i] == "Log 3 2020 summer.csv"){
      temp_data <- read_csv(file_list[i], col_types = cols(date = col_datetime(format = "%m/%d/%Y %H:%M")))
    } else {
      temp_data <- read_csv(file_list[i])
    }
    temp_data <- average_by_hour(temp_data)
    temp_data$fileSource <- file_list[i]
    all_logs <- bind_rows(all_logs, temp_data)
  }
  return(all_logs)
}

average_by_hour <- function(dataframe){
  dat <- dataframe
  ivs <- c(dat$Log[1], dat$station[1], dat$Canopy[1], dat$SilvTreat[1], dat$LogTreat[1])
  dat <- dat %>%
    mutate(hour = as.POSIXct(substr(as.character(date), 1, 13), format="%Y-%m-%d %H")) #chop off minute and second, save to new column
  dat <- dat %>%
    group_by(hour) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) 
  dat <- dat %>%
    mutate(date = hour + hours(1), #this is the START of the hour, adding an hour changes it to end of hour
           station = ivs[2],
           Canopy = ivs[3],
           SilvTreat = ivs[4],
           LogTreat = ivs[5]) %>%
    select(-c(hour))
  return(dat)
}

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
  
  return(dataframe)
}




folder_location = "~/TreeApp/S22-Tree-App/hbTree/treedata_unprocessed"

setwd(folder_location)

TREE_DATA <- create_dataset(folder_location=folder_location)

write.csv(TREE_DATA, "~/TreeApp/S22-Tree-App/hbTree/treedata/All_logs_unclean.csv")

write.csv(cleanup(TREE_DATA), "~/TreeApp/S22-Tree-App/hbTree/treedata/All_logs.csv")


