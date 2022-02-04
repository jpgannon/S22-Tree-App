# 2020 SOFL field data +
# NOAA Met Data from Pittsburg Reservoir


##################################
# Add libraries
#################################

#install.packages("hydroTSM")

library(tidyr)
library(dplyr)
library(plyr)
library(stringi)
library(gtools)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(forcats)
library(viridis)
library(ggridges)
library(ggthemes)
library(ggpubr)
library(BSDA)
library(lubridate)
library(hydroTSM)
library(wesanderson)

##################################
#set working directory where core file is kept

setwd('C:/Aworkspace/Frankenlog/Dec_2020_Data_Analysis/')


#bring all data together
Log1 <- read.csv("Log 1 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log1$'2m_VWC' <- as.numeric(Log1$'2m_VWC')
Log2 <- read.csv("Log 2 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log3 <- read.csv("Log 3 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log4 <- read.csv("Log 4 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log5 <- read.csv("Log 5 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log6 <- read.csv("Log 6 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log7 <- read.csv("Log 7 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log8 <- read.csv("Log 8 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log9 <- read.csv("Log 9 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log10 <- read.csv("Log 10 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log11 <- read.csv("Log 11 2020 Summer.csv", header = TRUE, check.names = FALSE)
Log12 <- read.csv("Log 12 2020 Summer.csv", header = TRUE, check.names = FALSE)
All_logs <- bind_rows(Log1, Log2, Log3, Log4, Log5, Log6, Log7, Log8, Log9, Log10, Log11, Log12)

#rename fields that start with a number...crashes a function later on
names(All_logs)[names(All_logs) == "4m_VWC"] <- "VWC_4m"
names(All_logs)[names(All_logs) == "2m_VWC"] <- "VWC_2m"
names(All_logs)[names(All_logs) == "6m_VWC"] <- "VWC_6m"
names(All_logs)[names(All_logs) == "6m_log_temp"] <- "log_temp_6m"
names(All_logs)[names(All_logs) == "4m_log_temp"] <- "log_temp_4m"
names(All_logs)[names(All_logs) == "2m_log_temp"] <- "log_temp_2m"

#Create New Variables
All_logs$Avg_log_vwc <- rowMeans(subset(All_logs,select = c('VWC_2m', 'VWC_4m','VWC_6m'), na.rm = TRUE))
All_logs$Avg_log_temp <- rowMeans(subset(All_logs,select = c('log_temp_2m', 'log_temp_4m','log_temp_6m'), na.rm = TRUE))
All_logs$up_matric <- All_logs$up_matric*(-1)
All_logs$down_matric <- All_logs$down_matric*(-1)

#More data cleaning to remove null VP and airtemp and log attributes
All_logs <- subset(All_logs,VP != 'NA'|air_temp !='NA'| battery>0 | Avg_log_vwc !='NA')

#Calculate vpd:   y1 = ... # T [deg C]    y2 = ... # RH
All_logs$vpsat <- (6.112*exp((17.67*All_logs$air_temp)/(All_logs$air_temp+243.5)))*0.1 # [kPa]
All_logs$vp <- All_logs$vpsat * (All_logs$RH/100)
All_logs$vpd <- All_logs$vpsat - All_logs$vp

# Take apart datetime stamp and replace midnight with one second past midnight
All_logs$datetime <- as.character(All_logs$date)
All_logs$date2 <- substr(All_logs$datetime,1,10)
All_logs$time2 <- substr(All_logs$datetime,12,19)
All_logs$time3 <- str_replace(All_logs$time2,'00:00:00','00:00:01')
All_logs$datetime2 <- paste(All_logs$date2,All_logs$time3,sep=" ")

###############################################
# Pittsburg Reservoir NOAA 15-min data ########
###############################################
Pitts_clim <- read.csv("Pittsburg_Reservoir_NOAA_15min_met_trunc.csv")
Pitts_clim2 <- select(Pitts_clim,DATE,X0000Val,X0015Val,X0030Val,X0045Val,X0100Val,X0115Val,X0130Val,X0145Val,X0200Val,
                      X0215Val,X0230Val,X0245Val,X0300Val,X0315Val,X0330Val,X0345Val,X0400Val,
                      X0415Val,X0430Val,X0445Val,X0500Val,X0515Val,X0530Val,X0545Val,X0600Val,
                      X0615Val,X0630Val,X0645Val,X0700Val,X0715Val,X0730Val,X0745Val,X0800Val,
                      X0815Val,X0830Val,X0845Val,X0900Val,X0915Val,X0930Val,X0945Val,X1000Val,
                      X1015Val,X1030Val,X1045Val,X1100Val,X1115Val,X1130Val,X1145Val,X1200Val,
                      X1215Val,X1230Val,X1245Val,X1300Val,X1315Val,X1330Val,X1345Val,X1400Val,
                      X1415Val,X1430Val,X1445Val,X1500Val,X1515Val,X1530Val,X1545Val,X1600Val,
                      X1615Val,X1630Val,X1645Val,X1700Val,X1715Val,X1730Val,X1745Val,X1800Val,
                      X1815Val,X1830Val,X1845Val,X1900Val,X1915Val,X1930Val,X1945Val,X2000Val,
                      X2015Val,X2030Val,X2045Val,X2100Val,X2115Val,X2130Val,X2145Val,X2200Val,
                      X2215Val,X2230Val,X2245Val,X2300Val,X2315Val,X2330Val,X2345Val)

#Convert NOAA dates
Pitts_clim2$DATE <- as.character(Pitts_clim2$DATE) 
Pitts_clim2$DATE2 <- as.Date(Pitts_clim2$DATE,"%m/%d/%Y")
Pitts_clim2$DATE2 <-as.character(Pitts_clim2$DATE2)

#Wide to long format to create Date/Time Identifier to Link with Log Data
Pitts_clim2_long <- gather(Pitts_clim2,key="time",value="precip",X0000Val:X2345Val)
Pitts_clim2_long$timehr <-str_sub(Pitts_clim2_long$time,2,3)
Pitts_clim2_long$timemin <-str_sub(Pitts_clim2_long$time,4,5) 
Pitts_clim2_long$timesec <- "00"
Pitts_clim2_long$time2 <- str_c(Pitts_clim2_long$timehr,Pitts_clim2_long$timemin,Pitts_clim2_long$timesec,sep=':')
Pitts_clim2_long$time3 <- str_replace(Pitts_clim2_long$time2,'00:00:00','00:00:01')

#Combine date and time character and convert to date/time type
#https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r
Pitts_clim2_long$datetime2 <- paste(Pitts_clim2_long$DATE2,Pitts_clim2_long$time3,sep=" ")
Pitts_clim2_long$datetime3 <- ymd_hms(Pitts_clim2_long$datetime2)
Pitts_clim2_long$precip[Pitts_clim2_long$precip==-9999] <- NA
Pitts_clim2_long$precip_mm <- (Pitts_clim2_long$precip*0.254)  #one hundredth of an inch is 0.254 mm


#Join SOFL and NOAA met data and remove superflous date/time variables
sofl_precip <- inner_join(All_logs,Pitts_clim2_long,by="datetime2")
sofl_precip2 <- subset(sofl_precip,select=-c(date,datetime,date2,time2.x,time3.x,datetime2,DATE,DATE2,time,
                                             timehr,timemin,timesec,time2.y,time3.y))
#summary(sofl_precip2)
#write.csv(sofl_precip2,"C:/Aworkspace/Frankenlog/Dec_2020_Data_Analysis/sofl_012521.csv", row.names = FALSE)

####################################################################
# Examine discrete periods of time to identify precip periodicity #
#####################################################################
#http://rstudio-pubs-static.s3.amazonaws.com/423489_acf6fa93aec54d7b9c54bc2af0294514.html
#https://cran.r-project.org/web/packages/hydroTSM/vignettes/hydroTSM_Vignette-knitr.pdf
#Neon Tutorial https://www.neonscience.org/resources/learning-hub/tutorials/da-viz-coop-precip-data-r 

#Just take out date/time and precip for analysis
Pitts_clim2_long <- subset(Pitts_clim2_long, select=c(datetime3,precip_mm))
Pitts_clim2_hourly <- aggregate(Pitts_clim2_long$precip_mm,by=list(Pitts_clim2_long$datetime3),
                                 FUN=sum,na.rm=TRUE)
names(Pitts_clim2_hourly)[names(Pitts_clim2_hourly)=="Group.1"] <- "datetime3"
names(Pitts_clim2_hourly)[names(Pitts_clim2_hourly)=="x"] <- "precip_mm"

ggplot(data=Pitts_clim2_hourly, aes(datetime3, precip_mm))+geom_bar(stat="identity") +   
  xlab("Date") + ylab("Precipitation (mm)")

#hydroplot(sofl_precip3_hourly$precip_mm, var.type="Precipitation", main="at Pittsburg",pfreq = "dm", from="2020-07-02")

## Summarize precip by day
Pitts_clim2_hourly$DATE <- as.Date(Pitts_clim2_hourly$datetime3, # convert to Date class
                               format="%Y%m%d %H:%M")

limits <- as.Date(c("2020-07-10", "2020-10-31"))

ggplot(data=Pitts_clim2_hourly,  # the data frame
       aes(DATE, precip_mm)) +   # the variables of interest
  geom_bar(stat="identity",fill=c("#00A08A")) +   # create a bar graph
  scale_x_date(limits=limits)+
  xlab("Date") + ylab("Precipitation (mm)")+
  theme_solarized()


########################################################
# Summarize Log Data by Canopy Treatments #
########################################################
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# Line Types: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software 
# Color Palettes: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software 
# Change line labels: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/ 

#sofl_precip3 <- subset(sofl_precip2,Canopy== 'C'|Canopy =='GA')  #Remove Certain Treatments
sofl_precip3 <- subset(sofl_precip2,Canopy != 'GQ')  #Remove Certain Treatments

sofl_summary <- ddply(sofl_precip3, c("Canopy","datetime3"), summarise,
               N    = length(Avg_log_vwc),
               median = median(Avg_log_vwc),
               sd   = sd(Avg_log_vwc),
               se   = sd / sqrt(N))

pal <- wes_palette("Darjeeling1", 5, type = "discrete")

#view(sofl_summary)

ggplot(sofl_summary, aes(x=datetime3, y=median, colour=Canopy)) + 
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1, position=pd) +
  geom_line(aes(color=Canopy)) +
  geom_point()+
  scale_color_manual(values = pal,labels=c("GQ"="1/4 Acre Gap", "M"="Matrix Thinning",
                                           "GA"="1 Acre Gap","C"="Control"))+
  theme_solarized() + 
  labs(x = "Datetime Summer 2020",
    y = "Moisture (%)",
    title = "Mean Log Moisture") +
  theme(plot.title = element_text(face = "bold", size = 16))+
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12))+
  theme(legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 14))


#######################################
# Precip and CWD double y axis figure #
#######################################

sofl_summary2 <- ddply(sofl_precip2, c("datetime3"), summarise,
                      N    = length(VWC_4m),
                      median = median(VWC_4m),
                      sd   = sd(VWC_4m),
                      se   = sd / sqrt(N),
                      rain = median(precip_mm))

# this first line is making the plot with precipitation
plot(sofl_summary2$datetime3, sofl_summary2$rain, ylim=c(20, 0), type="h", xaxt="n", yaxt="n", xlab="Summer 2020", ylab="", col="blue")

# this line adds the axis for precipitation. the 4 is the side of the plot where it should be (goes clockwise from bottom being 1)
axis(4, cex.axis=1.25)

# this line adds an axis label for the precip
mtext("15-min Precip (mm)", 4, line=3, cex=1.15, col="blue")

# this line tells the program that the next plot is a new plot on the same one that was just created
par(new="y")

# and then this is just the time series of wood moisture
plot(sofl_summary2$datetime3, sofl_summary2$median, type="l", 
     ylim=c(25, 40), cex.axis=1.25, cex.lab=1.5, ylab="CWD 4m VWC (%)", xlab="")


  
