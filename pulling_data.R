#install.packages(c("httr", "jsonlite", "lubridate"))
library(httr)
library(jsonlite)
library(stringr)
library(mosaic)
library(scales)
library(rnoaa)
library(lubridate)
library(tidyverse)


# This is the url to give datat
#https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=13055340&startDT=2018-04-01


# 

#function to get week numbers from month
first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}

input <- list()
input$dateRange <- c("2019-03-01","2019-03-31")
h <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=13055340&startDT=",input$dateRange[1],"&endDT=",input$dateRange[2])

#get all data from 2000 to make predictions 
#works for getting data from the API that was obtained
raw_1 <- readLines(h)
raw_2 <- raw_1[-grep("#",raw_1)]
raw_2a <- gsub("\t",",",raw_2)
write.csv(raw_2a[-2], "raw_2a.csv", row.names = FALSE, quote = FALSE, col.names=NULL)
# write.table(raw_2[-2], file = "raw_3.txt",row.names = F,col.names = F)
raw_3a <- read.csv("raw_2a.csv", header=TRUE, skip=1)
# raw_4 <- read.table("raw_3.txt",sep="\t",header = T,)

#cleaning the data, put date_time in proper format, and renamed 
# the cloumns in use 
df <- raw_3a %>%
  mutate(date_time = as.Date(paste(raw_3a$datetime,sep=""),format="%Y-%m-%d")) %>%
  mutate(year = date_time, flow = as.numeric(as.character(X45311_00060_00003))) %>% 
  mutate(year = substring(year,1,4)) %>% 
  # rename(flow = X45311_00060_00003) %>% 
  select(-c(datetime, X45311_00060_00003_cd, X45311_00060_00003)) %>% 
  mutate(week_num = ceiling((day(date_time) + first_day_of_month_wday(date_time) - 1) / 7)) %>% 
  mutate(week_num = factor(week_num)) %>% 
  mutate(month = month(date_time), year = year(date_time)) %>% 
  mutate(
    season = case_when(
      month %in% 9:11 ~ "Fall",
      month %in%  c(12,1,2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      month %in% 6:8 ~ "Summer")) 



#######
#for data visualization
df2 <- df %>% 
  mutate(season_order = factor(season, levels=c("Spring","Summer", "Fall","Winter")))%>% 
  mutate(month_name = month.name[month]) %>% 
  mutate_at(4,funs(factor)) %>% 
  mutate(date_time1 = format(df$date_time, "%m-%d"))

#creates viz for yearly flow data based on season 
ggplot(df2, aes(x=as.Date(date_time1,"%m-%d"), y=flow, color=year)) +
  # geom_line(aes(color=flow)) + 
  geom_line() + 
  scale_x_date(breaks = "month",labels=date_format("%m-%d")) +
  facet_grid(.~season_order, scales="free") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left") 




######
#just to see what the data looks like
ggplot(df, aes(x=date_time, y=flow)) +
  geom_point() +
  scale_x_date(breaks = "day", labels=date_format("%y-%m-%d")) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left")




#####
#get the first day of each week for the vline
df4 <- df %>%
  group_by(week_num) %>%
  slice(which.max(week_num)) %>% 
  ungroup()

#just to see what the data looks like with gradient
ggplot(df, aes(x=date_time, y=flow)) +
  geom_point(aes(size = flow, colour = flow)) +
  geom_line(aes(colour = flow)) +
  scale_x_date(breaks = "day", labels=date_format("%y-%m-%d")) +
  scale_colour_gradient(low = "green", high = "black") +
  guides(color=guide_legend(), size = guide_legend()) +
  geom_vline(data=df4,aes(xintercept = as.Date(date_time,"%Y-%m-%d")), linetype="dotted",
             color = "black", size=1) +
  #xintercept = as.Date("2016-03-29","%Y-%m-%d"))
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left") 

#Gradient filled graph
ggplot(df, aes(x=date_time, y=flow)) +
  geom_ribbon(aes(ymax = flow, ymin = 0, group = week_num)) +
  geom_col(aes(fill = flow)) +
  scale_x_date(breaks = "day", labels=date_format("%y-%m-%d")) +
  scale_fill_gradient2(position="bottom" , low = "green", mid = muted("green"), high = "black", 
                       midpoint = median(df$flow)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left") 



######   
#get weekly average
week_avg <- df %>% 
  group_by(week_num,month) %>% 
  summarise(mean = mean(flow,na.rm=T),max = max(flow,na.rm=T), min = min(flow,na.rm=T),Count = n()) %>% 
  ungroup() %>% 
  mutate(month_name = month.name[month], month = month) 

month_avg <- df %>% 
  group_by(month) %>% 
  summarise(mean = mean(flow,na.rm=T),max = max(flow,na.rm=T), min = min(flow,na.rm=T),Count = n()) %>% 
  ungroup() %>% 
  mutate(month_name = month.name[month])

#making df of all averages do DT 
average_flow <- full_join(week_avg,month_avg) %>% 
  mutate_if(is.factor, ~as.numeric(as.character(.))) %>% 
  mutate_at(3, funs(round(., 1))) %>% 
  # select(-c(month)) %>% 
  select(-Count, everything()) %>% 
  select(month_name, everything()) %>% 
  mutate(week_num=ifelse(is.na(week_num), str_c(month_name," Average"),week_num)) %>% 
  rename(Month = month_name, `Week Number` = week_num) %>%
  arrange(month) %>% 
  select(-c(month,Month))
  


ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00102676', token = "sySPPPjTnKPCJCgczZQUyOhBJqCsWJbE")
out <- ncdc(datasetid='NORMAL_DLY', datatypeid='dly-tmax-normal', startdate = '2010-05-01', enddate = '2010-05-10',token = "sySPPPjTnKPCJCgczZQUyOhBJqCsWJbE")
ncdc(datasetid = 'NORMAL_DLY', locationid = 'ZIP:83422', datatypeid = 'HPCP', limit = 5, token =  "sySPPPjTnKPCJCgczZQUyOhBJqCsWJbE")
#NCDC = national climate data center
#datasetid: (required) Accepts a single valid dataset id. Data returned will be from the dataset specified
#locationid: Accepts a valid location id or a vector or list of location ids (optional)
#datatypeid: Accepts a valid data type id or a vector or list of data type ids. (optional)
#limit: (integer) number to return
#stationid: Accepts a valid station id or a vector or list of station ids.
  #(character) A character string giving the identification of the weather station for
  # which the user would like to pull data. To get a full and current list of stations,
  # the user can use the ghcnd_stations function. To identify stations within a
  # certain radius of a location, the user can use the meteo_nearby_stations function.
#Token: This is the API key
#HPCP:	Hourly precipitation data.  This is the only data type reported.  (Includes the daily total.)

#Hone in on the weather data, you need to have this done by next week 











