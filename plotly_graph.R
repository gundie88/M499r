
 #function to get week numbers from month
 first_day_of_month_wday <- function(dx) {
   day(dx) <- 1
   wday(dx)
 }
 
 input <- list()
 input$dateRange <- c("2016-03-01","2016-03-31")
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
   mutate(year = date_time, flow = X45311_00060_00003) %>% 
   mutate(year = substring(year,1,4)) %>% 
   # rename(flow = X45311_00060_00003) %>% 
   select(-c(datetime, X45311_00060_00003_cd, X45311_00060_00003)) %>% 
   mutate(week_num = ceiling((day(date_time) + first_day_of_month_wday(date_time) - 1) / 7)) %>% 
   mutate(week_num = factor(week_num)) %>% 
   mutate(month = month(date_time), year = year(date_time), day = wday(date_time, label=T)) %>% 
   mutate(
     season = case_when(
       month %in% 9:11 ~ "Fall",
       month %in%  c(12,1,2)  ~ "Winter",
       month %in%  3:5  ~ "Spring",
       month %in% 6:8 ~ "Summer")) 
 
 ggplot(flow_df(), aes(x=date_time, y=flow)) +
   geom_point(aes(size = flow, colour = flow)) +
   geom_line(aes(colour = flow)) +
   scale_x_date(breaks = "day", labels=date_format("%y-%m-%d")) +
   scale_colour_gradient(low = "green", high = "black") +
   guides(color=guide_legend(), size = guide_legend()) +
   geom_vline(data=df4(),aes(xintercept = as.Date(date_time,"%Y-%m-%d")), linetype="dotted",
              color = "black", size=1) +
   #xintercept = as.Date("2016-03-29","%Y-%m-%d"))
   # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left")  
 
 df4 <- df %>%
   group_by(week_num) %>%
   slice(which.max(week_num)) %>% 
   ungroup()
 
 
 # paste(raw_3a$datetime,sep=""),format="%d-%m-%Y")
 # day(df$date_time), '-',month(df$date_time), ', ', year(df$date_time),
 plot_ly() %>% 
   add_trace(x=as.Date(df$date_time),
             y=df$flow,size=df$flow,
             mode='markers', 
             mode="scatter",
             name="Average Flow", 
             hoverinfo='text',
             text=paste0(df$day,', ', format(df$date_time,format="%d-%m-%Y"), '\n', 'Flow: ', df$flow)) %>%
   add_trace(x=as.Date(df$date_time),
             y=df$flow,
             mode='lines', 
             mode="scatter",
             name="Average Flow") %>% 
   layout(xaxis=list(
     tick0 = df$date_time[1]),
     dtick = 7) 
   
   
   
 