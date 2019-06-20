library(shiny)
library(shinydashboard)
library(rhandsontable)
library(readxl)
library(shiny)
library(DT)
library(mosaic)
library(scales)
library(lubridate)
library(plotly)
library(tidyverse)


if (interactive()){
ui <- dashboardPage(
      dashboardHeader(title = "DMR"),
      dashboardSidebar(
      # date range selector format yyyy-mm-dd
      # start = '2000-01-01', end = '2000-01-31',
      dateRangeInput("dates", label = h3("Date range"), start=paste0(strsplit(as.character(Sys.Date()),"-")[[1]][1],"-01-01"), 
                     end = paste0(strsplit(as.character(Sys.Date()),"-")[[1]][1],"-01-31")),
      submitButton("Submit", icon("refresh"))
          ),
      dashboardBody(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",
                             # column(
                             #   width = 6,
                             plotlyOutput("plot1", height = "500px"),
                               # ),
                               # column(
                                # width = 6,
                                tableOutput("selected_dailies"),
                             downloadButton("report", "Generate report")
                    ),
                    tabPanel("Data", 
                             column(
                              width = 8,
                              dataTableOutput("flow_data")),
                              downloadButton("downloadData", "Download")
                    )
                    )
        # column(6, dataTableOutput("flow_data")),
        # plotOutput("plot1")
        # tags$style(type="text/css",
        #            ".shiny-output-error { visibility: hidden; }",
        #            ".shiny-output-error:before { visibility: hidden; }")
      )
  )
server <- function(input, output){

    flow_df <- reactive({
      #if (is.null(input$dates)){
      #if (TRUE){
      #  raw_3a <- data.frame(input$dates)
      #}
      #else{
      
        #function to get week numbers from month
        first_day_of_month_wday <- function(dx) {
          day(dx) <- 1
          wday(dx)
        }
        
       api_connect <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=13055340&startDT=",input$dates[1],"&endDT=",input$dates[2])
       raw_1 <- readLines(api_connect)
       raw_2 <- raw_1[-grep("#",raw_1)]
       raw_2a <- gsub("\t",",",raw_2)
       write.csv(raw_2a[-2], "raw_2a.csv", row.names = FALSE, quote = FALSE, col.names=NULL)
       # # write.table(raw_2[-2], file = "raw_3.txt",row.names = F,col.names = F)
       raw_3a <- read.csv("raw_2a.csv", header=TRUE, skip=1)
       # # raw_4 <- read.table("raw_3.txt",sep="\t",header = T,)
       raw_3a <- raw_3a %>%
         mutate(date_time = as.Date(paste(raw_3a$datetime,sep=""),format="%Y-%m-%d")) %>%
         mutate(year = date_time, flow = as.numeric(as.character(X45311_00060_00003))) %>% 
         mutate(year = substring(year,1,4)) %>% 
         # rename(flow = X45311_00060_00003) %>% 
         select(-c(datetime, X45311_00060_00003_cd, X45311_00060_00003)) %>% 
         mutate(week_num = ceiling((day(date_time) + first_day_of_month_wday(date_time) - 1) / 7)) %>% 
         mutate(month = month(date_time), year = year(date_time), day = wday(date_time, label=T)) %>% 
         mutate(
           season = case_when(
             month %in% 7:11 ~ "Fall",
             month %in%  c(12,1,2)  ~ "Winter",
             month %in%  3:5  ~ "Spring",
             TRUE ~ "Summer")) %>% 
         select(-c(agency_cd, site_no))
      return(raw_3a)
      
    })
    
    #get the first day of each week for the vline
    df4 <- reactive({
      flow_df() %>%
      group_by(week_num) %>%
      slice(which.max(week_num)) %>% 
      ungroup()
    })
    
    output$flow_data <- renderDataTable(flow_df())
      # rendered_table <- flow()
      # datatable(rendered_table)
      # })
    

    output$plot1 <- renderPlotly({
      #ggplot to show the data
      #   flow_dmr <- ggplot(flow_df(), aes(x=date_time, y=flow)) +
      #   geom_point(aes(size = flow, colour = flow)) +
      #   geom_line(aes(colour = flow)) +
      #   scale_x_date(breaks = "day", labels=date_format("%y-%m-%d")) +
      #   scale_colour_gradient(low = "green", high = "black") +
      #   guides(color=guide_legend(), size = guide_legend()) +
      #   geom_vline(data=df4(),aes(xintercept = as.Date(date_time,"%Y-%m-%d")), linetype="dotted",
      #              color = "black", size=1) +
      #   #xintercept = as.Date("2016-03-29","%Y-%m-%d"))
      #   # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #   theme_bw() +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "left")
      # ggplotly(flow_dmr)
      
      #plotly plot that can show the data
      plot_ly() %>%
        add_trace(x=as.Date(flow_df()$date_time),
                  y=flow_df()$flow,
                  size=flow_df()$flow,
                  marker = list(
                    color = 'rgba(93, 204, 25, 1)',
                    line = list(
                      color = 'rgba(93, 204, 25, 1)'
                    )
                  ),
                  line = list(
                    color = 'rgba(0, 0, 0, 1)',
                    line = list(
                      color = 'rgba(0, 0, 0, 1)'
                    )
                  ),
                  # colors=c("green","black"),
                  mode='markers+lines',
                  mode="scatter",
                  name="Average Flow",
                  hoverinfo='text',
                  text=paste0(flow_df()$day,', ', format(flow_df()$date_time,format="%d-%m-%Y"), '\n', 'Flow: ', flow_df()$flow)) %>%
        # add_trace(x=as.Date(flow_df()$date_time),
        #           y=flow_df()$flow,
        #           # color = flow_df()$flow,
        #           mode='lines',
        #           mode="scatter",
        #           name="Average Flow") %>%
        layout(xaxis=list(
          tick0 = flow_df()$date_time[1]),
          dtick = 7)
          # plot_bgcolor='rgb(230,228,228,1)') #background color in graph
          # paper_bgcolor='rgb(233,233,233)') #background colour outside graph
      
      })
    

    #get weekly average
    week_avg <- reactive({
      flow_df() %>% 
      group_by(week_num,month) %>% 
      summarise(Mean = mean(flow,na.rm = T),Max = max(flow,na.rm=T), Min = min(flow,na.rm=T),Count = n()) %>% 
      ungroup() %>% 
      mutate(month_name = month.name[month], month = month) 
      
    })
    #get monthly average
    month_avg <- reactive({
      flow_df() %>% 
      group_by(month) %>% 
      summarise(Mean = mean(flow,na.rm=T),Max = max(flow,na.rm=T), Min = min(flow,na.rm=T),Count = n()) %>% 
      ungroup() %>% 
      mutate(month_name = month.name[month])
    })
      
    #making df of month and week avg
    average_flow <- reactive({
      full_join(week_avg(),month_avg()) %>% 
        mutate_if(is.factor, ~as.numeric(as.character(.))) %>% 
        mutate_at(3, funs(round(., 1))) %>% 
        # select(-c(month)) %>% 
        select(-Count, everything()) %>% 
        select(month_name, everything()) %>% 
        mutate(week_num=ifelse(is.na(week_num), str_c(month_name," Average"),week_num)) %>% 
        rename(Month = month_name, `Week Number` = week_num) %>%
        arrange(month) %>% 
        select(-c(month,Month))
      
    })
    
    #making a table from the average_flow data
    output$selected_dailies <- renderTable({average_flow()},
                                           include.rownames=FALSE)
    #download data for the data tab
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dates, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(flow_df(), file, row.names = FALSE)
      }
    )
    
    #download data for the first tab in the app
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.docx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(n = flow_df())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
      
      
# 
#       nearPoints(flow_df(), input$flow),
#       transmute(
#         date_time,
#         flow,
#         week_num,
#         season
#       )
# 
#       options = list(dom = "tip", pageLength = 10, searching = FALSE)
#     )
    
    # cuts weeks from a month
    # df2 %>% mutate(week = (year(Order_Date) - year(min(Order_Date)))*52 + 
    #                  week(Order_Date) - week(min(Order_Date)),
    #                week2 = (as.numeric(Order_Date) %/% 7) - (as.numeric(min(Order_Date)) %/% 7)) %>%
    #   arrange(Order_Date)

            
          }
      }

shinyApp(ui=ui, server = server)
