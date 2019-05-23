#libraries
library(shiny)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)


file_path <-"/home/jmoy001/build-dir/spec-data/ShinyApp/specdata"
setwd(file_path)

file_names <- dir("/home/jmoy001/build-dir/spec-data/ShinyApp/specdata/") 


complete<-function(file_directory, index) {
  df <- do.call(rbind,lapply(file_directory[index],read.csv))
  df<- df[complete.cases(df),]
}

#output
data.df <- complete(file_names, 1:332)

#convert dataframe to datatable
data.tb <- as.data.table(data.df)

#convert data from wide to long
#nitrate and sulfate label in 1 column, values in 1 column
data.tb.final <-data.tb %>%
  gather(ion, value,
         sulfate,
         nitrate)


#convert from factor to date class
data.tb.final$Date <-as.Date(data.tb.final$Date, format= "%Y-%m-%d")

#filter
sulfate.tb<- filter(data.tb.final, ion=="sulfate")
nitrate.tb<- filter(data.tb.final, ion=="nitrate")

#sulfate
#max values for each ID
sulfate_max <-sulfate.tb %>%
  group_by(ID)  %>%
  summarise(Value = max(value))

#ID for those values greater than 30
sulfate_final <- sulfate_max %>%
  filter(., Value>=30)

sulfate_final2 <-paste(sulfate_final$ID, collapse=", ")

#nitrate
nitrate_max <-nitrate.tb %>%
  group_by(ID)  %>%
  summarise(Value = max(value))

nitrate_final <-nitrate_max %>%
  filter(., Value>=40)

filter(nitrate_max, Value>=40)


nitrate_final2 <-paste(nitrate_final$ID, collapse=", ")


### json portion

file_path<-"/home/jmoy001/build-dir/spec-data/ShinyApp/"
setwd(file_path)
json <-fromJSON('samplejson.json')
json

json[[2]]="EPA Probes above limit"


json[[3]]$field_1 = "List of probes exceeding EPA limit"

json[[3]]$value_1 = 
  paste("probe ID exceeding sulfate limit: ", sulfate_final2,"probe ID exceeding nitrate limit: ", nitrate_final2)

#report url
json[[4]]$field_2="report url"

#url of shiny report
json[[4]]$value_2='http://jm-rstudio-3838-myproject.192.168.42.64.nip.io/'

#list email
json[[5]]$value_3='jeffrey.moy@uscellular.com'
###



### Event Post

#Event_POST <- POST(url = 'http://event-router-event-ms.devengos.uscc.com/event_router/v1/send/', 
#body = json, encode = "json", httr::add_headers("Content-Type" = "application/json"), 
#verbose())

#status_code(Event_POST)

#http_status(Event_POST)

###  





#ui
ui <- fluidPage(
  titlePanel("Probe IDs above EPA thresholds for Sulfate:"),
  titlePanel(h6(sulfate_final2)),
  titlePanel("Probe IDs above EPA thresholds for Nitrate:"),
  titlePanel(h6(nitrate_final2)),
  actionButton(inputId = "email", label= "Email Alarm"),
  sliderInput(inputId="probe", label="Select probeID:", min=min(data.tb.final$ID), max=max(data.tb.final$ID), value =24, step=1),
  plotOutput("timeseries")
)

#server
server <- function(input,output){
  observeEvent(input$email, {
    Event_POST <- POST(url = 'http://event-router-event-ms.devengos.uscc.com/event_router/v1/send/', 
    body = json, encode = "json", httr::add_headers("Content-Type" = "application/json"), 
    verbose())})
  output$timeseries <- renderPlot({
    #final
    g <- ggplot(data=data.tb.final[data.tb.final$ID==input$probe,], aes(x=Date, y=value, colour=ion)) + geom_line() #dont need geom_point()
    g <- g+ scale_colour_manual(values=c("blue","red")) #nitrate=blue, sulfate=red
    g <- g + scale_x_date(date_breaks= "1 year")
    #cutoff lines
    g <- g + geom_hline(yintercept=30, linetype="solid", color="red", size=1)
    g <- g + geom_hline(yintercept=40, linetype="solid", color="blue", size=1)
    plot(g)
  })
}


shinyApp(ui=ui, server=server)
