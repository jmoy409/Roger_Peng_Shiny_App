

library(RJSONIO)


file_names <- "C:\\Users\\jmoy001\\Documents\\Analytics Engineer\\Analytic and Automation Summit #2\\Shiny App"
setwd(file_names)
json <-fromJSON('samplejson.json')
json

json[2]="EPA Probes above limit"


json$opt_parms1[1] = "List of probes exceeding EPA limit"

json$opt_parms1[2] = 

