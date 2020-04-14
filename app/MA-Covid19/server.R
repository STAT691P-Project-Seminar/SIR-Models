library(shiny)
library(ggplot2)
library(anytime)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

state.data = read.csv("/Users/sixtusdakurah/Documents/GRAD SCHOOL/SPRING 2020/Project Seminar/SIR-Models/app/MA-Covid19/StateLevelData.csv")
# add date time stamp
getDate <- function(str){
  ls = as.list(str_split(str , '/'))[[1]] #month, day, year
  date.char = paste(ls[3], "-", ls[1], "-", ls[2], sep='')
  timestamp = as.numeric(as.POSIXct(as.Date(date.char)))
  return(timestamp)
}

getPlottableDates <- function(str){
  myDate = anytime(str + (24*60*60))
  newDate = paste(format(myDate,"%b"), as.numeric(format(myDate,"%d")), sep=" ")
  return(newDate)
}

state.data$timestamp <- sapply(state.data$Date, getDate)
state.data$timeplot <- sapply(state.data$timestamp, getPlottableDates)
state.data$timeplot <- factor(state.data$timeplot, levels = state.data$timeplot)
print(state.data)
  
  ##gather inputs
  date_range <- reactive({paste(input$daterange1)})
  
  ##listen to event change
  observe({
    #set the default values
    date_range <- date_range()
    start_date = as.list(str_split(date_range, ' '))[[1]]
    start_date.ts = as.numeric(as.POSIXct(as.Date(start_date)))
    end_date = as.list(str_split(date_range, ' '))[[2]]
    end_date.ts = as.numeric(as.POSIXct(as.Date(end_date)))
    
    ###varying delta
    pltStateInfections <- reactive(
      {
        new.data <- state.data[ which( state.data$timestamp >= start_date.ts & state.data$timestamp <= end_date.ts) , ]
        print(start_date)
        ggplot(data = new.data, mapping = aes(x = timeplot, y = Cases)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + xlab("Date") #+ scale_x_date(labels = date_format("%d-%m"))
      }
    )
    ##tie to a render event
    output$stateInfectionsPlot <- renderPlot({
      pltStateInfections()
    })
    
    #print(as.double(sliderVals()[1]))
    
  })

  
})
