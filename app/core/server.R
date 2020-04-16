library(shiny)
library(ggplot2)
library(anytime)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

## add all data processong stuffs here  
state.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/StateLevelData.csv")
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
# add the daily infections
daily_cases <- c(state.data$Cases)[1]
for (row in 2:dim(state.data)[1]){ daily_cases = c(daily_cases, (state.data$Cases[row]-state.data$Cases[row-1]) ) }
state.data$daily_cases = daily_cases
print(state.data)

### data processing ends here
  
  ##gather inputs
  date_range <- reactive({paste(input$daterange_cummulative)})
  cumm_daily_switch <- reactive({input$stateCummDailySwitch})
  
  ##listen to event change
  observe({
    #set the default values
    # date range
    date_range <- date_range()
    start_date = as.list(str_split(date_range, ' '))[[1]]
    start_date.ts = as.numeric(as.POSIXct(as.Date(start_date)))
    end_date = as.list(str_split(date_range, ' '))[[2]]
    end_date.ts = as.numeric(as.POSIXct(as.Date(end_date)))
    
    
  
    plotStateInfections <- reactive(
      {
        new.data <- state.data[ which( state.data$timestamp >= start_date.ts & state.data$timestamp <= end_date.ts) , ]
        if(cumm_daily_switch()){
          # make the cummulative plot
          print(new.data)
          ggplot(data = new.data, mapping = aes(x = timeplot, y = Cases)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + xlab("Date") + ylab("Cummulative Cases") #+ scale_x_date(labels = date_format("%d-%m"))
        }else{
          # make the daily plot
          ggplot(data = new.data, mapping = aes(x = timeplot, y = daily_cases)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + xlab("Date") + ylab("Daily Cases") #+ scale_x_date(labels = date_format("%d-%m"))
        }
        
      }
    )
    ##tie to a render event
    output$stateInfectionsPlot <- renderPlot({
      plotStateInfections()
    })
    
    #print(as.double(sliderVals()[1]))
    
  })

  
})
