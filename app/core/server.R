library(shiny)
library(ggplot2)
library(anytime)
library(tidyverse)
library(miceadds) # allows the sourcing of all files
# import widgets
source.all("../modules/", grepstring="\\.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
