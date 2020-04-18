library(modules) # must load
# load packages
packages.Self <- modules::use("core/libs.R")
packages.Self$getPackages("server")

# source modules
source.all("modules/", grepstring="\\.R")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  #get data files
  state.data <- getStateData()
  race.data <- getRaceData()
  
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
    
    
    # info box output
    # confirmed cases
    output$confirmedCases <- renderInfoBox({
      displayConfirmedCases(formatC( state.data$Cases[length(state.data$Cases)], big.mark = ","))
    })
    # deaths
    output$deaths <- renderInfoBox({
      displayTotalDeaths(formatC( state.data$Deaths[length(state.data$Deaths)], big.mark = ","))
    })
    # recovered
    output$totalRecovered <- renderInfoBox({
      displayTotalRecovered(0)
    })
    
    # display for state and county
    # confirmed cases
    output$confirmedCasesSC <- renderInfoBox({
      displayConfirmedCases(formatC( state.data$Cases[length(state.data$Cases)], big.mark = ","))
    })
    # deaths
    output$deathsSC <- renderInfoBox({
      displayTotalDeaths(formatC( state.data$Deaths[length(state.data$Deaths)], big.mark = ","))
    })
    
    # recovered
    output$recoveredSC<- renderInfoBox({
      displayTotalRecovered(0)
    })
    # male confirmed cases
    output$maleConfirmedCases <- renderInfoBox({
      vec.values <- c( state.data$MaleCases[length(state.data$MaleCases)], 
        state.data$MaleCases[length(state.data$MaleCases)]/state.data$Cases[length(state.data$Cases)]*100)
      displayMaleCasesDistribution( formatC(vec.values[1], big.mark = ",", format = "d"), round(vec.values[2], 2))
    })
    # female confirmed cases
    output$femaleConfirmedCases <- renderInfoBox({
      vec.values <- c( state.data$FemaleCases[length(state.data$FemaleCases)], 
                       state.data$FemaleCases[length(state.data$FemaleCases)]/state.data$Cases[length(state.data$Cases)]*100)
      displayFemaleCasesDistribution( formatC(vec.values[1], big.mark = ",", format = "d"), round(vec.values[2], 2))
    })
    # other confirmed cases
    output$otherConfirmedCases <- renderInfoBox({
      vec.values <- c( state.data$UnknownCases[length(state.data$UnknownCases)], 
                       state.data$UnknownCases[length(state.data$UnknownCases)]/state.data$Cases[length(state.data$Cases)]*100)
      displayOtherCasesDistribution( formatC(vec.values[1], big.mark = ",", format = "d"), round(vec.values[2], 2))
    })
    
  })

  
})

return(server)