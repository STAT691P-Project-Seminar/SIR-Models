library(modules) # must load
# load packages
packages.Self <- modules::use("core/libs.R")
packages.Self$getPackages("server")
library(shiny); library(miceadds); library(anytime); library(ggplot2); library(plotly); library(stringr);
library(tidyverse)
library(maps)
library(ggthemes)
library(deSolve)
# source modules
source.all("modules/", grepstring="\\.R")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  
  
  #get data files
  state.data <- getStateData()
  race.data <- getRaceData()
  county.data <- getCountyData_TS()
  county.data.daily <- getCountyData_Daily()
  county.data.pop <- getCountyData_Pop()
  state.data2 <- state.data
  state.data2$Date <- sapply(state.data2$Date, getTimeSIR)
  state.data2$Date <- as.Date(state.data2$Date)
  
  #gather inputs
  date_range <- reactive({paste(input$daterangeCummulativeState)})
  cumm_daily_switch <- reactive({input$stateCummDailySwitch})
  deaths_confirm <- reactive({input$deathConfirmedCases})
  
  date_range2 <- reactive({paste(input$daterange_cummulative_county)}) 
  cumm_daily_switch2 <- reactive({input$countyCummDailySwitch})
  select_county <- reactive({paste(input$county)})
  staticMap <- reactive({input$mapType})
  per1000Switch <- reactive({input$proportionSwitch})
  
  date_range_p <- reactive({paste(input$daterange_cummulative_county_P)}) 
  pop_confirm <- reactive({input$countyPopSwitch})
  select_county_p <- reactive({paste(input$county_p)})
  thematicDate <- reactive({paste(input$slideMapDate)})
  
  recov_day <- reactive({input$recoveryDays})
  beta <- reactive({input$infectionRate})
  Nos <- reactive({input$N})
  initial <- reactive({input$initInf})
  time_sim <- reactive({input$Tmax})
  model_switch <- reactive({paste(input$yscale)})
  model_reset <- reactive({paste(input$reset)}) 
  
  #set the default values
  # update the date range input values
  updateDateRangeInput(session, "daterangeCummulativeState",
                       start = anydate(min(state.data$timestamp)),
                       end = anydate(max(state.data$timestamp) + (24*60*60)),
                       #min = anydate(min(state.data$timestamp) + (24*60*60)), this does not work .. control it below
                       max = anydate(max(state.data$timestamp) + (24*60*60))
  )
  
  updateDateRangeInput(session, "daterange_cummulative_county",
                       start = anydate(min(county.data$timestamp)),
                       end = anydate(max(county.data$timestamp) + (24*60*60)),
                       #min = anydate(min(state.data$timestamp) + (24*60*60)), this does not work .. control it below
                       max = anydate(max(county.data$timestamp) + (24*60*60))
  )
  
  updateDateRangeInput(session, "daterange_cummulative_county_P",
                       start = anydate(min(county.data$timestamp)),
                       end = anydate(max(county.data$timestamp) + (24*60*60)),
                       #min = anydate(min(state.data$timestamp) + (24*60*60)), this does not work .. control it below
                       max = anydate(max(county.data$timestamp) + (24*60*60))
  )
  updateSliderInput(session, "slideMapDate", 
                    min = anydate(min(county.data$timestamp)+ (24*60*60)), 
                    max = anydate(max(county.data$timestamp)+ (24*60*60)),
                    value = anydate(max(county.data$timestamp)+ (24*60*60)), step = 1
                    )
  
  # toast information
  message = paste("Data was last updated on", getLastUpdated(), sep = " ")
  toastr_info(
    message, title = "Last Updated", 
    closeButton = TRUE, 
    newestOnTop = FALSE,
    progressBar = TRUE, 
    position = c("bottom-full-width"), 
    preventDuplicates = TRUE, 
    showDuration = 400,
    hideDuration = 1000, 
    timeOut = 5000, 
    extendedTimeOut = 1000,
    showEasing = c("swing"), 
    hideEasing = c("swing"),
    showMethod = c("fadeIn"), 
    hideMethod = c("fadeOut")
  )
  
  ##listen to event change
  observe({
    # set header
   output$header <- renderValueBox({
      valueBox(h4("Massachusetts Corona Virus Data ", align = "center"),
               h6("Data was last updated on", getLastUpdated(), align = "center"),
               color = "black"
               )
    })
    
    static <- staticMap()
    per1000Switch <- per1000Switch()
    thematic_date <- thematicDate()
    #print(thematic_date)
    if(static=="static"){

      output$stateMap1 <- renderPlotly({
        getStaticMap(per1000Switch, thematic_date)
      })
    }else if(static=="thematic"){
      output$stateMap1 <- renderPlotly({
        getCountyMap(per1000Switch, thematic_date)
      })
    }
    
    # construct user selected input range
    date_range <- date_range()
    #print(date_range)
    start_date = as.list(str_split(date_range, ' '))[[1]]
    start_date.ts = as.numeric(as.POSIXct(as.Date(start_date)))
    if (start_date.ts < min(state.data$timestamp)){ start_date.ts =   min(state.data$timestamp)}
    #print(max(state.data$timestamp))
    end_date = as.list(str_split(date_range, ' '))[[2]]
    end_date.ts = as.numeric(as.POSIXct(as.Date(end_date)))
    # ensure that the max does not fall below the min
    if (end_date.ts < start_date.ts){ end_date.ts = start_date.ts}
    
    date_range_p <- date_range_p()
    start_date_p = as.list(str_split(date_range_p, ' '))[[1]]
    start_date.ts_p = as.numeric(as.POSIXct(as.Date(start_date_p)))
    if (start_date.ts_p < min(county.data$timestamp)){start_date.ts_p = min(county.data$timestamp)}
    end_date_p = as.list(str_split(date_range_p, ' '))[[2]]
    end_date.ts_p = as.numeric(as.POSIXct(as.Date(end_date_p)))
    # ensure that the max does not fall below the min
    if (end_date.ts_p < start_date.ts_p){ end_date.ts_p = start_date.ts_p}
    
    date_range2 <- date_range2()
    start_date2 = as.list(str_split(date_range2, ' '))[[1]]
    start_date.ts2 = as.numeric(as.POSIXct(as.Date(start_date2)))
    if (start_date.ts2 < min(county.data$timestamp)){start_date.ts2 = min(county.data$timestamp)}
    end_date2 = as.list(str_split(date_range2, ' '))[[2]]
    end_date.ts2 = as.numeric(as.POSIXct(as.Date(end_date2)))
    # ensure that the max does not fall below the min
    if (end_date.ts2 < start_date.ts2){ end_date.ts2 = start_date.ts2}
    
    # subset the relevant data
    new.data <- state.data[ which( state.data$timestamp >= start_date.ts & state.data$timestamp <= end_date.ts) , ]
    
    plotStateCases <- reactive(
      { 
        if(!deaths_confirm()){
          displayPrettyBarChartConfirmed(new.data, start_date.ts, end_date.ts, cumm_daily_switch())
        }else{
          displayPrettyBarChartDeaths(new.data, start_date.ts, end_date.ts, cumm_daily_switch()) 
        }
        
      })
    
    plotCountyCases <- reactive({
      if(!cumm_daily_switch2())
      {
        filtered_data = subset(county.data, select = c("Date", "timestamp", "timeplot", select_county()))
        names(filtered_data)[4] = "Cases"
        new_data2 = filtered_data[which(filtered_data$timestamp >= start_date.ts2 & filtered_data$timestamp <= end_date.ts2),]
        displayPrettyChartCounty(new_data2, start_date.ts2, end_date.ts2)
      }
      else
      {
        filtered_data2 = subset(county.data.daily, select = c("Date", "timestamp", "timeplot", select_county()))
        names(filtered_data2)[4] = "Cases"
        new_data3 = filtered_data2[which(filtered_data2$timestamp >= start_date.ts2 & filtered_data2$timestamp <= end_date.ts2),]
        displayPrettyChartCounty(new_data3, start_date.ts2, end_date.ts2)
      }
      })
    
    plotCountyPop <- reactive({
      if(!pop_confirm())
      {
        filtered_data_p1 = subset(county.data, select = c("Date", "timestamp", "timeplot", select_county_p()))
        names(filtered_data_p1)[4] = "Cases"
        new_data_p1 = filtered_data_p1[which(filtered_data_p1$timestamp >= start_date.ts_p & filtered_data_p1$timestamp <= end_date.ts_p),]
        displayPrettyChartCounty(new_data_p1, start_date.ts_p, end_date.ts_p)
      }
      else
      {
        filtered_data_p2 = subset(county.data.pop, select = c("Date", "timestamp", "timeplot", select_county_p()))
        names(filtered_data_p2)[4] = "Cases"
        new_data_p2 = filtered_data_p2[which(filtered_data_p2$timestamp >= start_date.ts_p & filtered_data_p2$timestamp <= end_date.ts_p),]
        displayPrettyChartCounty_pop(new_data_p2, start_date.ts_p, end_date.ts_p)
      }
    })
    
    if(model_reset() == 1)
    {
      updateSliderInput(session, "recoveryDays", min = 1, max = 30, value = 14)
      updateSliderInput(session, "infectionRate", min = 0.001, max = 1, value = 0.3)
      updateNumericInput(session, "N", value = 6900000, max = 10^10, min = 1000)
      updateNumericInput(session, "initInf", value = 1, min = 1)
      updateSliderInput(session, "Tmax", min = 0, max = 365, value = 150)
    }
    
    plotsir1 <- reactive({
      if(model_switch() == "linear")
      {
      sir_data <- sir_sim2(N = Nos(), alpha = 1/recov_day(), beta = beta(), initInf = initial(), TS = time_sim()) 
      displayPrettyChart_SIR(sir_data, state.data2)
      #ggplot(sir_data, aes(x = Date, y = I_tcom)) + geom_line() + geom_point(state.data2, mapping = aes(x = Date, y = Cases)) + xlab("") + ylab("Cases")
      }
      else
      {
      sir_data <- sir_sim3(N = Nos(), alpha = 1/recov_day(), beta = beta(), initInf = initial(), TS = time_sim()) 
      state.data2$Cases = state.data2$Cases/Nos()
      state.data2$Cases = round(state.data2$Cases, 2)
      displayPrettyChart_SIR(sir_data, state.data2)
      }
    })
    
    plotsir2 <- reactive({
      data.sir <-  sir_simulate(beta = beta(), gamma = 1/recov_day(), N = Nos(), I0 = initial(), time = time_sim())
      sir_data2 <- sir_sim2(N = Nos(), alpha = 1/recov_day(), beta = beta(), initInf = initial(), TS = time_sim()) 
      #displayPrettyChart_SIR2(sir_data2)
      displayPrettySIRSimulation(data.sir)
      #ggplot(sir_data, aes(x = Date, y = I_tcom)) + geom_line() + geom_point(state.data2, mapping = aes(x = Date, y = Cases)) + xlab("") + ylab("Cases")
    })
    
    # tie to a render event
    output$stateCasesPlot <- renderPlotly({
      plotStateCases()
    })
    
    output$countyInfectionsPlot <- renderPlotly({
      plotCountyCases()
    })
    
    output$countyInfectionsPlot_P <- renderPlotly({
      plotCountyPop()
    })
    
    output$spreadPlot <- renderPlotly({
      plotsir1()
    })
    
    output$plotSIR <- renderPlotly({
      plotsir2()
    })
    
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
      displayTotalRecovered("(?)")
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
      displayTotalRecovered("(?)")
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
    
    # race distribution plots
    raceCases = displayCasesRaceDistribution(race.data)
    output$hispanicCases <- renderInfoBox({raceCases$hisp})
    output$nonHispanicWhiteCases <- renderInfoBox({ raceCases$white })
    output$blackCases <- renderInfoBox({ raceCases$black })
    output$asianCases <- renderInfoBox({ raceCases$asian })
    output$otherCases <- renderInfoBox({ raceCases$other })
    output$unknownCases <- renderInfoBox({ raceCases$unknown })
    
    deathCases = displayDeathsRaceDistribution(race.data)
    output$hispanicDeaths <- renderInfoBox({deathCases$hisp})
    output$nonHispanicWhiteDeaths <- renderInfoBox({ deathCases$white })
    output$blackDeaths <- renderInfoBox({ deathCases$black })
    output$asianDeaths <- renderInfoBox({ deathCases$asian })
    output$otherRaceDeaths <- renderInfoBox({ deathCases$other })
    output$unknownDeaths <- renderInfoBox({ deathCases$unknown })
    
  })

  
})

return(server)
