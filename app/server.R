library(modules) # must load



# load packages
packages.Self <- modules::use("core/libs.R")
packages.Self$getPackages("server")
library(shiny); library(miceadds); library(anytime); library(ggplot2); library(plotly); library(stringr);

# source modules
source.all("modules/", grepstring="\\.R")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  #get data files
  state.data <- getStateData()
  race.data <- getRaceData()
  county.data <- getCountyData()
  
  ##gather inputs
  date_range <- reactive({paste(input$daterangeCummulativeState)})
  cumm_daily_switch <- reactive({input$stateCummDailySwitch})
  deaths_confirm <- reactive({input$deathConfirmedCases})
  
  #set the default values
  # update the date range input values
  updateDateRangeInput(session, "daterangeCummulativeState",
                       start = anydate(min(state.data$timestamp)),
                       end = anydate(max(state.data$timestamp) + (24*60*60)),
                       #min = anydate(min(state.data$timestamp) + (24*60*60)), this does not work .. control it below
                       max = anydate(max(state.data$timestamp) + (24*60*60))
  )
  
  ##listen to event change
  observe({
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
    
    # tie to a render event
    output$stateCasesPlot <- renderPlotly({
      plotStateCases()
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