stateCountyWidget <- function(addMap){
  
  
  tabPanel( title ="State & County", icon = icon("globe-americas"),
            box(
              width = NULL,
              height = 80,
              title = h4("Data From Mass.gov", align = "center"),
              status = NULL
            ),
            
            fluidRow(
              # # A static infoBox
              #infoBox("Confirmed Cases", "--", width = 6, icon = icon("check-circle"), fill = TRUE),
              #infoBox("Deaths", "--", width = 6, icon = icon("dizzy"), fill = TRUE),
              # Dynamic infoBoxes
              infoBoxOutput("confirmedCasesSC"),
              infoBoxOutput("deathsSC"),
              infoBoxOutput("recoveredSC")
            ),
            
            # add map
            addMap,
            
            boxPlus(
              title = h4("Total Daily New Cases", align = "center"), 
              closable = FALSE, 
              width = NULL,
              status = "danger", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              p(
                switchInput(inputId = "stateCummDailySwitch", label = NULL, offLabel = "Cummulative", onLabel = "Daily", inline = TRUE,  offStatus = "success", onStatus = "primary", size = "mini"),
                switchInput(inputId = "deathConfirmedCases", label = NULL, offLabel = "Confirmed Cases", onLabel = "Deaths", inline = TRUE,  offStatus = "warning", onStatus = "danger", size = "mini"),
                # switch between these plots
                plotlyOutput("stateCasesPlot"),
                dateRangeInput("daterangeCummulativeState", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
              )
            ),
            
            # add county level plots
            
            box(
              width = NULL,
              height = 80,
              title = h4("County Level", align = "center"),
              status = NULL
            ),
            
            boxPlus(
              title = h5("Total Daily New Cases", align = "center"), 
              closable = FALSE, 
              width = NULL,
              status = "danger", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              p(
                switchInput(inputId = "countyCummDailySwitch", label = NULL, onLabel = "Cummulative", offLabel = "Daily", inline = TRUE,  onStatus = "danger", offStatus = "warning", size = "mini"),
                # switch between these plots
                plotOutput("countyInfectionsPlot"),
                dateRangeInput("daterange_cummulative_county", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
              )
            )
  )
  
}