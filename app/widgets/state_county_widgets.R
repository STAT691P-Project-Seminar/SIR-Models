stateCountyWidget <- function(addMap){
  
  
  tabPanel( title ="State & County", icon = icon("globe-americas"),
            box(
              width = NULL,
              height = 80,
              title = h4("Massachusetts State Department Data", align = "center"),
              status = NULL
            ),
            
            fluidRow(
              # # A static infoBox
              infoBox("Confirmed Cases", "--", width = 6, icon = icon("check-circle"), fill = TRUE),
              infoBox("Deaths", "--", width = 6, icon = icon("dizzy"), fill = TRUE)
              # Dynamic infoBoxes
              #infoBoxOutput("confirmedCases")
              # infoBoxOutput("deaths")
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
                switchInput(inputId = "stateCummDailySwitch", label = NULL, onLabel = "Cummulative", offLabel = "Daily", inline = TRUE,  onStatus = "danger", offStatus = "warning", size = "mini"),
                # switch between these plots
                plotOutput("stateInfectionsPlot"),
                dateRangeInput("daterange_cummulative", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
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