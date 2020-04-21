stateCountyWidget <- function(addMap){
  
  county_names = c("Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", "Franklin", "Hampden", 
                   "Hampshire", "Middlesex", "Nantucket", "Norfolk", "Plymouth", "Suffolk", "Worcester", 
                   "Unknown")
  county_namesp = county_names[-15]
  tabPanel( title ="State & County", icon = icon("globe-americas"),
            box(
              width = NULL,
              height = 80,
              title = h4("Data From mass.gov", align = "center"),
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
            
            box(
              width = NULL,
              height = 80,
              title = h4("State Level", align = "center"),
              status = NULL
            ),
            
            boxPlus(
              title = h4("Cummulative/Daily New Cases", align = "center"), 
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
              title = h4("Cummulative/Daily New Cases", align = "center"), 
              closable = FALSE, 
              width = NULL,
              status = "danger", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              p(
                selectInput("county",
                            "County:",
                            county_names,
                            selected = "Barnstable"),
                switchInput(inputId = "countyCummDailySwitch", label = NULL, onLabel = "Daily", offLabel = "Cummulative", inline = TRUE,  onStatus = "danger", offStatus = "warning", size = "mini"),
                # switch between these plots
                plotlyOutput("countyInfectionsPlot"),
                dateRangeInput("daterange_cummulative_county", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
              )
            ),
            
            boxPlus(
              title = h4("Cummulative/Per 1000 Pop New Cases", align = "center"), 
              closable = FALSE, 
              width = NULL,
              status = "danger", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              p(
                selectInput("county_p",
                            "County:",
                            county_namesp,
                            selected = "Barnstable"),
                switchInput(inputId = "countyPopSwitch", label = NULL, offLabel = "Total Pop", onLabel = "Per 1000 Pop", inline = TRUE,  offStatus = "success", onStatus = "primary", size = "mini"),
                # switch between these plots
                plotlyOutput("countyInfectionsPlot_P"),
                dateRangeInput("daterange_cummulative_county_P", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
              )
            )
  )
  
}
