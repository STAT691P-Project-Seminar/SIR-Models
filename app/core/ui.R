rm(list = ls())
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)


# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  
    # dashboard header  begins here
    dashboardHeaderPlus(
      title = "MA Covid-19 Tracker",
      enable_rightsidebar = FALSE
      # set to true if desired
      #rightSidebarIcon = "gears"
      
    ),
    
    # 
    dashboardSidebar(
      
      sidebarUserPanel(
        "About"
      ),
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("MA Data", tabName = "database", icon = icon("database")),
        menuItem("Code", icon = icon("code"), tabName = "code"),
        menuItem("Doc", icon = icon("dochub"), tabName = "document")
      )
      
    ),
    
    dashboardBody(
      
      navbarPage("", 
                 position = c("fixed-bottom"),
                 responsive = TRUE,
                 collapsible = FALSE,
                 fluid = TRUE,
                 
                 tabPanel( title = "Home", icon=icon("home"),
                           
                          
                           box(
                             width = NULL,
                             title = h4("Massachusetts Corona Virus Data", align = "center"),
                             status = NULL
                           )
                 ),
                
                 tabPanel( title ="State", icon = icon("globe-americas"),
                           
                           
                           boxPlus(
                             title = h3("State Level Cummulative Infections", align = "center"), 
                             closable = FALSE, 
                             width = NULL,
                             status = "danger", 
                             solidHeader = FALSE, 
                             collapsible = TRUE,
                             p(
                               plotOutput("stateInfectionsPlot"),
                               dateRangeInput("daterange1", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
                               )
                           ),
                           
                           boxPlus(
                             title = div(style="text-align:center","State Level Daily Infections"), 
                             closable = FALSE, 
                             width = NULL,
                             status = "danger", 
                             solidHeader = FALSE, 
                             collapsible = TRUE,
                             enable_dropdown = TRUE,
                             p(
                               plotOutput("stateInfectionsDailyPlot"),
                               dateRangeInput("daterange_state_daily", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
                             )
                           )
                           
                           
                           ),
                 
                 tabPanel(title = "County", icon = icon("map-marked"),
                          
                          
                          boxPlus(
                            title = div(style="text-align:center","County Level Cummulative Infections"), 
                            closable = FALSE, 
                            width = NULL,
                            status = "danger", 
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            enable_dropdown = TRUE,
                            p(
                              plotOutput("countyInfectionsPlot"),
                              dateRangeInput("daterange_county_cumm", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
                            )
                          ),
                          
                          boxPlus(
                            title = div(style="text-align:center","County Level Daily Infections"), 
                            closable = FALSE, 
                            width = NULL,
                            status = "danger", 
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            enable_dropdown = TRUE,
                            p(
                              plotOutput("countyInfectionsDailyPlot"),
                              dateRangeInput("daterange_county_daily", "Date range:", start = "2020-01-02", end   = "2020-12-31", format = "mm/dd/yy")
                            )
                          )
                          
                          
                          ),
                 tabPanel(title = "Models", icon = icon("omega")),
                 tabPanel(title = "News", icon = icon("rss"))
      ) # navbar page ends here
    ), # dashboard ends here
    
    rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Tab 1",
        icon = "desktop",
        active = TRUE,
        sliderInput(
          "obs",
          "Number of observations:",
          min = 0, max = 1000, value = 500
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tab 2",
        textInput("caption", "Caption", "Data Summary")
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "paint-brush",
        title = "Tab 3",
        numericInput("obs", "Observations:", 10, min = 1, max = 100)
      )
    )
    
  
)

return(ui)
