library(modules) # must load
# load packages
packages.Self <- modules::use("core/libs.R")
packages.Self$getPackages("ui")
library(shiny); library(miceadds); library(shinydashboard); library(shinydashboardPlus); library(shinytoastr)
library(shinyWidgets); library(dashboardthemes); library(ggplot2); library(plotly); library(leaflet)

# source widgets
source.all("widgets/", grepstring="\\.R")


# Define UI for the application
ui <- dashboardPagePlus(
    enable_preloader = TRUE,
    loading_duration = 4,
    # dashboard header  begins here
    dashboardHeaderPlus(
      title = "MA Covid-19 Tracker",
      enable_rightsidebar = FALSE
    ),
    #dashboard header ends here
    
    #dashboard sidebar begins here
    dashboardSidebar(
      width = 0,
      sidebarMenu(
        id = "tabs",
        menuItem("MA Data", tabName = "database", icon = icon("database")),
        menuItem("Code", icon = icon("code"), tabName = "code"),
        menuItem("Doc", icon = icon("dochub"), tabName = "document")
      )
      
    ),
    #dashboard sidebar ends here
    
    #dashboard body starts here
    dashboardBody(
      ### changing theme
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      useToastr(), # for alert on data 
      
      # add css file here
      tags$head( tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      
      navbarPage("", 
                 position = c("fixed-bottom"),
                 collapsible = TRUE,
                 fluid = TRUE,
                 # home tab
                 
                 homeWidget(),
                
                 # state tab
                 stateCountyWidget(mapWidget()),
                 
                 # models tab
                 tabPanel(title = "Models", icon = icon("chart-line"),
                          
                          modelsSidebarPanel(),
                          modelsMainPanel()
                          
                          )
                 # news tab
                 #tabPanel(title = "News", icon = icon("rss"))
      ) # navbar page ends here
    ), # dashboard ends here
    
    box(
      width = NULL,
      height = 100,
      title = h4(""),
      status = NULL
    ),
    
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
