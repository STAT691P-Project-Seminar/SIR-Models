rm(list = ls())
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(miceadds) # allows the sourcing of all files
# import widgets
source.all("../widgets/", grepstring="\\.R")


# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  
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
                          
                          ),
                 # news tab
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
