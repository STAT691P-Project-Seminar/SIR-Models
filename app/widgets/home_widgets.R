# this will contain the home tab widgets

# not so mnay widgets so can combine them into a single function


homeWidget <- function(){
  
  tabPanel( title = "Home", icon=icon("home"),
            
            
            #box(
            #  width = NULL,
            #  height = 80,
            #  title = h4("Massachusetts Corona Virus Data", align = "center"),
            #  status = NULL
            #),
            fluidRow(
              infoBox( title =  h4("Massachusetts Corona Virus Data", align = "center"), width = 12, fill = TRUE, color = "red")
            ),
            fluidRow(
              # A static infoBox
              infoBox("Confirmed Cases", "--", width = 6, icon = icon("check-circle"), fill = TRUE),
              infoBox("Deaths", "--", width = 6, icon = icon("dizzy"), fill = TRUE),
              # Dynamic infoBoxes
              infoBoxOutput("confirmedCases"),
              infoBoxOutput("deaths")
            ),
            
            # infoBoxes with fill=TRUE
            fluidRow(
              infoBox("Total Tested", "--", width = 4, icon = icon("vial"), fill = TRUE),
              infoBox("Total Under Investigation", 10 * 2, width = 4, icon = icon("file-medical"), fill = TRUE),
              infoBox("Total Recovered", "--", width = 4, icon = icon("star-of-life"), fill = TRUE),
              infoBoxOutput("totalTested"),
              infoBoxOutput("totalUnderinvestigation"),
              infoBoxOutput("totalRecovered")
            ),
            
            #box(
            # width = NULL,
            # height = 80,
            # title = h4("Gender Distribution", align = "center"),
            # status = NULL
            #),
            fluidRow(
              infoBox( title =  h4("Gender Distribution", align = "center"), width = 12, fill = TRUE, color = "red")
            ),
            # infoBoxes with fill=TRUE
            fluidRow(
              infoBox("Male Deaths", "--", subtitle = "%", width = 4, icon = icon("male"), fill = TRUE),
              infoBox("Female Deaths", "--", subtitle = "%", width = 4, icon = icon("female"), fill = TRUE),
              infoBox("Other Deaths", "--", width = 4, subtitle = "%", icon = icon("genderless"), fill = TRUE),
              infoBoxOutput("maleDeaths"),
              infoBoxOutput("femaleDeaths"),
              infoBoxOutput("otherDeaths")
            ),
            fluidRow(
              infoBox("Male Confirmed Cases", "--", subtitle = "%", width = 4, icon = icon("male"), fill = TRUE),
              infoBox("Female Confirmed Cases", "--", subtitle = "%", width = 4, icon = icon("female"), fill = TRUE),
              infoBox("Other Confirmed Cases", "--", width = 4, subtitle = "%", icon = icon("genderless"), fill = TRUE),
              infoBoxOutput("maleConfirmedCases"),
              infoBoxOutput("femaleConfirmedCases"),
              infoBoxOutput("otherConfirmedCases")
            )
  )
}