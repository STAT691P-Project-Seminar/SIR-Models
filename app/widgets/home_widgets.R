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
              box( title =  h4("Massachusetts Corona Virus Data", align = "center"), width = 12, fill = TRUE, background = "black")
            ),
            fluidRow(
              # A static infoBox
              #infoBox("Confirmed Cases", "--", width = 6, icon = icon("check-circle"), fill = TRUE),
              #infoBox("Deaths", "--", width = 6, icon = icon("dizzy"), fill = TRUE),
              # Dynamic infoBoxes
              infoBoxOutput("confirmedCases"),
              infoBoxOutput("deaths"),
              infoBoxOutput("totalRecovered")
            ),
            
            # infoBoxes with fill=TRUE
            # fluidRow(
            #   infoBox("Total Tested", "--", width = 4, icon = icon("vial"), fill = TRUE),
            #   infoBox("Total Under Investigation", 10 * 2, width = 4, icon = icon("file-medical"), fill = TRUE),
            #   infoBox("Total Recovered", "--", width = 4, icon = icon("star-of-life"), fill = TRUE),
            #   infoBoxOutput("totalTested"),
            #   infoBoxOutput("totalUnderinvestigation"),
            #   infoBoxOutput("totalRecovered")
            # ),
            
            #box(
            # width = NULL,
            # height = 80,
            # title = h4("Gender Distribution", align = "center"),
            # status = NULL
            #),
            fluidRow(
              box( title =  h4("Gender Statistics", align = "center"), width = 12, fill = TRUE, background =  "black")
            ),
            # infoBoxes with fill=TRUE
            fluidRow(
              #infoBox("Male Confirmed Cases", "--", subtitle = "%", width = 4, icon = icon("male"), fill = TRUE),
              #infoBox("Female Confirmed Cases", "--", subtitle = "%", width = 4, icon = icon("female"), fill = TRUE),
              #infoBox("Other Confirmed Cases", "--", width = 4, subtitle = "%", icon = icon("genderless"), fill = TRUE),
              infoBoxOutput("maleConfirmedCases"),
              infoBoxOutput("femaleConfirmedCases"),
              infoBoxOutput("otherConfirmedCases")
            ),
            
            fluidRow(
              infoBox("Male Deaths", "--", subtitle = "%", width = 4, icon = icon("male"), fill = TRUE, color = "black"),
              infoBox("Female Deaths", "--", subtitle = "%", width = 4, icon = icon("female"), fill = TRUE, color = "black"),
              infoBox("Other Deaths", "--", width = 4, subtitle = "%", icon = icon("genderless"), fill = TRUE, color = "black"),
              infoBoxOutput("maleDeaths"),
              infoBoxOutput("femaleDeaths"),
              infoBoxOutput("otherGenderDeaths")
            ),
            
            fluidRow(
              box( title =  h4("Race Statistics", align = "center"), width = 12, fill = TRUE, background = "black")
            ),
            
            fluidRow(
              column(
                width = 6,
                box( title =  h4("Confirmed Cases", align = "center"), width = 12, fill = TRUE, background = "black"),
                infoBox("Hispanic", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Non-Hispanic White", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Black/African American", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBox("Non-Hispanic Asian", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Non-Hispanic Other", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Unknown", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBox("Missing", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBoxOutput("hispanicCases"),
                infoBoxOutput("nonHispanicWhiteCases"),
                infoBoxOutput("blackCases"),
                infoBoxOutput("asianCases"),
                infoBoxOutput("otherCases"),
                infoBoxOutput("unknownCases"),
                infoBoxOutput("missingCases")
              ),
              column(
                width = 6,
                box( title =  h4("Deaths", align = "center"), width = 12, fill = TRUE, background = "black"),
                infoBox("Hispanic", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Non-Hispanic White", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Black/African American", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBox("Non-Hispanic Asian", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Non-Hispanic Other", "--", subtitle = "%", width = 12, fill = TRUE, color = "black"),
                infoBox("Unknown", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBox("Missing", "--", width = 12, subtitle = "%",fill = TRUE, color = "black"),
                infoBoxOutput("hispanicDeaths"),
                infoBoxOutput("nonHispanicWhiteDeaths"),
                infoBoxOutput("blackDeaths"),
                infoBoxOutput("asianDeaths"),
                infoBoxOutput("otherRaceDeaths"),
                infoBoxOutput("unknownDeaths"),
                infoBoxOutput("missingDeaths")

              )
            )
  )
}