mapWidget <- function(){
  
  boxPlus(
    title = h4("State Map", align = "center"), 
    closable = FALSE, 
    width = NULL,
    status = "primary", 
    solidHeader = FALSE, 
    collapsible = TRUE,
    p(
      fluidRow(
        column(width = 6,
               radioButtons("mapType", "Map Type:",
                            c("Static" = "static",
                              "Thematic" = "thematic"))#,
               #switchInput(inputId = "staticSwitch", label = NULL, offLabel = "Static", onLabel = "Animation", inline = TRUE,  offStatus = "success", onStatus = "primary", size = "mini")
               ),
        column(width = 6,
               h4("Toggle Scale: "),
               switchInput(inputId = "proportionSwitch", label = NULL, offLabel = "Total Cases", onLabel = "Cases per 1000", inline = TRUE,  offStatus = "success", onStatus = "primary", size = "mini")
        )
      ),
      addSpinner(plotlyOutput("stateMap1", height = "500px"), spin = "double-bounce", color = "red"),
      #absolutePanel(bottom = 10, left = 30, sliderInput("SlideDate", "Date", min(as.Date(data.covid$Date)), max(as.Date(data.covid$Date)), value = max(as.Date(data.covid$Date)), step = 1))
      sliderInput("slideMapDate", value=NULL, min=0, max=0, label = "Date")
    )
  )
}