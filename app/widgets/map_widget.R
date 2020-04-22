mapWidget <- function(){
  
  boxPlus(
    title = h4("State Map", align = "center"), 
    closable = FALSE, 
    width = NULL,
    status = "primary", 
    solidHeader = FALSE, 
    collapsible = TRUE,
    p(
      switchInput(inputId = "staticSwitch", label = NULL, offLabel = "Static", onLabel = "Animation", inline = TRUE,  offStatus = "success", onStatus = "primary", size = "mini"),
      plotlyOutput("stateMap1")
      #plotOutput("stateMap2")
    )
  )
}