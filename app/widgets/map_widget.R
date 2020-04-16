mapWidget <- function(){
  
  boxPlus(
    title = h4("State Map", align = "center"), 
    closable = FALSE, 
    width = NULL,
    status = "primary", 
    solidHeader = FALSE, 
    collapsible = TRUE,
    p(
      leafletOutput("stateMap")
    )
  )
}