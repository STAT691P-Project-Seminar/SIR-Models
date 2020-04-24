getStaticMap <- function(switchPop){
  
  ma = getMapData()$ma
  ma.county.data = getMapData()$county
  ma.county.data$Cases.Per.1000 <- round((ma.county.data$Cases/ma.county.data$Population * 1000), 0)
  static <- ggplot(ma, aes(x=long, y=lat)) + geom_polygon(aes(group=group), colour='white') +
    theme(
      axis.text.x = element_text(colour = "white"),
      axis.text.y = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      plot.background = element_rect(fill = "#282b29"),
      panel.background = element_rect(fill = "#282b29"),
      legend.position="none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) + labs(x = 'Longitude', y = 'Latitude')
  
  if(switchPop){
    static <- static + geom_point(aes(x=long, y=lat, colour = County, size=Cases.Per.1000), data=ma.county.data, alpha=.5)
  }else{
    static <- static + geom_point(aes(x=long, y=lat, colour = County, size=Cases), data=ma.county.data, alpha=.5)
  }
    
  
  static <- ggplotly(static, tooltip = c("County", "Cases", "Cases.Per.1000"))
  
  return(static)
}
