getStaticMap <- function(){
  
  ma = getMapData()$ma
  ma.county.data = getMapData()$county
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
    )+
    geom_point(aes(x=long, y=lat, colour = County, size=Cases), data=ma.county.data, alpha=.5)+
    labs(title = 'Static Map', x = 'Longitude', y = 'Latitude')
  
  
  static <- ggplotly(static, tooltip = c("County", "Cases"))
  
  return(static)
}
