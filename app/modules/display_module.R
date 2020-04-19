# add all the display dynamic content here

displayConfirmedCases <- function(value){
  
  infoBox( value = h5('Confirmed Cases'), title = h4(value), width = 4, icon = icon("check-circle"), fill = TRUE, color = "black")
  
}

displayTotalDeaths <- function(value){
  
  infoBox( value = h5('Deaths'), title = h4(style="color: red", value), width = 4, icon = icon("dizzy"), fill = TRUE, color = "black")
  
}

displayTotalRecovered <- function(value){
  
  infoBox( value = h5('Recovered'), title = h4(value), width = 4, icon = icon("star-of-life"), fill = TRUE, color = "black")
  
}

displayTotalTested <- function(value){
  
  infoBox( value = h5('Tested'), title = h4(value), width = 6, icon = icon("check-circle"), fill = TRUE, color = "black")
  
}
displayTotalUnderInvestigation <- function(value){
  
  infoBox( value = h5('Under Investigation'), title = h4(value), width = 6, icon = icon("check-circle"), fill = TRUE, color = "black")
  
}

displayDeathsGenderDistribution <- function(vec.value){
  
  b1 <- infoBox(title = "Male Deaths", value =  vec.value[1], subtitle = paste(vec.value[2], "%", sep=""), width = 4, icon = icon("male"), fill = TRUE, color = "black")
  b2 <- infoBox(title = "Female Deaths", value =  vec.value[3], subtitle = paste(vec.value[4], "%", sep=""), width = 4, icon = icon("female"), fill = TRUE, color = "black")
  b3 <- infoBox(title = "Unknown", value =  vec.value[5], subtitle = paste(vec.value[6], "%", sep=""), width = 4, icon = icon("genderless"), fill = TRUE, color = "black")
  ls <- list(malebox =b1, fembox = b2, otherbox = b3)
  return(ls)
}

displayMaleCasesDistribution <- function(val, percentage){
  
  infoBox(title = "Male Confirmed Cases", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("male"), fill = TRUE, color = "black")
  
}
displayFemaleCasesDistribution <- function(val, percentage){
  
  infoBox(title = "Female Confirmed Cases", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("female"), fill = TRUE, color = "black")
  
}
displayOtherCasesDistribution <- function(val, percentage){
  
  infoBox(title = "Other", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("genderless"), fill = TRUE, color = "black")
  
}

displayCasesRaceDistribution <- function(data){
  total_cases = data[8, 2]; total_deaths = data[8, 3]; 
  
  hisp = infoBox("Hispanic", value = formatC(data[1, 2], big.mark = ","), subtitle = paste( round(data[1, 2]/total_cases *100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  white = infoBox("Non-Hispanic White", value = formatC(data[2, 2], big.mark = ","), subtitle = paste(round(data[2, 2]/total_cases*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  black = infoBox("Black/African American", value = formatC(data[3, 2], big.mark = ","), width = 12, subtitle = paste(round(data[3, 2]/total_cases*100, 2), "%", sep=""),fill = TRUE, color = "black")
  asian = infoBox("Non-Hispanic Asian", value = formatC(data[4, 2], big.mark = ","), subtitle = paste(round(data[4, 2]/total_cases*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  other = infoBox("Non-Hispanic Other", value = formatC(data[5, 2], big.mark = ","), subtitle = paste(round(data[5, 2]/total_cases*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  unknown = infoBox("Unknown/Missing", value = formatC(data[6, 2]+data[7, 2], big.mark = ","), width = 12, subtitle = paste(round((data[6, 2]+data[7, 2])/total_cases*100, 2), "%", sep=""),fill = TRUE, color = "black")
  #missing = infoBox("Missing", value = formatC(data[7, 2], big.mark = ","), width = 12, subtitle = paste(round(data[7, 2]/total_cases*100, 2), "%", sep=""),fill = TRUE)
  ls = list(hisp = hisp, white=white, black = black, asian = asian, other = other, unknown = unknown)
  return(ls)
  
}

displayDeathsRaceDistribution <- function(data){
  total_deaths = data[8, 3]; 
  #infoBox("Missing", value = data[7, 3], width = 12, subtitle = paste(data[7, 3]/total_deaths, "%", sep=""),fill = TRUE)
  hisp = infoBox("Hispanic", value = data[1, 3], subtitle = paste(round(data[1, 3]/total_deaths*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  white = infoBox("Non-Hispanic White", value = data[2, 3], subtitle = paste(round(data[2, 3]/total_deaths*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  black = infoBox("Black/African American", value = data[3, 3], width = 12, subtitle = paste(round(data[3, 3]/total_deaths*100, 2), "%", sep=""),fill = TRUE, color = "black")
  asian = infoBox("Non-Hispanic Asian", value = data[4, 3], subtitle = paste(round(data[4, 3]/total_deaths*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  other = infoBox("Non-Hispanic Other", value = data[5, 3], subtitle = paste(round(data[5, 3]/total_deaths*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  unknown = infoBox("Unknown/Missing", value = data[6, 3]+data[7, 3], width = 12, subtitle = paste(round((data[6, 3]+data[7, 3])/total_deaths*100, 2), "%", sep=""),fill = TRUE, color = "black")
  #missing = infoBox("Missing", value = data[7, 3], width = 12, subtitle = paste(data[7, 3]/total_deaths*100, "%", sep=""),fill = TRUE)
  ls = list(hisp = hisp, white=white, black = black, asian = asian, other = other, unknown = unknown)
  return(ls)
  
}


# for plotting deaths and confirmed cases

displayPrettyBarChartConfirmed <- function(new.data, start_date.ts, end_date.ts, cumm.cases){
  # replace the date for ggplot
  new.data$Date <- new.data$timeplot
  if(!cumm.cases){
    # make the cummulative plot
    #print(new.data)
    g <- ggplot(data = new.data, mapping = aes(x = Date, y = Cases)) + 
      theme(
        axis.text.x = element_text(angle = 90, colour = "white"), 
        axis.text.y = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.position="none",
        plot.background = element_rect(fill = "#282b29"), 
        panel.background = element_rect(fill = "#282b29"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) + 
      geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Cummulative Confirmed Cases")
    ggplotly(g, tooltip = c("Date", "Cases"))
  }else{
    # make the daily plot
    g <- ggplot(data = new.data, mapping = aes(x = Date, y = DailyCases)) + 
      theme(
        axis.text.x = element_text(angle = 90, colour = "white"), 
        axis.text.y = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.position="none",
        plot.background = element_rect(fill = "#282b29"), 
        panel.background = element_rect(fill = "#282b29"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) + 
      geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Daily Confirmed Cases")
    ggplotly(g, tooltip = c("Date", "DailyCases"))
  }
}

displayPrettyBarChartDeaths <- function(new.data, start_date.ts, end_date.ts, cumm.cases){
  # replace the date for ggplot
  new.data$Date <- new.data$timeplot
  if(!cumm.cases){
    # make the cummulative plot
    #print(new.data)
    g <- ggplot(data = new.data, mapping = aes(x = Date, y = Deaths)) + 
      theme(
        axis.text.x = element_text(angle = 90, colour = "white"), 
        axis.text.y = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.position="none",
        plot.background = element_rect(fill = "#282b29"), 
        panel.background = element_rect(fill = "#282b29"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) + 
      geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Cummulative Deaths")
    ggplotly(g, tooltip = c("Date", "Deaths"))
    #g + theme(axis.text.x = element_text(angle = 90)) 
  }else{
    # make the daily plot
    g <- ggplot(data = new.data, mapping = aes(x = Date, y = DailyDeaths)) + 
      theme(
        axis.text.x = element_text(angle = 90, colour = "white"), 
        axis.text.y = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.position="none",
        plot.background = element_rect(fill = "#282b29"), 
        panel.background = element_rect(fill = "#282b29"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) + 
      geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Daily Deaths")
    ggplotly(g, tooltip = c("Date", "DailyDeaths"))
  }
}