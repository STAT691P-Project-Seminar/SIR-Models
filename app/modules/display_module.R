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
  
  per.1000 <- (as.numeric(sub(",", '', val))/3058816)*1000
  #infoBox(title = "Male Confirmed Cases", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("male"), fill = TRUE, color = "black")
  infoBox(title = "Male Confirmed Cases", value =  val, subtitle = paste(round(per.1000, 0), " cases per 1000 men"), width = 4, icon = icon("male"), fill = TRUE, color = "black")
  
}
displayFemaleCasesDistribution <- function(val, percentage){
  per.1000 <- (as.numeric(sub(",", '', val))/3290281)*1000
  #infoBox(title = "Female Confirmed Cases", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("female"), fill = TRUE, color = "black")
  infoBox(title = "Female Confirmed Cases", value =  val, subtitle = paste(round(per.1000, 0), " cases per 1000 women"), width = 4, icon = icon("female"), fill = TRUE, color = "black")
  
}
displayOtherCasesDistribution <- function(val, percentage){
  
  infoBox(title = "Other", value =  val, subtitle = paste(percentage, "%", sep=""), width = 4, icon = icon("genderless"), fill = TRUE, color = "black")
  
}

displayCasesRaceDistribution <- function(data){
  total_cases = data[8, 2]; total_deaths = data[8, 3]; 
  
  hisp = infoBox("Hispanic", value = formatC(data[1, 2], big.mark = ","), subtitle = paste( round( (data[1, 2]/847777.9)*1000, 0), " cases per 1000 Hispanics", sep=""), width = 12, fill = TRUE, color = "black")
  white = infoBox("Non-Hispanic White", value = formatC(data[2, 2], big.mark = ","), subtitle = paste(round( (data[2, 2]/4921247)*1000, 0), " cases per 1000 Non-Hispanic Whites", sep=""), width = 12, fill = TRUE, color = "black")
  black = infoBox("Black/African American", value = formatC(data[3, 2], big.mark = ","), width = 12, subtitle = paste(round((data[3, 2]/613432.8)*1000, 0), " cases per 1000 Blacks/African Americans", sep=""),fill = TRUE, color = "black")
  asian = infoBox("Non-Hispanic Asian", value = formatC(data[4, 2], big.mark = ","), subtitle = paste(round((data[4, 2]/489367.7)*1000, 0), " case per 1000 Non-Hispanic Asians", sep=""), width = 12, fill = TRUE, color = "black")
  other = infoBox("Non-Hispanic Other", value = formatC(data[5, 2], big.mark = ","), subtitle = paste(round(data[5, 2]/total_cases*100, 2), "%", sep=""), width = 12, fill = TRUE, color = "black")
  unknown = infoBox("Unknown/Missing", value = formatC(data[6, 2]+data[7, 2], big.mark = ","), width = 12, subtitle = paste(round((data[6, 2]+data[7, 2])/total_cases*100, 2), "%", sep=""),fill = TRUE, color = "black")
  #missing = infoBox("Missing", value = formatC(data[7, 2], big.mark = ","), width = 12, subtitle = paste(round(data[7, 2]/total_cases*100, 2), "%", sep=""),fill = TRUE)
  ls = list(hisp = hisp, white=white, black = black, asian = asian, other = other, unknown = unknown)
  return(ls)
  
}

displayDeathsRaceDistribution <- function(data){
  total_deaths = data[8, 3]; 
  #infoBox("Missing", value = data[7, 3], width = 12, subtitle = paste(data[7, 3]/total_deaths, "%", sep=""),fill = TRUE)
  hisp = infoBox("Hispanic", value = data[1, 3], subtitle = paste(round((data[1, 3]/847777.9)*10000, 0), " deaths per 10000 Hispanics", sep=""), width = 12, fill = TRUE, color = "black")
  white = infoBox("Non-Hispanic White", value = data[2, 3], subtitle = paste(round((data[2, 3]/4921247)*10000, 0), " deaths per 10000 Non-Hispanic Whites", sep=""), width = 12, fill = TRUE, color = "black")
  black = infoBox("Black/African American", value = data[3, 3], width = 12, subtitle = paste(round((data[3, 3]/613432.8)*10000, 0), " deaths per 10000 Blacks/African Americans", sep=""),fill = TRUE, color = "black")
  asian = infoBox("Non-Hispanic Asian", value = data[4, 3], subtitle = paste(round((data[4, 3]/489367.7)*10000, 0), " death per 10000 Non-Hispanic Asians", sep=""), width = 12, fill = TRUE, color = "black")
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

displayPrettyChartCounty <- function(new.data2, start_date.ts2, end_date.ts2){
  # replace the date for ggplot
  new.data2$Date <- new.data2$timeplot
  #print(new.data2)
  g <- ggplot(data = new.data2, mapping = aes(x = Date, y = Cases)) + 
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
      geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Confirmed Cases")
    ggplotly(g, tooltip = c("Date", "Cases"))
}

displayPrettyChartCounty_pop <- function(new.data2, start_date.ts2, end_date.ts2){
  # replace the date for ggplot
  new.data2$Date <- new.data2$timeplot
  #print(new.data2)
  g <- ggplot(data = new.data2, mapping = aes(x = Date, y = Cases)) + 
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
    geom_bar(stat = "identity", fill = "#91341d") + xlab("") + ylab("Confirmed Cases Per 1000 Pop")
  ggplotly(g, tooltip = c("Date", "Cases"))
}
displayPrettyChart_SIR <- function(new.data3, state.data3){
  new.data3$Cases = new.data3$I_tcom
 g <- ggplot(data = new.data3, mapping = aes(x = Date, y = Cases)) + 
    theme(
      axis.text.x = element_text(colour = "white"), 
      axis.text.y = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white"),
      plot.title = element_text(colour = "white"),
      legend.position ="none",
      plot.background = element_rect(fill = "#282b29"), 
      panel.background = element_rect(fill = "#282b29"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) + 
    geom_line(colour = "#CC0000") + geom_point(state.data3, mapping = aes(x = Date, y = Cases), colour = "#CC0000") + 
   xlab("") + ylab("Infected + Recovered") + labs(title = "Dots: Observed Data and Smooth line: Simulated Data") 
 ggplotly(g, tooltip = c("Date", "Cases"))
}

# "#56B4E9"

displayPrettyChart_SIR2 <- function(new.data4){
  new.data4$Cases = new.data4$I_t
 g <- ggplot(data = new.data4, mapping = aes(x = Date, y = Cases)) + 
    theme(
      axis.text.x = element_text(colour = "white"), 
      axis.text.y = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white"),
      legend.position="none",
      plot.background = element_rect(fill = "#282b29"), 
      panel.background = element_rect(fill = "#282b29"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) + 
    geom_line(colour = "#CC0000") + xlab("") + ylab("Number of Infected")
 ggplotly(g, tooltip = c("Date", "Cases"))
}

displayPrettySIRSimulation <- function(sim.data){
  colors<-c("Susceptible"="blue","Infected"="red","Recovered"="green")
    g <- ggplot(data = sim.data) + geom_line(aes(time, Susceptible, color = "Susceptible", label1 = Date)) + 
      geom_line(aes(time, Infected, color = "Infected", label1 = Date)) + 
      geom_line(aes(time, Recovered, color = "Recovered", label1 = Date)) + ylab("")+
      labs( x = "Day", color = "Legend")+
      scale_color_manual(values = colors) + theme(legend.position = c(0.14, 0.86))+
    theme(
      axis.text.x = element_text(colour = "white"), 
      axis.text.y = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white"),
      plot.background = element_rect(fill = "#282b29"), 
      panel.background = element_rect(fill = "#282b29"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  ggplotly(g, tooltip = c("Date", "Susceptible", "Infected", "Recovered"))
}
