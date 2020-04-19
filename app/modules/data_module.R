# put all data processing functions here

getRaceData <- function(){
  
  #no preprocessing necessary
  race.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/RaceData.csv")
  return(race.data)
}

getStateData <- function(){

  # load the data files from guthub 
  state.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/StateLevelData.csv")
  #state.data = read.csv("../data/StateLevelData.csv")
  
  # add date time stamp
  # date_char has format month/day/year
  getTimeStamp <- function(date_char){
    
    ls = as.list(str_split(date_char , '/'))[[1]] #month, day, year
    date_char_format = paste(ls[3], "-", ls[1], "-", ls[2], sep='')
    time_stamp = as.numeric(as.POSIXct(as.Date(date_char_format)))
    return(time_stamp)
  }
  
  # this returns dates suitable for plotting
  getPlottableDates <- function(time_stamp){
    my_date = anytime(time_stamp + (24*60*60)) # add additional day
    new_date = paste(format(my_date,"%b"), as.numeric(format(my_date,"%d")), sep=" ")
    return(new_date)
  }
  
  state.data$timestamp <- sapply(state.data$Date, getTimeStamp)
  state.data$timeplot <- sapply(state.data$timestamp, getPlottableDates)
  state.data$timeplot <- factor(state.data$timeplot, levels = state.data$timeplot) # this prevents ordering
  
  # add the daily infections and deaths
  DailyCases <- c(state.data$Cases)[1]
  DailyDeaths <- c(state.data$Deaths)[1]
  for (row in 2:dim(state.data)[1]){ 
    DailyCases = c(DailyCases, (state.data$Cases[row]-state.data$Cases[row-1]) ) 
    DailyDeaths = c(DailyDeaths, (state.data$Deaths[row]-state.data$Deaths[row-1]) ) 
    }
  state.data$DailyCases = DailyCases
  state.data$DailyDeaths = DailyDeaths
  
  return(state.data)
}