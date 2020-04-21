# put all data processing functions here

getRaceData <- function(){
  
  #no preprocessing necessary
  race.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/RaceData.csv")
  return(race.data)
}

getCountyData <- function(){
  county.data <- read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/CountyLevel.csv")
  return(county.data)
}

getCountyPop <- function(){
  county.pop <- read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/CountyLevel_Population.csv")
  return(county.pop)
}

getCountyNames1 = function()
{
  county.names1 = names(getCountyData())[-1]
  
  return(county.names1)
}

getCountyNames2 = function()
{
  county.names2 = names(getCountyData())[-c(1, 16)]
  
  return(county.names2)
}

convert_daily = function(cols_df)
{ vec = vector()
vec[1] = cols_df[1]
len = length(cols_df)-1
for(i in 1:len)
{
  vec[i+1] = cols_df[i+1] - cols_df[i]
}

return(vec)

}

county_level_convert = function(county_df)
{ county_df = county_df[,-c(1)]
nlen = ncol(county_df)
nrows = nrow(county_df)
vec_req = data.frame(matrix(nrow = nrows, ncol = nlen))
for(i in 1:nlen)
{
  vec_req[,i] = convert_daily(county_df[,i])
}

names(vec_req) = getCountyNames1()
vec_req = cbind(getCountyData()[,1], vec_req)
names(vec_req)[1] = "Date"

return(vec_req)

}

convert_pop = function(county_df2, mass_pop)
{ county_df2 = county_df2[,-c(1)]
nlen2 = 14  
nrows2 = nrow(county_df2)
vec_req2 = data.frame(matrix(nrow = nrows2, ncol = nlen2))
for(i in 1:nlen2)
{
  vec_req2[,i] = (county_df2[,i]/mass_pop[i,2])*1000
}

names(vec_req2) = getCountyNames2()
vec_req2 = cbind(getCountyData()[,1], vec_req2)
names(vec_req2)[1] = "Date"
return(vec_req2)
}


getCountyData_TS = function()
{
  county.data <- read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/CountyLevel.csv")
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
  
  county.data$timestamp <- sapply(county.data$Date, getTimeStamp)
  county.data$timeplot <- sapply(county.data$timestamp, getPlottableDates)
  county.data$timeplot <- factor(county.data$timeplot, levels = county.data$timeplot) # this prevents ordering
  
  return(county.data)
}

getCountyData_Daily = function()
{
  county.data.daily = county_level_convert(getCountyData())
  
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
  
  county.data.daily$timestamp <- sapply(county.data.daily$Date, getTimeStamp)
  county.data.daily$timeplot <- sapply(county.data.daily$timestamp, getPlottableDates)
  county.data.daily$timeplot <- factor(county.data.daily$timeplot, levels = county.data.daily$timeplot) # this prevents ordering
  
  return(county.data.daily)
}

getCountyData_Pop = function()
{
  county.pop = convert_pop(getCountyData(), getCountyPop())
  
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
  
  county.pop$timestamp <- sapply(county.pop$Date, getTimeStamp)
  county.pop$timeplot <- sapply(county.pop$timestamp, getPlottableDates)
  county.pop$timeplot <- factor(county.pop$timeplot, levels = county.pop$timeplot) # this prevents ordering
  
  return(county.pop)
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

getLastUpdated <- function(){
  
  ts = getStateData()$timestamp[dim(getStateData())[1]]
  my_date = anytime(ts + (24*60*60)) # add additional day
  last_updated = format(my_date, format="%B %d %Y") #paste(format(my_date,"%b"), as.numeric(format(my_date,"%d")), sep=" ")
  return(last_updated)
}
