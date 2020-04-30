# put all data processing functions here

# convert first character to upper
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

getRaceData <- function(){
  
  #no preprocessing necessary
  race.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/RaceData.csv")
  return(race.data)
}

getCountyDataMap <- function(){
  county.data <- read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/CountyLevelData-NA-Removed.csv")
  return(county.data)
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
  
  library(stringr)
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

getStateData <- function(){

  # load the data files from guthub 
  state.data = read.csv("https://raw.githubusercontent.com/STAT691P-Project-Seminar/SIR-Models/master/data/StateLevelData.csv")
  #state.data = read.csv("../data/StateLevelData.csv")
  
  # add date time stamp
  
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
  my_date = anydate(ts + (24*60*60)) # add additional day
  last_updated = format(my_date, format="%B %d %Y") #paste(format(my_date,"%b"), as.numeric(format(my_date,"%d")), sep=" ")
  return(last_updated)
}

### process map data

getMapData <- function(){
  
  county.pop <- getCountyPop()
  ma.county.data <- getCountyDataMap()
  #View(ma.county.data)
  # case data (each row is unique county & data combination)
  #ma.county.data <- read.csv("data/macountydata.csv")
  ma.county.data <- ma.county.data[with(ma.county.data, order(County, Date)), ]
  #View(ma.county.data)
  # counties by state
  county_df <- map_data('county') 
  
  # just MA
  ma <- subset(county_df, region=="massachusetts")   #subset just for ma
  names(ma)[6] <- "county"
  
  # get "center" of each county
  cnames <- aggregate(cbind(long, lat) ~ county, data=ma, 
                      FUN=function(x)mean(range(x)))
  
  Barnstable.lat <- rep(-70.31052, 38); Barnstable.long <- rep(41.81733, 38)
  Barnstable.pop <- rep(county.pop[1, 2], 38)
  Berkshire.lat <- rep(-73.22114, 42); Berkshire.long <- rep(42.39315, 42)
  Berkshire.pop <- rep(county.pop[2, 2], 42)
  Bristol.lat <- rep(-71.10120, 38); Bristol.long <- rep(41.79441, 38)
  Bristol.pop <- rep(county.pop[3, 2], 38)
  Dukes.lat <- rep(-70.63997, 31); Dukes.long <- rep(41.39047, 31)
  Dukes.pop <- rep(county.pop[4, 2], 31)
  Essex.lat <- rep(-70.97514, 42); Essex.long <- rep(42.65384, 42)
  Essex.pop <- rep(county.pop[5, 2], 42)
  Franklin.lat <- rep(-72.61954, 34); Franklin.long <- rep(42.54211, 34)
  Franklin.pop <- rep(county.pop[6, 2], 34)
  Hampden.lat <- rep(-72.59948, 37); Hampden.long <- rep(42.17256, 37)
  Hampden.pop <- rep(county.pop[7, 2], 37)
  Hampshire.lat <- rep(-72.63099, 33); Hampshire.long <- rep(42.37023, 33)
  Hampshire.pop <- rep(county.pop[8, 2], 33)
  Middlesex.lat <- rep(-71.44784, 42); Middlesex.long <- rep(42.44185, 42)
  Middlesex.pop <- rep(county.pop[9, 2], 42)
  Nantucket.lat <- rep(-70.08133, 31);  Nantucket.long <- rep(41.31312, 31)
  Nantucket.pop <- rep(county.pop[10, 2], 31)
  Norfolk.lat <- rep(-71.20147, 42); Norfolk.long <- rep(42.15823, 42)
  Norfolk.pop <- rep(county.pop[11, 2], 42)
  Plymouth.lat <- rep(-70.81186, 37); Plymouth.long <- rep(41.96056, 37)
  Plymouth.pop <- rep(county.pop[12, 2], 37)
  Suffolk.lat <- rep(-71.10693, 42); Suffolk.long <- rep(42.29861, 42)
  Suffolk.pop <- rep(county.pop[13, 2], 42)
  Unknown.lat <- rep( 0, 38); Unknown.long <- rep(0, 38); Unknown.pop <- rep(1, 38)
  Worcester.lat <- rep(-71.89188, 42); Worcester.long <- rep(42.36736, 42)
  Worcester.pop <- rep(county.pop[14, 2], 42)
  
  lot <- c(Barnstable.lat, Berkshire.lat, Bristol.lat, Dukes.lat, Essex.lat, Franklin.lat, Hampden.lat, Hampshire.lat,
           Middlesex.lat, Nantucket.lat, Norfolk.lat, Plymouth.lat, Suffolk.lat, Unknown.lat, Worcester.lat)
  
  lat <- c(Barnstable.long, Berkshire.long, Bristol.long, Dukes.long, Essex.long, Franklin.long, Hampden.long, Hampshire.long,
           Middlesex.long, Nantucket.long, Norfolk.long, Plymouth.long, Suffolk.long, Unknown.long, Worcester.long)
  pop <- c(Barnstable.pop, Berkshire.pop, Bristol.pop, Dukes.pop, Essex.pop, Franklin.pop, Hampden.pop, Hampshire.pop,
           Middlesex.pop, Nantucket.pop, Norfolk.pop, Plymouth.pop, Suffolk.pop, Unknown.pop, Worcester.pop)
  
  #ma.county.data <- merge(ma.county.data, cnames)
  ma.county.data$lat <- lat
  ma.county.data$long <- lot
  ma.county.data$Population <- pop
  #remote the unkon
  ma.county.data <- ma.county.data[!ma.county.data$County=="Unknown", ]
  
  ma.county.data$timestamp <- sapply(ma.county.data$Date, getTimeStamp)
  ma.county.data$Date2 <- anydate(ma.county.data$timestamp + (24*60*60))
  #View(ma.county.data)
  
  return(list(ma = ma, county = ma.county.data))
  
}

