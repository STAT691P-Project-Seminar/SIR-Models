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

# date_char has format month/day/year
getTimeSIR <- function(date_char2){
  
  ls = as.list(str_split(date_char2 , '/'))[[1]] #month, day, year
  date_char_format = paste(ls[3], "-", ls[1], "-", ls[2], sep='')
  
  return(date_char_format)
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
  ma.table = table(ma.county.data$County)[[1]]
  #View(ma.county.data)
  # counties by state
  county_df <- map_data('county') 
  
  # just MA
  ma <- subset(county_df, region=="massachusetts")   #subset just for ma
  names(ma)[6] <- "county"
  
  # get "center" of each county
  cnames <- aggregate(cbind(long, lat) ~ county, data=ma, 
                      FUN=function(x)mean(range(x)))
  
  Barnstable.lat <- rep(-70.31052, ma.table); Barnstable.long <- rep(41.81733, ma.table)
  Barnstable.pop <- rep(county.pop[1, 2], ma.table)
  Berkshire.lat <- rep(-73.22114, ma.table); Berkshire.long <- rep(42.39315, ma.table)
  Berkshire.pop <- rep(county.pop[2, 2], ma.table)
  Bristol.lat <- rep(-71.10120, ma.table); Bristol.long <- rep(41.79441, ma.table)
  Bristol.pop <- rep(county.pop[3, 2], ma.table)
  Dukes.lat <- rep(-70.63997, ma.table); Dukes.long <- rep(41.39047, ma.table)
  Dukes.pop <- rep(county.pop[4, 2], ma.table)
  Essex.lat <- rep(-70.97514, ma.table); Essex.long <- rep(42.65384, ma.table)
  Essex.pop <- rep(county.pop[5, 2], ma.table)
  Franklin.lat <- rep(-72.61954, ma.table); Franklin.long <- rep(42.54211, ma.table)
  Franklin.pop <- rep(county.pop[6, 2], ma.table)
  Hampden.lat <- rep(-72.59948, ma.table); Hampden.long <- rep(42.17256, ma.table)
  Hampden.pop <- rep(county.pop[7, 2], ma.table)
  Hampshire.lat <- rep(-72.63099, ma.table); Hampshire.long <- rep(42.37023, ma.table)
  Hampshire.pop <- rep(county.pop[8, 2], ma.table)
  Middlesex.lat <- rep(-71.44784, ma.table); Middlesex.long <- rep(42.44185, ma.table)
  Middlesex.pop <- rep(county.pop[9, 2], ma.table)
  Nantucket.lat <- rep(-70.08133, ma.table);  Nantucket.long <- rep(41.31312, ma.table)
  Nantucket.pop <- rep(county.pop[10, 2], ma.table)
  Norfolk.lat <- rep(-71.20147, ma.table); Norfolk.long <- rep(42.15823, ma.table)
  Norfolk.pop <- rep(county.pop[11, 2], ma.table)
  Plymouth.lat <- rep(-70.81186, ma.table); Plymouth.long <- rep(41.96056, ma.table)
  Plymouth.pop <- rep(county.pop[12, 2], ma.table)
  Suffolk.lat <- rep(-71.10693, ma.table); Suffolk.long <- rep(42.29861, ma.table)
  Suffolk.pop <- rep(county.pop[13, 2], ma.table)
  Unknown.lat <- rep( 0, ma.table); Unknown.long <- rep(0, ma.table); Unknown.pop <- rep(1, ma.table)
  Worcester.lat <- rep(-71.89188, ma.table); Worcester.long <- rep(42.36736, ma.table)
  Worcester.pop <- rep(county.pop[14, 2], ma.table)
  
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

# SIR Simulation Model

sir_sim2 = function(N, alpha, beta, initInf, TS)
{ 
  test.perc <- .0025
  test.till <- (length(seq(as.Date("2020-4-30"), as.Date(Sys.time()) , by = "+1 day")) -1) * 10000
  N <- (N*test.perc) + 275000 + test.till 
  delta = 1
  times = seq(0, TS, by = delta)
  Date = as.Date(vector())
  S_t = vector()
  I_t = vector()
  R_t = vector()
  I_t[1] = initInf
  S_t[1] = N - initInf
  R_t[1] = 0
  Date[1] = as.Date("2020-03-02")
  
  for(i in 2:length(times))
  {
    S_t[i] = S_t[i-1] * (1 - (delta * beta * I_t[i-1] * (1/N)))
    R_t[i] = R_t[i-1] + delta * alpha * I_t[i-1]
    I_t[i] = I_t[i-1] + ((beta * (S_t[i-1]/N) * I_t[i-1]) - (alpha * I_t[i-1])) * delta
    Date[i] = Date[i-1] + 1
  }
  
  I_tcom = I_t + R_t
  df1 = as.data.frame(cbind(times, S_t, R_t, I_t, I_tcom))
  df1$I_tcom = round(df1$I_tcom, 0)
  df1$I_t = round(df1$I_t, 0)
  df1$Date = Date
  #df1$Date = as.numeric(as.POSIXct(df1$Date))
  #df1$Date = getPlottableDates(df1$Date)
  
  return(df1)
  
}

sir_simulate <- function(beta, gamma, N, I0, time) {
  test.perc <- .0025
  # 10000 test per day
  test.till <- (length(seq(as.Date("2020-4-30"), as.Date(Sys.time()) , by = "+1 day")) -1) * 10000
  N <- (N*test.perc) + 275000 + test.till 
  R0 = 0; S0 = N - I0
  print(time)
  times <- c(1:time)
  beta <- beta/N
  sir_equations <- function(time, variables, parameters) 
  {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))})
  }
  
  parameters_values <- c(beta  = beta, gamma = gamma)
  
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values)
  df <- data.frame(out)
  st <- as.Date("2020-3-2")
  ll <- seq(st, by = "+1 day", length.out = time)
  df <- data.frame("time" = df$time, "Susceptible" = round(df$S, 0), "Infected" = round(df$I, 0), "Recovered" = round(df$R, 0), "Date" = ll)
  # returning the output:
  return (df)
}

sir_sim3 = function(N, alpha, beta, initInf, TS)
{ 
  test.perc <- .0025
  test.till <- (length(seq(as.Date("2020-4-30"), as.Date(Sys.time()) , by = "+1 day")) -1) * 10000
  N <- (N*test.perc) + 275000 + test.till 
  delta = 1
  times = seq(0, TS, by = delta)
  Date = as.Date(vector())
  S_t = vector()
  I_t = vector()
  R_t = vector()
  I_t[1] = initInf/N
  S_t[1] = (N - initInf)/N
  R_t[1] = 0
  Date[1] = as.Date("2020-03-02")
  
  for(i in 2:length(times))
  {
    S_t[i] = S_t[i-1] * (1 - (delta * beta * I_t[i-1]))
    R_t[i] = R_t[i-1] + delta * alpha * I_t[i-1]
    I_t[i] = I_t[i-1] + ((beta * S_t[i-1] * I_t[i-1]) - (alpha * I_t[i-1])) * delta
    Date[i] = Date[i-1] + 1
  }
  
  I_tcom = I_t + R_t
  df1 = as.data.frame(cbind(times, S_t, R_t, I_t, I_tcom))
  df1$I_tcom = round(df1$I_tcom, 2)
  df1$I_t = round(df1$I_t, 2)
  df1$Date = Date
  #df1$Date = as.numeric(as.POSIXct(df1$Date))
  #df1$Date = getPlottableDates(df1$Date)
  
  return(df1)
  
}
