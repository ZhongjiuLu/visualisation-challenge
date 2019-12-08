#### Module Weather ####
# This module supports the Weather tab with:
#  - Loading all the required data for:
#     - Weather Chart
#     - Claims Chart
#     - Weather Events Feed
#  - creating the plotting functions for
#     - Weather Chart
#     - Claims Chart

#### Libraries ####
# This module uses the following libraries, which are loaded in the global.R file:
#library(ggplot2) # for charts
#library(reshape2) # for transforming data into the right shape for charts

#### Load Data ####
# weather data sourced from: https://www.metoffice.gov.uk/weather/
# this data is only used in this page, so it is loaded here instead of in global.R

df_Weather <- read.csv("./www/Weather Data/Weather.csv")
df_Weather_Newsfeed <- read.csv("./www/Weather Data/Weather_Newsfeed.csv")

#### Weather Chart ####

fn.plot.Weather <- function(df_W = df_Weather){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function generates the data for the chart of weather data
# inputs - df_W, weather data downloaded from met office, formatted outside of the app
# returns - p, a ggplot of the weather data
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  df_W$rownum <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  df_W$Month  <- factor(df_W$Month, levels = unique(df_W$Month))
  df_Rainfall <- melt(df_W[,c("Month", "Rainfall_mm_LTA", "Rainfall_mm_2018")],id.vars = "Month")
  
  p <- ggplot()+
    geom_bar(
      data = df_Rainfall
      ,aes(x = Month, y = value, group = variable, fill = variable)
      ,position="dodge"
      ,stat = "identity"
      #,fill = rep(c("blue", "light blue"),12)
      ,colour = "#404040"
      ,width = .80
      #,show.legend = rep(c(TRUE,TRUE),12)
    )+
    scale_fill_manual(name = "", values = c("blue", "light blue"))+
    geom_line(data = df_W, aes(x = Month, y = Mean_Temp_LTA*8  + 200, group = 1, colour = "Long Term Avg"), size = 1)+
    geom_line(data = df_W, aes(x = Month, y = Max_Temp_2018*8  + 200, group = 1, colour = "2018 Max"), size = 1)+
    geom_line(data = df_W, aes(x = Month, y = Mean_Temp_2018*8 + 200, group = 1, colour = "2018 Avg"), size = 1)+
    geom_line(data = df_W, aes(x = Month, y = Min_Temp_2018*8  + 200, group = 1, colour = "2018 Min"), size = 1)+
    scale_colour_manual(
      name = "Temp"
      ,values = c(
        "Long Term Avg" = "black"
        ,"2018 Max" = "red"
        ,"2018 Avg" = "brown"
        ,"2018 Min" = "blue")
    )+
    # the following labels would be generated automatically using lapply() if we were making this page up-to-date
    geom_text(aes(x = "Feb", y = 140), label = "Snow and Low Temperatures", angle = 0, nudge_x = 1, nudge_y = 15)+
    scale_y_continuous(
      name = "Rainfall /mm"
      ,limits = c(0,200+8*30)
      ,breaks = c(0,50,100,150)
      ,sec.axis = sec_axis(~./8-25,name = "Temperature /oC", breaks = c(-10,0,10,20,30))
      )+
    ggtitle("Long Term Average and 2018 Weather")+
    theme(panel.background = element_blank()
          ,axis.line = element_line(colour = "#404041")
          ,axis.title.y = element_text(hjust=0.1)
          ,plot.title = element_text(hjust = 0.5)
          ,legend.position = "bottom"
          )
  print(p)
}

# function testing
# fn.plot.Weather()

#### Claims Chart ####
fn.plot.Weather.Claims <- function(df_C = df_Claims) {
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function generates the data for the chart of weather Claims
# inputs - df_Claims = input table loaded from .csv in global.R
# returns - p, a ggplot of the weather data
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  df_Claims_Weather <- df_C[
    (df_C$Version.Name == "Actual") &
      (df_C$Period > "201800") &
      (df_C$Period < "201900") &
      (df_C$Peril == "Weather") &
      (df_C$Segment == "Total")
    ,
    ]
  df_Claims_Weather$Date <- factor(
    rep(c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),2)
    ,levels = c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  )
  
  p <- ggplot(data = df_Claims_Weather, aes(x = Date, y = Incurred.Cost, group = Large.Claim))+
    geom_bar(position = "dodge",stat = "identity", aes(fill = Large.Claim), colour = "#404040")+
    scale_fill_manual(values = c("#ffff99", "#ee5555"))+
    scale_y_continuous(labels = comma)+
    # the following labels would be generated automatically using lapply() if we were making this page up-to-date
    geom_text(aes(x = "Mar", y = 10000000),label = "Snow and Low Temperatures", angle = 0, nudge_x = 0.5)+
    geom_text(aes(x = "Jun", y = 4000000), label = "Storm Hector", angle = 80, nudge_x = 0.3)+
    geom_text(aes(x = "Sep", y = 6000000), label = "Storms Ali and Bronagh", angle = 80, nudge_x = 0.3)+
    geom_text(aes(x = "Dec", y = 4000000), label = "Storm Dierdre", angle = 80, nudge_x = 0.3)+
    ggtitle("Weather Claims")+
    theme(plot.title = element_text(hjust = 0.5)
          ,legend.position = "top"
          ,panel.background = element_blank()
          ,axis.line = element_line(colour = "#404041")
    )
  print(p)
}

# function testing
# fn.plot.Weather.Claims()

#### Weather Events Feed ####

fn.data.Weather_Newsfeed <- function(){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # This is a dummy function, which just returns a loaded data frame. This is a placeholder technique in
  # case we decided to create something more substantial here.
  # returns - weather newsfeed data
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  return(df_Weather_Newsfeed)}