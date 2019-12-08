#### Documentation #############################################################################################################
#
####  /- Code Structure ################################################################################################################
#  
#  App
#  # R files
#  - /global.R                 = standard shiny app file, loads before app run
#  - /server.R                 = standard shiny app file, contains reactive functionality
#  - /ui.R                     = standard shiny app file, contains code for ui (how the app appears on screen)
#  - /module_01_Summary.R      = supports the "Summary View" main tab
#  - /module_02_GWP.R          = supports the GWP tab 
#  - /module_03_WCR.R          = supports the WCR tab
#  - /module_04_claims_RnJ.R   = supports the claims tabs (valueboxes and left-hand chart)
#  - /module_05_claims_Devon.R = supports the claims tabs (right-hand chart)
#  - /module_06_Weather.R      = supports the weather tab
#  - /module_07_Seb_Extras.R   = provides a function for calculating the profit impact valueboxes for each tab
#  # excel file
#  - /Development Notes.xlsx   = Spreadsheet detailing and logging developments
#  # www subfolder
#  - www/Data/                 = Folder containing financial data
#  - www/Weather Data/         = Folder containing weather data for weather tab
#  - www/Images/               = Folder containing all image files
#  - www/custom.css            = custom style sheet (written using colours-for-shiny app, 
#                                with a couple of amendments from Sebastian for this app)
#

#### Libraries ####
# Shiny and related
library(shiny) # for making basic shiny apps
library(shinydashboard) # for making shiny dashboard
library(shinydashboardPlus) # more widgets for shinydashboard (socialfeed on summary and weather pages)
library(shinyWidgets) # for pickerInput widget (a selectInput which can add images to the selections, used for input$Segment)
library(flexdashboard) # for gauge widget on summary page
library(DT) # display data tables (one on summary page)

# Data handling
library(readxl) # for loading data
library('data.table') # for data manipulation (module_claims_RnJ)
library(reshape2) # for transforming data into the right shape for charts

# Charting
library(ggplot2) # basic charting package
library(scales)  # package for chart enhancement - formatting axes


#### Load Modules ####
source("./module_01_Summary.R")
source("./module_02_GWP.R")
source("./module_03_WCR.R")
source("./module_04_claims_RnJ.R")
source("./module_05_claims_Devon.R")
source("./module_06_Weather.R")
source("./module_07_Seb_Extras.R")

#### Load Data ####

# flat data files
df_Trading  = read.csv("./www/Data/Trading.csv"       , stringsAsFactors = FALSE)
df_Claims   = read.csv("./www/Data/Claims.csv"        , stringsAsFactors = FALSE)
df_Forecast = read.csv("./www/Data/Forecast.csv"      , stringsAsFactors = FALSE)
df_Profit   = read.csv("./www/Data/Profit_Impacts.csv", stringsAsFactors = FALSE)
df_Baseline = read.csv("./www/Data/Baseline_Info.csv" , stringsAsFactors = FALSE)

df_Forecast['Forecast.Date'] = gsub("Forecast", "",df_Forecast$Version.Name)

# df_tolerance needs to be dynamic, so a function has been created
fn.Tolerance <- function(df_B = df_Baseline, MaterialityPC = 0.1){
  df_Tol <- data.frame(
    'Profit_Materiality'             = df_B$Written.Contribution.GBP * MaterialityPC
  )
  df_Tol['Segment']                  <- df_Baseline$Segment
  df_Tol['Annual_GWP_Tolerance']     <- round(df_Tol$Profit_Materiality/((df_B$Written.Contribution.PC^2)^0.5),-5)
  df_Tol['Day1WCR_Tolerance']        <- df_Tol$Profit_Materiality*0.92/df_B$Annual.GWP
  df_Tol['Claims_Freq_Tolerance']    <- df_Tol$Profit_Materiality/df_B$Average.Cost/df_B$Earned.Exposures
  df_Tol['Average_Claims_Tolerance'] <- round(df_Tol$Profit_Materiality/df_B$No.Claims.Normalised,-1)

  return(df_Tol)
}

df_Tolerance <- fn.Tolerance(df_Baseline)

#### Load Images for Segment Branding ####
# list of segments
df_Segment_Branding <- data.frame("Segment" = unique(df_Baseline$Segment))
# list of image names
df_Segment_Branding$src <- c(
  'Images/QuoteMeHappy.png'
  ,'Images/Barclays.png'
  ,'Images/HSBC.png'
  ,'Images/Aviva.png'
  ,'Images/Aviva.png'
  ,'Images/Santander.png'
  ,'Images/TSB.png'
  ,'Images/Aviva.png'
  ,'Images/Aviva.png'
)

# HTML img tags for images
df_Segment_Branding$img <- c(
  sprintf("<img src='Images/QuoteMeHappy.png' width=25px></img><span>&nbsp&nbspAggregators</span>","Aggregators")
  ,sprintf("<img src='Images/Barclays.png' width=25px></img><span>&nbsp&nbspBarclays</span>","Barclays")
  ,sprintf("<img src='Images/HSBC.png' width=25px></img><span>&nbsp&nbspHSBC</span>","HSBC")
  ,sprintf("<img src='Images/Aviva.png' width=25px></img><span>&nbsp&nbspIB APC</span>","IB APC")
  ,sprintf("<img src='Images/Aviva.png' width=25px></img><span>&nbsp&nbspIB Main ex APC</span>","IB Main ex APC")
  ,sprintf("<img src='Images/Santander.png' width=25px></img><span>&nbsp&nbspSantander</span>","Santander")
  ,sprintf("<img src='Images/TSB.png' width=25px></img><span>&nbsp&nbspTSB</span>","TSB")
  ,sprintf("<img src='Images/Aviva.png' width=25px></img><span>&nbsp&nbspUKDI</span>","UKDI")
  ,sprintf("<img src='Images/Aviva.png' width=25px></img><span>&nbsp&nbspTotal</span>","Total")
)

#### Colour Palette ####
pallette <- list(
  black      = rgb(33,33,33,255, maxColorValue=255),
  greydk     = rgb(64,64,65,255, maxColorValue=255),
  greymid    = rgb(109,110,112,255, maxColorValue=255),
  greylt     = rgb(205,207,207,255, maxColorValue=255),
  greypale   = rgb(235,236,234,255, maxColorValue=255),
  white      = rgb(255,255,255,255, maxColorValue=255),
  yellow     = rgb(255,221,22,255, maxColorValue=255),
  yellowpale = rgb(255,221,22,95, maxColorValue=255),
  yellowdk   = rgb(128,110,11,255, maxColorValue=255),
  blue       = rgb(66,139,202,255, maxColorValue=255),
  red        = rgb(238,76,55,255, maxColorValue=255),
  redpale    = rgb(238,76,55,95, maxColorValue=255),
  green      = rgb(79,160,49,255, maxColorValue=255),
  greenpale  = rgb(79,160,49,95, maxColorValue=255)
)

#### Useful Functions ####

fn.multimerge <- function(list_of_dataframes, merge_by){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # This function merges multiple tables at the same time
  # for example if you had lots of tables to join on Period, you could use this instead of 
  # merging them one at a time
  # inputs  - list_of_dataframes = list of dataframes to merge
  #         - merge_by =  name of column to merge by
  # returns - df_out = a dataframe which consists of all the list_of_dataframes merged by merge_by
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  if (length(list_of_dataframes) <= 1 ){
    df_out <- list_of_dataframes[1]
  } else {
    df_temp <- merge(list_of_dataframes[1]
                     ,list_of_dataframes[2]
                     ,by = merge_by)
    if (length(list_of_dataframes) >=3){
      for (df in list_of_dataframes[3:length(list_of_dataframes)]){
        df_temp <- merge(df_temp, df, by = merge_by)
      }
    }
    df_out <- df_temp
  }
  return (df_out)
}

