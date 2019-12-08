#### Claims Module - Richard and Jack ####
# This module supports the Claims tabs with:
#  - creating all the required data for:
#     - valueboxes
#     - chart 1
#  - creating the plotting functions for
#     - chart 1

#### Libraries ####
# This module uses the following libraries, which are loaded in the global.R file:
# library('data.table') # for data manipulation
# library(ggplot2)      # for plotting charts
# library(plotly)       # for plotting pretty charts

#### Data Handling ####
#### /-Forecast ####
fn.data.claims.RnJ.Forecast <- function(
  df_F = df_Forecast
  ,Class
  ,As_At_Date
){
  Forecast<-as.data.table(df_F)
  Forecast$Period <- paste0(Forecast$Period, "", "01")
  Forecast$Period <- as.Date.character(Forecast$Period, "%Y%m%d")  
  Forecast_C <- Forecast[Forecast$Segment == Class]
  Forecast_P <- Forecast_C[substr(Forecast_C$Version.Name,9,11)==substr(As_At_Date,1,3)]
  return(Forecast_P)
}
  
#function testing
#fn.data.claims.RnJ.Forecast()  
 
#### /-Claims ####
fn.data.claims.RnJ.Claims.Actuals <- function(
  df_C = df_Claims
  ,Class #input$Segment
){
  ClaimsData<-as.data.table(df_C)
  #Convert numbers to numeric from character #for the date, need to get this to work so that when using str it shows as date. 
  ClaimsData$Period      <- paste0(ClaimsData$Period, "", "01")
  ClaimsData$Period      <- as.Date.character(ClaimsData$Period, "%Y%m%d")  
  ClaimsData$Frequency   <- ClaimsData$Claims.Nos / ClaimsData$Earned.Exposure
  ClaimsData$AverageCost <- ClaimsData$Incurred.Cost / ClaimsData$Claims.Nos
  ClaimsData_C  <- ClaimsData  [ClaimsData$Segment          == Class]
  ClaimsData_P  <- ClaimsData_C[ClaimsData_C$Peril          == "Non Weather"]
  ClaimsData_LC <- ClaimsData_P[ClaimsData_P$Large.Claim    == "Attritional"]
  Actuals       <- ClaimsData_LC[ClaimsData_LC$Version.Name == "Actual"]
  return(Actuals)
} 
  
#function testing
#fn.data.claims.RnJ.Claims.Actuals()

fn.data.claims.RnJ.Claims.Plan <- function(
  df_C = df_Claims
  ,Class #input$Segment
  ,Plan_Version #input$Plan.Version
){
  ClaimsData<-as.data.table(df_C)
  #Convert numbers to numeric from character #for the date, need to get this to work so that when using str it shows as date. 
  ClaimsData$Period      <- paste0(ClaimsData$Period, "", "01")
  ClaimsData$Period      <- as.Date.character(ClaimsData$Period, "%Y%m%d")
  ClaimsData$Frequency   <- ClaimsData$Claims.Nos / ClaimsData$Earned.Exposure
  ClaimsData$AverageCost <- ClaimsData$Incurred.Cost / ClaimsData$Claims.Nos
  ClaimsData_C  <- ClaimsData  [ClaimsData$Segment          == Class]
  ClaimsData_P  <- ClaimsData_C[ClaimsData_C$Peril          == "Non Weather"]
  ClaimsData_LC <- ClaimsData_P[ClaimsData_P$Large.Claim    == "Attritional"]
  Plan          <- ClaimsData_LC[ClaimsData_LC$Version.Name == Plan_Version]
  return(Plan)
}
#function testing
# fn.data.claims.RnJ.Claims.Plan()

#### Charts ####
fn.chart.claims.RnJ.AC <- function(
  df_act, df_plan, df_fore, Class
  ){
  p <- ggplot() + 
     # forecast background
     geom_rect(
       aes(
         ymin = 0
         ,ymax = Inf
         ,xmin = min(df_fore$Period)
         ,xmax = max(df_plan$Period)
       ), fill = pallette$greypale
       , na.rm=TRUE
     )+
     geom_line(data = df_act,  aes(x = Period, y = AverageCost, colour = "Actual") ,size = 1) + 
     geom_line(data = df_plan, aes(x = Period, y = AverageCost, colour = "Plan") ,size = 1) +
     geom_line(data = df_fore, aes(x = Period, y = Average.Cost, colour = "Forecast"),size = 1) +
     scale_colour_manual(name = ""
                         ,values = c(
                           "Actual" = pallette$greydk
                           ,"Plan"  = pallette$blue
                           ,"Forecast" = pallette$red)
                         )+
     theme_bw() + 
     theme(
       legend.position = c(0.9,0.2)
     ) + 
     xlab("Period") +
     ylab("Average Cost /£")+
     ylim(0,NA)
  print(p)
}

#function testing
# fn.chart.claims.RnJ.AC(fn.data.claims.RnJ.Claims.Actuals(df_Claims,"HSBC")
#                         ,fn.data.claims.RnJ.Claims.Plan(df_Claims,"HSBC","Plan")
#                         ,fn.data.claims.RnJ.Forecast(df_Forecast,Class = "HSBC",As_At_Date = "Nov")
#                         ,"HSBC")

fn.chart.claims.RnJ.Freq <- function(
  df_act, df_plan, df_fore, Class
){ 
 p<- ggplot() +
   # forecast background
   geom_rect(
     aes(
       ymin = 0
       ,ymax = Inf
       ,xmin = min(df_fore$Period)
       ,xmax = max(df_plan$Period)
     ), fill = pallette$greypale
     , na.rm=TRUE
   )+
   geom_line(data = df_act, aes(x = Period, y = Frequency*100,colour="Actual"),size = 1) + 
   geom_line(data = df_plan, aes(x = Period, y = Frequency*100, colour = "Plan"),size = 1) +
   geom_line(data = df_fore, aes(x = Period, y = Claims.Frequency*100, colour = "Forecast"),size = 1) +
   scale_colour_manual(
     name = ""
     ,values = c(
     "Actual" = pallette$greydk
     ,"Plan"  = pallette$blue
     ,"Forecast" = pallette$red)
   )+          
   theme_bw() + 
   theme(
     legend.position = c(0.9,0.2)
   )+ 
   xlab("Loss Date") +
   ylab("Frequency (%)")+
   ylim(0,NA)
 print(p)
}  

#function testing
#fn.chart.claims.RnJ.Freq(fn.data.claims.RnJ.Claims.Actuals(df_Claims,"HSBC")
#                         ,fn.data.claims.RnJ.Claims.Plan(df_Claims,"HSBC","Plan")
#                         ,fn.data.claims.RnJ.Forecast(df_Forecast,Class = "HSBC",As_At_Date = "Nov")
#                         ,"HSBC")
  
fn.claims.RnJ.LTAAverageCost <- function(Actuals){return(mean(Actuals$AverageCost))}
fn.claims.RnJ.LTAFrequency   <- function(Actuals){return(mean(Actuals$Frequency))}