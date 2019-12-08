#### Profit Summary Module ####
# This module supports the profit summary tab with:
#   - creating all the required data for:
#     - Valuebox total profit
#     - tolerance breaches gauge
#     - profit summary data table
#     - profit summary chart
#     - tolerance breaches socialbox comments
#  - creating the plotting functions for
#     - data table
#     - chart

#### Libraries ####
# This module uses the following libraries, which are loaded in the global.R file:
#library(ggplot2)  # for charts
#library(scales)   # for scales on charts
# 
# #calculate sums and cumulative sums
# df_Profit$sum <- apply(
#   df_Profit[c("GWP", "Day1.WCR", "Average.Cost", "Claims.Frequency")]
#   ,1
#   ,FUN = sum
# )
# df_Profit$csum <- ave(df_Profit$sum,df_Profit$Segment, df_Profit$Version.Name, df_Profit$Forecast.Date ,FUN = cumsum)
# 
# df_Profit$csum_GWP  <- ave(df_Profit$GWP             ,df_Profit$Segment, df_Profit$Version.Name, df_Profit$Forecast.Date ,FUN = cumsum)
# df_Profit$csum_WCR  <- ave(df_Profit$Day1.WCR        ,df_Profit$Segment, df_Profit$Version.Name, df_Profit$Forecast.Date ,FUN = cumsum)
# df_Profit$csum_Freq <- ave(df_Profit$Claims.Frequency,df_Profit$Segment, df_Profit$Version.Name, df_Profit$Forecast.Date ,FUN = cumsum)
# df_Profit$csum_AC   <- ave(df_Profit$Average.Cost    ,df_Profit$Segment, df_Profit$Version.Name, df_Profit$Forecast.Date ,FUN = cumsum)

#### ValueBox Total Profit ####
fn.data.Profit.ValueBox <- function(
  df_P = df_Profit,
  segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version,  #input$Plan_Version
  forecast.window #input$slider_forecast_horizon
  ){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
  # function description
  # inputs - df_Profit = input table loaded from .csv in global.R
  #        - segment, forecast.date, plan.version = dynamic inputs for filtering data, taken from input widgets
  # returns - single number representing the cumulative sum of profit impacts for full forcast window (used in valuebox)
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
  # filter df_Profit
   df_P <- df_P[
     (df_P$Forecast.Date == forecast.date) &
     (df_P$Plan.Version  == plan.version) 
   ,]
  # sum profit impacts
  df_P$sum <- apply(
    df_P[c("GWP", "Day1.WCR", "Average.Cost", "Claims.Frequency")]
    ,1
    ,FUN = sum
  )
  # cumulative sums
  df_P$csum <- ave(df_P$sum,df_P$Segment, df_P$Version.Name, df_P$Forecast.Date ,FUN = cumsum)
  
  # return last of cumulative sums for Total segment
  num_out <- as.numeric(df_P[(df_P$Segment == segment) & df_P$Period == df_P$Period[forecast.window], "csum"])
  # round num_out to tenth of millions
  num_out <- round(num_out, -5)/1000000
  return(num_out)
}

#TESTING THE FUNCTION:
#fn.data.Profit.ValueBox(segment = "Total", forecast.date = "Nov", plan.version = "Plan",forecast.window = 12)

#### Data Table Profit Impacts by Segment #####
fn.data.Profit.Table <- function(
  df_P  = df_Profit,
  df_B  = df_Baseline,
  segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version,  #input$Plan_Version
  forecast.window #input$slider_forecast_horizon
){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # function description
  # inputs - df_P, df_B  = input tables loaded from .csv in global.R
  #        - segment, forecast.date, plan.version, forecast.window = dynamic inputs for filtering data, taken from input widgets
  # returns - df_out is a data frame of segments, written contribution percent, annual GWP and total profit impact
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # filter df_Profit for forecast date and segment
  df_P <- df_P[
    (df_P$Forecast.Date == forecast.date) &
    (df_P$Plan.Version  == plan.version) 
    ,]
  # sum profit impacts
  df_P$sum <- apply(
    df_P[c("GWP", "Day1.WCR", "Average.Cost", "Claims.Frequency")]
    ,1
    ,FUN = sum
  )
  # cumulative sums
  # make sure df_P is in order before cumulative sums
  df_P <- df_P[order(df_P$Segment, df_P$Period),]
  df_P$csum <- ave(df_P$sum,df_P$Segment, df_P$Version.Name, df_P$Forecast.Date ,FUN = cumsum)
  # return last of cumulative sums for each segment
  df_out <- df_P[df_P$Period == df_P$Period[forecast.window],c("Segment", "csum")]
  # add on written contribution percent and annual GWP
  df_out <- merge(df_out, df_B[,c("Segment", "Written.Contribution.PC", "Annual.GWP")], by = "Segment")
  # order by descending GWP
  df_out <- df_out[order(df_out$Annual.GWP, decreasing = TRUE),c("Segment", "Written.Contribution.PC", "Annual.GWP","csum")]
  colnames(df_out)[colnames(df_out)=="csum"] <- "Profit Impact"
  #df_out['Annual.GWP'] <- apply(df_out[,'Annual.GWP',drop=F],1,function(x){paste("&pound;",as.character(x/1000000),"m",sep="")})
  df_out['Annual.GWP'] <- apply(df_out[,'Annual.GWP',drop=F],1,function(x){x/1000000})
  df_out['Profit Impact'] <- apply(df_out[,'Profit Impact',drop=F],1,function(x){round(x,-5)/1000000})
  return(df_out)
}

#testing the function 
#fn.data.Profit.Table(segment = "Total", forecast.date = "Nov", plan.version = "Plan", forecast.window = 12)

fn.data.Profit.Chart <- function(
  df_P   = df_Profit,
  df_Tol = df_Tolerance,
  segment,        #input$Segment
  forecast.date,  #input$As_At_Date
  plan.version,   #input$Plan_Version
  forecast.window #input$slider_forecast_horizon
){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # function description
  # inputs - df_Profit, df_Tolerance = input tables loaded from .csv in global.R
  #        - segment, forecast.date, plan.version, forecast.window = dynamic inputs for filtering data, taken from input widgets
  # returns - df_out is a data frame of segments, written contribution percent, annual GWP and total profit impact
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # filter df_Profit for forecast date and segment
  df_P <- df_P[
    (df_P$Segment       == segment       ) &
    (df_P$Forecast.Date == forecast.date ) &
    (df_P$Plan.Version  == plan.version  ) 
    ,]
  # sum profit impacts
  df_P$sum <- apply(
    df_P[c("GWP", "Day1.WCR", "Average.Cost", "Claims.Frequency")]
    ,1
    ,FUN = sum
  )
  # cumulative sums
  # make sure df_P is in order before cumulative sums
  df_P <- df_P[order(df_P$Segment, df_P$Period),]
  df_P$csum <- ave(df_P$sum,df_P$Segment, df_P$Version.Name, df_P$Forecast.Date ,FUN = cumsum)
  df_out <- df_P[(df_P$Segment == segment),c("Period", "csum")][1:forecast.window,]
  df_out["ToleranceUp"]   <- rep(df_Tol$Profit_Materiality[df_Tol$Segment == segment], NROW(df_out))
  df_out["ToleranceDown"] <- rep(-df_Tol$Profit_Materiality[df_Tol$Segment == segment], NROW(df_out))
  df_out["Pluses"]  <- apply(df_out[,"csum",drop=F],1,function(x){max(0,x)})
  df_out["Minuses"] <- apply(df_out[,"csum",drop=F],1,function(x){min(0,x)})
  df_out["colours"] <- apply(
    df_out[,"csum",drop=F]
    ,1
    ,function(x){if(x>0){return(pallette$greenpale)}else{pallette$redpale}
    }
  )
  #print(df_out)
  df_out$Period <- as.character(df_out$Period)
  return(df_out)
}

# function testing 
# test_chart_data <- fn.data.Profit.Chart(
#   segment = "Total"
#   ,forecast.date = "Nov"
#   ,plan.version = "Plan"
#   ,forecast.window = 12
# )

fn.convert.period.to.month <- function(period){
  conv <- list("01" = "Jan"
      ,"02" = "Feb"
      ,"03" = "Mar"
      ,"04" = "Apr"
      ,"05" = "May"
      ,"06" = "Jun"
      ,"07" = "Jul"
      ,"08" = "Aug"
      ,"09" = "Sep"
      ,"10" = "Oct"
      ,"11" = "Nov"
      ,"12" = "Dec")
  return(conv[[substr(period,5,6)]])
}

# function testing
# fn.convert.period.to.month("201904")

fn.plot.Profit.Chart <- function(df_Chart){
  # a few calculations to determine the y axis limits, tickmarks and tickmark labels
  limit <- ceiling(max(abs(df_Chart$csum),df_Chart$ToleranceUp[1])/1000000)*1000000 #10^(ceiling(log10(max(abs(df_Chart$csum)))))
  #print(df_Chart$ToleranceUp[1])
  #print(limit)# generate a set of month labels for x axis
  xlabels <- as.character(apply(df_Chart[,"Period",drop=F], 1, fn.convert.period.to.month))
  # generate the plot
  p <- ggplot(data = df_Chart, aes(x = Period))+
    geom_bar(aes(y = Pluses , fill = Period), stat = "identity", color = "#404041")+
    geom_bar(aes(y = Minuses, fill = Period), stat = "identity", color = "#404041")+
    geom_line(aes(y = ToleranceUp  , group = 1), color = pallette$red, size = 1, linetype = 5)+
    geom_text(aes(x = 1.5, y = df_Chart[1,"ToleranceUp"]),label = "Tolerance",nudge_y = limit/20)+
    geom_line(aes(y = ToleranceDown, group = 1), color = pallette$red, size = 1, linetype = 5)+
    scale_fill_manual(values = df_Chart$colours)+
    scale_y_continuous(
      name = "Cumulative Profit Impact"
      ,limits = c(-limit, limit)
      ,breaks = c(-limit, -limit/2, 0, limit/2, limit)
      ,labels = lapply(c(-limit, -limit/2, 0, limit/2, limit),function(x){dollar(x, prefix = "?")})
    ) +
    scale_x_discrete(labels = xlabels, name = "2019", expand = c(0,0))+
    ggtitle("Cumulative Profit Impact")+
    theme(plot.title = element_text(hjust = 0.5)
          ,legend.position = "none"
          #,axis.line = element_line(colour = "#000000", size = 1)
          ,panel.background = element_blank()
          ,axis.line = element_line(colour = "#404041")
          )
  # output the plot
  suppressWarnings(print(p))
}

#function testing
#fn.plot.Profit.Chart(fn.data.Profit.Chart(segment = "Barclays", forecast.date = "Nov", plan.version = "Plan V2", forecast.window = 12))

#### Tolerance Breaches ####

fn.data.Profit.Tolerance_Breaches <- function(
  df_P   = df_Profit,
  df_B   = df_Baseline,
  df_Tol = df_Tolerance,
  #segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version,  #input$Plan_Version
  forecast.window, #input$slider_forecast_horizon
  sort.order = "Materiality"  #input$tolerance_breach_sort_order , options are "Materiality", "Breach_Period"
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# function description
# inputs - df_Profit, df_Tolerance = input tables loaded from .csv in global.R
#        - segment, forecast.date, plan.version, forecast.window = dynamic inputs for filtering data, taken from input widgets
# returns - df_out is a data frame of segment, period, end-of-forcast-window-cumulative-profit-impact 
#           for segments with tolerance breaches in the forecast window
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # filter df_Profit for forecast date and segment
  df_out <- df_P[
     # (df_P$Segment       == segment       ) &
      (df_P$Forecast.Date == forecast.date ) &
      (df_P$Plan.Version  == plan.version  ) 
    ,]
  # sum profit impacts
  df_out$sum <- apply(
    df_out[c("GWP", "Day1.WCR", "Average.Cost", "Claims.Frequency")]
    ,1
    ,FUN = sum
  )
  # cumulative sums
  # make sure df_P is in order before cumulative sums
  df_out <- df_out[order(df_out$Segment, df_out$Period),]
  df_out$csum <- ave(df_out$sum, df_out$Segment, df_out$Version.Name, df_out$Forecast.Date, FUN = cumsum)
  
  df_out$csum_GWP <- ave(df_out$GWP             , df_out$Segment, df_out$Version.Name, df_out$Forecast.Date, FUN = cumsum)
  df_out$csum_WCR <- ave(df_out$Day1.WCR        , df_out$Segment, df_out$Version.Name, df_out$Forecast.Date, FUN = cumsum)
  df_out$csum_FRQ <- ave(df_out$Claims.Frequency, df_out$Segment, df_out$Version.Name, df_out$Forecast.Date, FUN = cumsum)
  df_out$csum_AC  <- ave(df_out$Average.Cost    , df_out$Segment, df_out$Version.Name, df_out$Forecast.Date, FUN = cumsum)
  
  df_out <- merge(df_out, df_Tol[,c("Segment", "Profit_Materiality")], on = "Segment")
  #rename and sort
  colnames(df_out)[colnames(df_out)=="Profit_Materiality"] <- "Tolerance"
  df_out <- df_out[order(df_out$Segment, df_out$Period),]
  #print(df_out) #TEST = PASS
  df_out["Breach"] <- apply(df_out[,c("csum", "Tolerance"),drop=F], 1, function(pair){abs(pair["Tolerance"])-abs(pair["csum"]) < 0})
  #print(df_out) #TEST = PASS
  # isolate first period of tolerance breach
  df_Period <- df_out[(df_out$Breach == TRUE),c("Period", "Segment")]
  df_Period["Breach_Period"] <- df_Period[apply(df_Period[,"Segment",drop = F], 1,function(x){match(x,df_Period$Segment)}),"Period"]
  df_Period <- df_Period[!duplicated(df_Period[,c("Segment", "Breach_Period")]),c("Segment","Breach_Period")]
  # print(df_Period) #TEST = PASS
  # isolate total ? amount of breach at end of forecast window
  df_Total <- df_out[df_out$Period == max(df_out$Period),c("Segment", "csum")]
  # print(df_Total) #TEST = PASS
  # find biggest contributer amongst 4 metrics to the breach
  df_Metric <- df_out[df_out$Period == max(df_out$Period),c("Segment", "csum", "csum_GWP", "csum_WCR", "csum_FRQ", "csum_AC")]
  # print(df_Metric)
  df_Metric["Most_Material_MetricUp"] <- names(
    df_Metric[,c("csum_GWP", "csum_WCR", "csum_FRQ", "csum_AC")])[
      apply(
        df_Metric[,c("csum_GWP", "csum_WCR", "csum_FRQ", "csum_AC"),drop=F]
        ,1
        ,which.max
      )
    ]
  df_Metric["Most_Material_MetricDown"] <- names(
    df_Metric[,c("csum_GWP", "csum_WCR", "csum_FRQ", "csum_AC")])[
      apply(
        df_Metric[,c("csum_GWP", "csum_WCR", "csum_FRQ", "csum_AC"),drop=F]
        ,1
        ,which.min
      )
    ]
  
  df_Metric["Most_Material_Metric"] <- apply(
    df_Metric
    ,1
    ,function(x){
      if(as.numeric(x["csum"]) > 0){
        return(x["Most_Material_MetricUp"])
      }else{
        return(x["Most_Material_MetricDown"])
      }
    }
  )
  df_Metric <- df_Metric[,c("Segment", "Most_Material_Metric")]
  
  df_output <- fn.multimerge(list(df_Period, df_Total, df_Metric), merge_by = "Segment")
  colnames(df_output)[colnames(df_output)=="csum"] <- "Materiality"
  list_mmm_convertor <- list("csum_GWP" = "GWP", "csum_WCR" = "Day1.WCR", "csum_FRQ" = "Claims.Frequency", "csum_AC" = "Average.Cost")
  df_output$Most_Material_Metric <- apply(df_output[,"Most_Material_Metric",drop=F],1,function(x){as.character(list_mmm_convertor[x])})
  
  if (sort.order == "Materiality"){
    return(df_output[order(abs(df_output[,"Materiality"]),decreasing = TRUE),])  
  }else{
    return(df_output[order(abs(df_output[,"Breach_Period"])),])  
  }
}

#fn.data.Profit.Tolerance_Breaches(#segment = "Total", 
#  forecast.date = "Nov", plan.version = "Plan", forecast.window = 12)
