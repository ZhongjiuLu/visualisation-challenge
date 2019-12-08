#### Module Weather ####
# This module supports the metrics (trading and claims) tabs with:
# data for profit impact value box (top-left)
# There is only one function in this module

#### Libraries ####
# This module uses no libraries

fn.data.ValueBox.Profit_Impact <- function(
  df_P = df_Profit,
  Metric, #static input from c("GWP", "Day1.WCR","Average.Cost", "Claims.Frequency")
  segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version #input$Plan_Version
){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
  # This function returns the profit impact for a given metric and segment, it goes in the top-left
  # value box on each metric tab.
  # returns - num_out, a profit impact to go in a value box
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
  # filter df_Profit
  df_P <- df_P[
    (df_P$Segment == segment) &
    (df_P$Forecast.Date == forecast.date) &
    (df_P$Plan.Version  == plan.version) 
    ,]
  # sum profit impacts
  num_out <- sum(df_P[,Metric])
  # round num_out to tenth of millions
  num_out <- round(num_out, -4)/1000000
  return(num_out)
}

#function testing
#fn.data.ValueBox.Profit_Impact(df_Profit, "Claims.Frequency", "Aggregators", "Nov", "Plan")