#### WCR Module ####
# This module supports the WCR tab with:
#  - creating all the required data for:
#     - valueboxes
#     - chart 1
#     - chart 2 
#  - creating the plotting functions for
#     - chart 1
#     - chart 2 

#### Libraries ####
# This module uses the following libraries, which are loaded in the global.R file:
# library(ggplot2) # for charts

#### ValueBoxes ####
fn.data.WCR.valueBox1 <- function(
  # data source
  df_Tr = df_Trading,
  # data filter
  segment #input$Segment
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@    
# This function creates the string for WCR valuebox 1 (long term average WCR for this segment)
# inputs  - df_Trading = input table loaded from .csv in global.R
#         - segment = dynamic input for filtering data, taken from input widgets
# returns - number = Average Day 1 WCR for this segment, over all historic periods
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  df_calc <- df_Tr[
    (df_Tr$Segment == segment) &
    (df_Tr$Version.Name == "Actual")
    ,
  ]
  str_out <- paste(
    as.character(
      round(
        sum(df_calc$WCR.Numerator)/sum(df_calc$WCR.Denominator)
        ,3
      )*100
    )
    ,"%"
    ,sep = ""
  )
  return(str_out)
}

# function testing
# fn.data.WCR.valueBox1(segment = "Aggregators")

fn.data.WCR.valueBox2 <- function(
  # data source
  df_P = df_Profit,
  # data filters
  segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version #input$Plan_Version
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@    
# This function creates the value for WCR valuebox 2
# inputs  - df_Trading = input table loaded from .csv in global.R
#         - segment, forecast.date, plan.version = dynamic inputs for filtering data, taken from input widgets
# returns - num_out = total profit impact for this segment
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  df_P <- df_P[
    (df_P$Forecast.Date == forecast.date) &
    (df_P$Plan.Version  == plan.version) 
    ,]

  # cumulative sums
  df_P$csum_WCR <- ave(df_P$Day1.WCR,df_P$Segment, df_P$Version.Name, df_P$Forecast.Date ,FUN = cumsum)
  
  # return last of cumulative sums for Total segment
  num_out <- as.numeric(df_P[(df_P$Segment == segment) & df_P$Period == max(df_P$Period), "csum_WCR"])
  
  # round num_out to tenth of millions
  num_out <- round(num_out, -4)/1000000
  return(num_out)
}

# function testing
# fn.data.WCR.valueBox2(segment = "Aggregators", forecast.date = "Nov", plan.version = "Plan")

#### Actuals Chart ####
fn.data.WCR.chart1 <- function(
  # data sources
  df_Tr = df_Trading,
  df_F  = df_Forecast,
  # data filters
  segment, #input$Segment
  forecast.date, #input$As_At_Date
  plan.version, #input$Plan_Version
  # chart controls
  historic.years = 1 #input$WCR_historic_years
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@    
#  This function creates the data required for chart 1
# inputs - df_Trading, df_Forecast = input tables loaded from .csv in global.R
#        - segment, forecast.date, plan.version, historic.years = dynamic inputs for filtering data, taken from input widgets
# returns - df_chart = a data frame for the WCR chart 1
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@

  # filter trading data by segment and plan version
  df_RN_plus_NB <- df_Tr[
    (df_Tr$Segment == segment) &
    ((df_Tr$Version.Name == plan.version) | (df_Tr$Version.Name == "Actual"))
  ,]
  
  # calculate Total (renewal + new business) 
  df_RN <- df_RN_plus_NB[(df_RN_plus_NB$Policy.Duration == "Renewal"),]
  df_NB <- df_RN_plus_NB[(df_RN_plus_NB$Policy.Duration == "New Business"),]   
  
  df_Total <- df_RN_plus_NB[(df_RN_plus_NB$Policy.Duration == "Renewal"),]
  df_Total$Policy.Duration <- rep("Total", NROW(df_Total))
  df_Total$GWP             <- df_RN$GWP             + df_NB$GWP
  df_Total$WCR.Numerator   <- df_RN$WCR.Numerator   + df_NB$WCR.Numerator 
  df_Total$WCR.Denominator <- df_RN$WCR.Denominator + df_NB$WCR.Denominator
  
  # append df_Total onto bottom
  df_chart <- rbind(df_RN_plus_NB,df_Total)
  
  #calculate WCR %
  df_chart$WCR = apply(
    df_chart[,c("WCR.Numerator","WCR.Denominator"),drop=F]
    ,1
    ,function(x){
      x["WCR.Numerator"]/x["WCR.Denominator"]
    }
  )
  
  # fixing column datatpyes for chart 
  df_chart$Period <- as.character(df_chart$Period)
  
  # join on forecasts
  df_chart <- merge(df_chart, df_F[(df_F$Forecast.Date == forecast.date),], by = c("Period", "Segment"),all.x = TRUE)
  df_chart <- df_chart[order(df_chart$Version.Name.x, df_chart$Segment, df_chart$Policy.Duration, df_chart$Period),]
  
  # filter rows down to just data to chart
  df_chart = df_chart[
    !((df_chart$Period < min(df_F[(df_F$Forecast.Date == forecast.date),"Period"]))&(df_chart$Version.Name.x == plan.version)) &
    !((!is.na(df_chart$Forecast.Date)) & (df_chart$Version.Name.x == "Actual")) &
    !((df_chart$Version.Name.x == plan.version) & (df_chart$Policy.Duration != "Total"))
    ,]
  
  # reduce historic years
  df_chart = df_chart[(df_chart$Period >= paste(as.character(2019-historic.years),"00")),]

  ## This would be a really nice end-point, as df_chart contains all the data for every metric
  ## but now I will force the data into the shape required for the chart:

  # reduce df_chart to only required columns:
  df_chart = df_chart[,c("Period", "Year.x", "Month.x","Segment", "Version.Name.x", "Policy.Duration", "GWP", "WCR", "Gross.Written.Premium", "Day1.WCR")]
  
  # split GWP and WCR columns by plan/actual
  df_chart$Plan_GWP = apply(df_chart[,c("Version.Name.x","GWP"),drop=F],1, function(x){if(x["Version.Name.x"] == plan.version){x["GWP"]}else{NA}})
  df_chart$Plan_WCR = apply(df_chart[,c("Version.Name.x","WCR"),drop=F],1, function(x){if(x["Version.Name.x"] == plan.version){x["WCR"]}else{NA}})

  df_chart$Act_GWP = apply(df_chart[,c("Version.Name.x","GWP"),drop=F],1, function(x){if(x["Version.Name.x"] == "Actual"){x["GWP"]}else{NA}})
  df_chart$Act_WCR = apply(df_chart[,c("Version.Name.x","WCR"),drop=F],1, function(x){if(x["Version.Name.x"] == "Actual"){x["WCR"]}else{NA}})

  # chop up by renewal/new business/total and stitch together side-by-side
  
  df_chart_nb  <- df_chart[(df_chart$Policy.Duration == "New Business"),c("Period", "Act_GWP", "Act_WCR")]
  df_chart_rn  <- df_chart[(df_chart$Policy.Duration == "Renewal"),c("Period", "Act_GWP", "Act_WCR")]
  df_chart_tot <- df_chart[(df_chart$Policy.Duration == "Total"),]
  
  df_temp <- merge(
    df_chart_nb, df_chart_rn,
    "Period"
  )
  
  names(df_temp) <- c("Period", "NB_GWP", "NB_WCR", "RN_GWP", "RN_WCR")
  
  df_chart <- merge(df_chart_tot, df_temp, "Period",all.x = TRUE)
  
  df_chart <- df_chart[,c("Period", "Year.x", "Month.x", "Segment", "Version.Name.x","Plan_GWP", "Plan_WCR"
                         ,"Gross.Written.Premium", "Day1.WCR", "Act_GWP", "Act_WCR"
                         ,"NB_GWP", "NB_WCR", "RN_GWP", "RN_WCR")
                      ]
  names(df_chart) <- c("Period", "Year", "Month.x", "Segment", "Version.Name", "Plan_GWP", "Plan_WCR"
                       ,"FC_GWP", "FC_WCR", "Act_GWP", "Act_WCR"
                       ,"NB_GWP", "NB_WCR", "RN_GWP", "RN_WCR")
  # resolve datatypes
  
  df_chart$Plan_GWP <- as.numeric(df_chart$Plan_GWP)
  df_chart$Plan_WCR <- as.numeric(df_chart$Plan_WCR)
  df_chart$FC_GWP   <- as.numeric(df_chart$FC_GWP)
  df_chart$FC_WCR   <- as.numeric(df_chart$FC_WCR)
  df_chart$Act_GWP  <- as.numeric(df_chart$Act_GWP)
  df_chart$Act_WCR  <- as.numeric(df_chart$Act_WCR)
  df_chart$NB_GWP   <- as.numeric(df_chart$NB_GWP)
  df_chart$NB_WCR   <- as.numeric(df_chart$NB_WCR)
  df_chart$RN_GWP   <- as.numeric(df_chart$RN_GWP)
  df_chart$RN_WCR   <- as.numeric(df_chart$RN_WCR)
  
  # add 3-letter month for minor x-axis 
  dic_month <- list(
    "01"  = "Jan"
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
    ,"12" = "Dec"
  )
  df_chart$Month <- lapply(df_chart$Month.x,function(x){as.character(dic_month[x])})
  
  return(df_chart)
}

# function testing
# df_test <- fn.data.WCR.chart1(segment = "Barclays", forecast.date = "Nov", plan.version = "Plan", historic.years = 1)

fn.plot.WCR.chart1 <- function(
  df_chart
  ,split_NB_RN = FALSE #input$WCR_Split_NB_RN
  ){
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@    
  # inputs - df_chart, output of fn.data.chart1 data tailored to this chart type
  #        - split_NB_RN, dynamic input taken from input widget (determines whether NB/RN split appears on chart)
  # returns - p, a ggplot
  #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  p <- ggplot(data = df_chart, aes(x = Period), na.rm=TRUE)+
    # forecast background
    geom_rect(
      aes(
        ymin = 0
        ,ymax = Inf
        ,xmin = min(df_chart[(df_chart$Version.Name != "Actual"),"Period"])
        ,xmax = max(df_chart$Period)
      ), fill = pallette$greypale
      , na.rm=TRUE
    )
    # WCR lines
  if (split_NB_RN){
    p <- p + 
         geom_line(aes(y = RN_WCR, group = 1, colour = "Renewal"     ), size = 1, na.rm=TRUE) +
         geom_line(aes(y = NB_WCR, group = 2, colour = "New Business"), size = 1, na.rm=TRUE)
  } else {
    p <- p + geom_line(aes(y = Act_WCR, group = 3, colour = "Total"  ), size = 1, na.rm=TRUE)
  }
  p <- p+
       geom_line(aes(y = FC_WCR  , group = 4, colour = "Forecast"), size = 1, linetype = 2, na.rm=TRUE)+
       geom_line(aes(y = Plan_WCR, group = 5, colour = "Plan")    , size = 1, linetype = 2, na.rm=TRUE)+
       # colours
       scale_color_manual(
         values = if (split_NB_RN){
           c("Renewal" = pallette$yellow, 
             "New Business" = pallette$green, 
             "Forecast" = pallette$red, 
             "Plan" = pallette$blue)
         }else{
           c("Total" = pallette$greydk, "Forecast" = pallette$red, "Plan" = pallette$blue)
         }
       )+
      # Add year annotations
      lapply( seq(1,length(unique(df_chart$Year)))
              ,function(year)
              {geom_text(
                aes(x = year*12-10,
                    y = 1 
                )
                ,label = unique(df_chart$Year)[year]
              )
              }
      )+
      # axes
      scale_y_continuous(
        name = "Day 1 Written Claims Ratio"
        ,limits = c(0,1)
        ,breaks = c(0,0.25,0.5,0.75,1)
        ,labels = c("0%", "25%", "50%", "75%", "100%")
      )+
      scale_x_discrete(
        name = "Date"
        ,labels = df_chart$Month
      )+
      # theme
      theme(panel.background = element_blank()
            ,axis.line = element_line(colour = "#404041")
            ,axis.text.x = element_text(angle = 30, hjust = 1)
            ,legend.position = c(0.8,0.25)
      )
    
  print(p)
}

# # function testing
# fn.plot.WCR.chart1(df_test, split_NB_RN = FALSE)
# fn.plot.WCR.chart1(df_test, split_NB_RN = TRUE)

#### Tolerance Chart ####
fn.data.WCR.chart2 <- function(
# data sources
df_T  = df_Tolerance,
df_Tr = df_Trading,
df_F  = df_Forecast,
# data filters
segment, #input$Segment
plan.version, #input$Plan_Version
forecast.date #substr(input$as_at_date,1,3)
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function generates the data for WCR chart 2
# inputs - df_Trading, df_Forecast = input tables loaded from .csv in global.R
#        - df_Tolerance = dynamically generatd table in sympathy with input slider
#        - segment, forecast.date, plan.version = dynamic inputs for filtering data, taken from input widgets
# returns - df_chart is a data frame for the WCR chart 2
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  
  # filter Trading data for plan version
  df_A  = df_Tr[
    (df_Tr$Segment      == segment) &
      (df_Tr$Version.Name == plan.version)
    ,]
  
  #summarise NB and RN
  df_RN <- df_A[(df_A$Policy.Duration == "Renewal"),]
  df_NB <- df_A[(df_A$Policy.Duration == "New Business"),]
  
  # add totals in 
  df_A     <- df_A[(df_A$Policy.Duration == "Renewal"),]
  df_A$Policy.Duration <- rep("Total", NROW(df_A))
  df_A$GWP             <- df_RN$GWP             + df_NB$GWP
  df_A$WCR.Numerator   <- df_RN$WCR.Numerator   + df_NB$WCR.Numerator
  df_A$WCR.Denominator <- df_RN$WCR.Denominator + df_NB$WCR.Denominator
  df_A$Plan_WCR        <- apply(df_A,1,function(x){as.numeric(x["WCR.Numerator"])/as.numeric(x["WCR.Denominator"])})
  
  # filter Forecast data
  df_B =  df_F[
    (df_F$Segment       == segment) &
      (df_F$Forecast.Date == forecast.date)
    ,]
  
  # join plan and forecast
  df_chart = merge(df_A, df_B, by = "Period")
  
  df_chart$diff_GWP <- apply(df_chart,1,function(x){as.numeric(x["Gross.Written.Premium"])-as.numeric(x["GWP"])})
  df_chart$diff_WCR <- apply(df_chart,1,function(x){as.numeric(x["Day1.WCR"])-as.numeric(x["Plan_WCR"])})
  
  # cumulative sums
  #GWP
  df_chart$csum_diff_GWP <- ave(df_chart$diff_GWP, FUN = cumsum)
  #WCR
  df_chart$csum_WCR.Numerator <- ave(df_chart$WCR.Numerator, FUN = cumsum)
  df_chart$csum_WCR.Denominator <- ave(df_chart$WCR.Denominator, FUN = cumsum)
  df_chart$csum_WCR <- apply(df_chart,1,function(x){as.numeric(x["csum_WCR.Numerator"]) / as.numeric(x["csum_WCR.Denominator"])})
  df_chart$csum_diff_WCR <- apply(df_chart,1,function(x){as.numeric(x["Day1.WCR"]) - as.numeric(x["csum_WCR"])})
  
  # find tolerance level
  df_chart$GWP_Tol <- rep(df_T[(df_T$Segment == segment),"Annual_GWP_Tolerance"],NROW(df_chart))
  df_chart$WCR_Tol <- rep(df_T[(df_T$Segment == segment),"Day1WCR_Tolerance"]   ,NROW(df_chart))
  df_chart$GWP_Tol_down <- -df_chart$GWP_Tol
  df_chart$WCR_Tol_down <- -df_chart$WCR_Tol
  
  # Ok that's all the data in place, now just to shape it for the charts
  # pluses and minuses as separate series so that bars can be coloured how we want:
  
  df_chart$csum_diff_GWP_pluses  <- apply(df_chart[,"csum_diff_GWP",drop=F],1,function(x){if(x>=0){x}else{0}})
  df_chart$csum_diff_GWP_minuses <- apply(df_chart[,"csum_diff_GWP",drop=F],1,function(x){if(x <0){x}else{0}})
  
  df_chart$csum_diff_WCR_pluses  <- apply(df_chart[,"csum_diff_WCR",drop=F],1,function(x){if(x>=0){x}else{0}})
  df_chart$csum_diff_WCR_minuses <- apply(df_chart[,"csum_diff_WCR",drop=F],1,function(x){if(x <0){x}else{0}})
  
  # amending Period so it isn't interpreted as number
  df_chart$Period <- as.character(df_chart$Period)
  # adding 3-letter month for x axis labels
  dic_month <- list(
    "01"  = "Jan"
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
    ,"12" = "Dec"
  )
  df_chart$Month <- lapply(df_chart$Month.x,function(x){as.character(dic_month[x])})
  
  return(df_chart)
}

# # function testing:
# df_test <- fn.data.WCR.chart2(
#   df_T  = df_Tolerance,
#   df_Tr = df_Trading,
#   df_F  = df_Forecast,
#   segment = "Total",
#   plan.version = "Plan",
#   forecast.date = "Nov"
# )

fn.chart.WCR.chart2 <- function(df_chart){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function creats the plot for chart 2
# inputs  - df_chart (generated by fn.data.WCR.chart2)
# returns - p, a ggplot for chart 2
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#

  # generate plot
  p<- ggplot(data = df_chart, aes(x = Period))+
    # up and down bars
    geom_bar(aes(y = csum_diff_WCR_pluses), stat = "identity", fill = pallette$redpale)+
    geom_bar(aes(y = csum_diff_WCR_minuses), stat = "identity", fill = pallette$greenpale)+
    # tolerance lines
    geom_line(aes(y = WCR_Tol)     , group = 3, color = pallette$red, size = 1, linetype = 5)+
    geom_line(aes(y = WCR_Tol_down), group = 4, color = pallette$red, size = 1, linetype = 5)+
    # label "Tolerance"
    geom_text(aes(x = min(df_chart$Period), y = df_chart$WCR_Tol[1]), label="Tolerance", nudge_x = 1, nudge_y = df_chart$WCR_Tol[1]/20)+
    # formatting axes
    scale_y_continuous(labels = function(x){scales::percent(x)}, name = "Forecast - Plan (negative means forecast WCR is lower than plan)")+ # up is forecast ahead of plan, down is forecast behind plan
    scale_x_discrete(name = "Date", labels = df_chart$Month)+ 
    # adding year labels
    lapply( 
      seq(1,length(unique(df_chart$Year.x)))
      ,function(year)
      {geom_text(
        aes(
          x = df_chart$Period[match(unique(df_chart$Year.x)[year], df_chart$Year.x)],
          y = min(df_chart$WCR_Tol_down, df_chart$csum_diff_WCR_minuses) - df_chart$WCR_Tol[1]/20
        )
        ,label = unique(df_chart$Year.x)[year]
        ,na.rm=TRUE
      )
      }
    )+
    # set theme
    theme_bw()
  
  print(p)
}

# function testing
# fn.chart.WCR.chart2(df_test)