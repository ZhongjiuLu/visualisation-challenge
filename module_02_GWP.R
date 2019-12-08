#### GWP Module ####
# This module supports the GWP tab with:
#  - creating all the required data for:
#     - chart 1
#     - chart 2 
#  - creating the plotting functions for
#     - chart 1
#     - chart 2 

#### Libraries ####
# This module uses the following libraries, which are loaded in the global.R file:
# library(ggplot2) # for charts
# library(scales)  # for formatting axis labels (into gbp)

#### Actuals Chart ####
fn.data.GWP.chart1 <- function(
  # data sources
  df_Tr = df_Trading,
  df_F  = df_Forecast,
  # data filters
  segment, #input$Segment
  forecast.date, #substr(input$As_At_Date,1,3)
  plan.version, #input$Plan_Version
  # chart controls
  historic.years = 1, #input$GWP_historic_years
  split_NB_RN = TRUE #input$GWP_chart1_RN_NB_Split
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@    
# This function generates the dataframe for chart 1
# inputs - df_Trading, df_Forecast = input tables loaded from .csv in global.R
#        - segment, forecast.date, plan.version, historic.years,split_NB_RN = dynamic inputs for filtering data, taken from input widgets
# returns - df_chart is a data frame for the GWP chart 1
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
  
  if (split_NB_RN){
    df_chart <- df_chart[!((df_chart$Version.Name.x == "Actual") & (df_chart$Policy.Duration == "Total")),]
  } else {
    df_chart <- df_chart[(df_chart$Policy.Duration == "Total"),]
  }
  
  df_chart$Act_GWP  <- as.numeric(apply(df_chart[,c("GWP","Version.Name.x"),drop=F],1,function(x){if(x["Version.Name.x"] == "Actual"){x["GWP"]}else{NA}}))
  df_chart$Plan_GWP <- as.numeric(apply(df_chart[,c("GWP","Version.Name.x"),drop=F],1,function(x){if(x["Version.Name.x"] == plan.version){x["GWP"]}else{NA}}))
  
  # reorder policy.duration factor
  df_chart$Policy.Duration <- factor(df_chart$Policy.Duration, levels = c( "Renewal","New Business","Total"))
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
  # might want to rename some stuff here to make it easier on the chart function
  # rename Forecast_GWP
  names(df_chart)[names(df_chart)=="Gross.Written.Premium"] <- "Forecast_GWP"
  
  return(df_chart)
  
}

#function testing
# 
# df_test_split <- fn.data.GWP.chart1(
#   df_Tr = df_Trading,
#   df_F  = df_Forecast,
#   segment = "Total", #input$Segment
#   forecast.date = "Nov", #input$As_At_Date
#   plan.version = "Plan", #input$Plan_Version
#   historic.years = 1,
#   split_NB_RN = TRUE
# )
# 
# df_test <- fn.data.GWP.chart1(
#   df_Tr = df_Trading,
#   df_F  = df_Forecast,
#   segment = "Total", #input$Segment
#   forecast.date = "Nov", #input$As_At_Date
#   plan.version = "Plan", #input$Plan_Version
#   historic.years = 1,
#   split_NB_RN = FALSE
# )

fn.chart.GWP.chart1 <- function(df_chart)
{
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function creats the plot for chart 1
# inputs  - df_chart (generated by fn.data.GWP.chart1)
# returns - p, a ggplot for chart 1
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
  # generate plot
  p <- ggplot(data=df_chart, aes(x=Period)) +
    # projections background rectangle
    geom_rect(
      aes(
        ymin = 0
        ,ymax = Inf
        ,xmin = min(df_chart[(df_chart$Version.Name.x != "Actual"),"Period"])
        ,xmax = max(df_chart$Period)
      ), fill = pallette$greypale
      , na.rm=TRUE
    )+
    # geom_text(
    #   aes(
    #     x = min(df_chart[(df_chart$Version.Name.x != "Actual"),"Period"])
    #     ,y = 0
    #   )
    #   ,label = "Projections"
    # )+
    # Forecast Line (red)
    geom_line(
      aes(x=Period, y=Forecast_GWP,group = 3, color = "Forecast")
      ,linetype = "longdash"
      ,size = 1
      ,na.rm=TRUE
    ) +
    # Plan Line (blue)
    geom_line(
      aes(x=Period, y=Plan_GWP,group = 4, color = "Plan")
      ,linetype = "longdash"
      ,size = 1
      ,na.rm=TRUE
    )+
    # colouring in the lines
    scale_color_manual(
      name = "Projections"
      ,labels = c("Forecast", "Plan")
      ,values = c(pallette$red, pallette$blue)
    )+
    # Historic actual GWP
    geom_bar(
      stat="identity"
      ,color = "#CC9900"
      ,aes(y = Act_GWP
           ,fill = Policy.Duration
           ,group = 1
      )
      ,na.rm=TRUE
    )+
    # Colouring in Historic Actual GWP bars
    scale_fill_manual(name = "GWP split", values =  c(pallette$yellow, pallette$greymid))+
    # formatting axes
    scale_y_continuous(
      labels = function(x) {dollar(x, prefix = "£")}
      ,name = "Gross Written Premium"
    ) + 
    scale_x_discrete(
      name = "Date"
      ,labels = df_chart[df_chart$Policy.Duration != "New Business","Month"] # a hack to prevent weirdness from double-counting months
    )+
    # Add year annotations
    lapply( seq(1,length(unique(df_chart$Year.x)))
      ,function(year) 
        {geom_text(
            aes(x = year*12-10, 
                y = (as.numeric(
                      substr(
                        as.character(
                          max(df_chart$GWP)
                        )
                      ,1
                      ,1
                      )
                    )+1)*10^floor(
                      log10(
                        max(df_chart$GWP)
                      )
                    )
            )
            ,label = unique(df_chart$Year.x)[year]
         )
        }
    )+
    # set theme - set the background to blank white, rotate the axis text, set legend position
    theme(panel.background = element_blank()
          ,axis.text.x = element_text(angle = 30, hjust = 1)
          ,legend.position=c(0.90, 0.30))
  
  # return the plot
  suppressWarnings(print(p))
}

# # function testing - using df_test from module_WCR
# fn.chart.GWP.chart1(df_chart = df_test_split)
# fn.chart.GWP.chart1(df_chart = df_test)

#### Tolerance Chart ####
fn.data.GWP.chart2 <- function(
  # data sources
  df_T  = df_Tolerance,
  df_Tr = df_Trading,
  df_F  = df_Forecast,
  # data filters
  segment, #input$Segment
  plan.version, #input$Plan_Version
  forecast.date #substr(input$As_At_Date,1,3)
){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function generates the data for GWP chart 2
# inputs - df_Trading, df_Forecast = input tables loaded from .csv in global.R
#        - df_Tolerance = dynamically generatd table in sympathy with input slider
#        - segment, forecast.date, plan.version = dynamic inputs for filtering data, taken from input widgets
# returns - df_chart is a data frame for the GWP chart 2
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

# function testing:
# df_test <- fn.data.GWP.chart2(
#   df_T  = df_Tolerance,
#   df_Tr = df_Trading,
#   df_F  = df_Forecast,
#   segment = "Total",
#   plan.version = "Plan",
#   forecast.date = "Nov"
# )

fn.chart.GWP.chart2 <- function(df_chart){
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# This function creats the plot for chart 2
# inputs  - df_chart (generated by fn.data.GWP.chart2)
# returns - p, a ggplot for chart 2
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
  
  # generate plot
  p<- ggplot(data = df_chart, aes(x = Period))+
    # up and down bars
    geom_bar(aes(y = csum_diff_GWP_pluses), stat = "identity", fill = pallette$greenpale)+
    geom_bar(aes(y = csum_diff_GWP_minuses), stat = "identity", fill = pallette$redpale)+
    # tolerance lines
    geom_line(aes(y = GWP_Tol)     , group = 3, color = pallette$red, size = 1, linetype = 5)+
    geom_line(aes(y = GWP_Tol_down), group = 4, color = pallette$red, size = 1, linetype = 5)+
    # label "Tolerance"
    geom_text(aes(x = min(df_chart$Period), y = df_chart$GWP_Tol[1]), label="Tolerance", nudge_x = 1, nudge_y = df_chart$GWP_Tol[1]/20)+
    # formatting axes
    scale_y_continuous(labels = function(x){dollar(x, prefix = "£")}, name = "Forecast - Plan (negative means forecast is behind plan)")+ # up is forecast ahead of plan, down is forecast behind plan
    scale_x_discrete(name = "Date", labels = df_chart$Month)+ 
    # adding year labels
    lapply( 
      seq(1,length(unique(df_chart$Year.x)))
      ,function(year)
        {geom_text(
           aes(
             x = df_chart$Period[match(unique(df_chart$Year.x)[year], df_chart$Year.x)],
             y = min(df_chart$GWP_Tol_down, df_chart$csum_diff_GWP_minuses) - df_chart$GWP_Tol[1]/20
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
# fn.chart.GWP.chart2(df_test)