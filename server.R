server <- function(input,output, session){

  #### rdf.Tolerance - reactive dataframe based on input slider ####
  rdf.Tolerance <- reactive({fn.Tolerance(MaterialityPC = input$slider_Tolerance/100)})

  #### Debugging ####
  output$debug <- #renderPrint(as.character(input$Segment)
     renderDataTable(
        datatable(rfn.data.Profit.Tolerance_Breaches(), options = list(dom = 't', pageLength = 12), rownames = FALSE)
     )
  
  #### Functions by UI tab:####
  #### / ####
  #### Profit Summary ####
  
  #### /- Segment Logo ####
  output$Profit_Segment_Info <- renderUI({
    attachmentBlock(
      src = df_Segment_Branding[df_Segment_Branding == input$Segment,"src"],
      title = input$Segment,
      #title_url = "",
      ""
    )
  })
  
  #### /- ValueBox - Profit Impact ####
  rfn.data.Profit.ValueBox <- reactive({
     fn.data.Profit.ValueBox(
        df_P             = df_Profit
        ,segment         = input$Segment
        ,forecast.date   = substr(input$As_At_Date,1,3)
        ,plan.version    = input$Plan_Version
        ,forecast.window = input$slider_forecast_horizon
     )
  })
  
  output$vb_Total_Profit_Impact <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = rfn.data.Profit.ValueBox()
      ,subtitle = HTML("Total Profit Impact /m")
      ,icon     = icon("gbp")
      ,color    = if(rfn.data.Profit.ValueBox()>0){"green"}else{"red"}
    )
  })
  
  #### /- Profit Table ####
  rfn.data.Profit.Table <- reactive({
     fn.data.Profit.Table(
        df_P             = df_Profit
        ,df_B            = df_Baseline
        ,segment         = input$Segment
        ,forecast.date   = substr(input$As_At_Date,1,3)
        ,plan.version    = input$Plan_Version
        ,forecast.window = input$slider_forecast_horizon
     )
  })
  
  output$DT_Profit_Impacts <- renderDataTable({
     df_Tolerance <- rdf.Tolerance()
     tolerance_limits <- df_Tolerance[(df_Tolerance$Segment == "Total"),"Profit_Materiality"]/1000000
     datatable(
        data = rfn.data.Profit.Table()
        ,options = list(
           dom = 't'
           ,pageLength = 12
           ,autoWidth = TRUE
           ,columnDefs = list(list(width = '50px', targets = "Written.Contribution.PC"))
        )
        ,rownames = FALSE
     ) %>% 
        formatPercentage("Written.Contribution.PC") %>%
        formatStyle(
           'Profit Impact',
           color = styleInterval(c(0), c("red","green")) # very tricky to get this to depend on tolerance limits per row
        )
  }) 
  
  #### /- Chart - Plan vs Forecast ####
  
  rfn.plot.Profit.Chart <- reactive({
     df_Tolerance <- rdf.Tolerance()
     fn.plot.Profit.Chart(
        fn.data.Profit.Chart(
           df_Tol = df_Tolerance,
           segment = input$Segment, 
           forecast.date = substr(input$As_At_Date,1,3), 
           plan.version = input$Plan_Version, 
           forecast.window = input$slider_forecast_horizon
        )
     )
  })
  
  output$plot_Profit_Impacts <- renderPlot({
     rfn.plot.Profit.Chart()
  })
  
  #### /- Profit Indicators ####
  rfn.data.Profit.Tolerance_Breaches <- reactive({
     df_Tolerance <- rdf.Tolerance()
     fn.data.Profit.Tolerance_Breaches(
        df_P             = df_Profit
        ,df_B            = df_Baseline
        ,df_Tol          = df_Tolerance
        #,segment         = input$Segment
        ,forecast.date   = substr(input$As_At_Date,1,3)
        ,plan.version    = input$Plan_Version
        ,forecast.window = input$slider_forecast_horizon
        ,sort.order      = input$tolerance_breach_sort_order
     )
  })
  
  #### /- /- Gauge ####
  output$Tolerance_gauge <- flexdashboard::renderGauge({ 
     df_Tol_Breaches <- rfn.data.Profit.Tolerance_Breaches()
     gauge(
        value = NROW(df_Tol_Breaches)
        ,min = 0
        ,max = 9 #need to be dynamic, based on whether all segments, or individual segments are selected
        ,sectors = gaugeSectors(
           success  = c(0,3)
           ,warning = c(4,7)
           ,danger  = c(8,9)
        )
        #,colors = c() #choose 3 colours for ranges
        ,label = "Performance Indicators"
     )
  })
  
  #### /- /- Newsfeed ####
  output$Tolerance_Newsfeed <- renderUI({
     df_Tol_Breaches <- rfn.data.Profit.Tolerance_Breaches()
     iconlist <- list(
       "GWP" = "gbp",
       "Day1.WCR" = "asterisk",
       "Claims.Frequency" = "ambulance",
       "Average.Cost" = "money"
     )
     tagList(
        apply(df_Tol_Breaches,1,
              function(x){
                 boxComment(title = x["Segment"], date = x["Breach_Period"]
                            ,src = df_Segment_Branding[df_Segment_Branding == x["Segment"],"src"]
                            ,HTML(paste(
                               "<span "
                               ,if(as.numeric(x["Materiality"])<0){" style = color:red;"}else{" style = color:green;"}
                               ,">"
                               ,"&pound;"
                               ,"<b>"
                               ,as.character(round(as.numeric(x["Materiality"]),-5)/1000000)
                               ,"</b>"
                               ,"m"
                               ,"</span>"
                               ,"<br>")
                            )
                            ,icon(iconlist[x["Most_Material_Metric"]])
                            ,x["Most_Material_Metric"]
                            
                 )
              }
        )
     )
  })
  
  #### / ####
  #### Trading - GWP ####
  #### /- Segment Logo ####
  output$GWP_Segment_Info <- renderUI({
    attachmentBlock(
      src = df_Segment_Branding[df_Segment_Branding == input$Segment,"src"],
      title = input$Segment,
      #title_url = "",
      ""
    )
  })
  
  #### /- ValueBox - Profit Impact ####
  rfn.data.ValueBox.Profit_Impact.GWP <- reactive({
    fn.data.ValueBox.Profit_Impact(
      df_P = df_Profit,
      Metric = "GWP", #static input from c("GWP", "Day1.WCR","Average.Cost", "Claims.Frequency")
      segment = input$Segment,
      forecast.date = substr(input$As_At_Date,1,3),
      plan.version = input$Plan_Version
    )
  })
  
  output$vb_GWP_3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = rfn.data.ValueBox.Profit_Impact.GWP()
      ,subtitle = HTML("Total Profit Impact /m")
      ,icon     = icon("gbp")
      ,color    = if(rfn.data.ValueBox.Profit_Impact.GWP()>=0){"green"}else{"red"}
    )
  })
  
  #### /- ValueBox - Long Term Average ####
  output$value_box_gwp <- shinydashboard::renderValueBox({
    
    input_value <- df_Baseline[(df_Baseline$Segment == input$Segment), "Annual.GWP"]
    input_value <- input_value/1000000
    
    shinydashboard::valueBox(
      value = paste0(input_value, " Million")
      ,subtitle = "Annual GWP"
      ,icon = icon("gbp")
      ,color = "yellow"
    )
  })
  
  #### /- Chart 1 - Historic Data and Projections ####
  rfn.data.GWP.chart1 <- reactive({
    fn.data.GWP.chart1(
      df_Tr = df_Trading,
      df_F  = df_Forecast,
      segment = input$Segment,
      forecast.date = substr(input$As_At_Date,1,3),
      plan.version = input$Plan_Version,
      historic.years = input$GWP_historic_years,
      split_NB_RN  = input$GWP_chart1_RN_NB_Split
    )
  })
  
  output$gwp_plot <- renderPlot({
    fn.chart.GWP.chart1(rfn.data.GWP.chart1())
  }) 
  
  #### /- Chart 2 - Plan vs Forecast ####
  rfn.data.GWP.chart2 <- reactive({
    df_Tolerance = fn.Tolerance(df_Baseline, MaterialityPC = input$slider_Tolerance_GWP/100)
    fn.data.GWP.chart2(
      df_T  = df_Tolerance,
      df_Tr = df_Trading,
      df_F  = df_Forecast,
      segment = input$Segment,
      plan.version = input$Plan_Version,
      forecast.date = substr(input$As_At_Date,1,3)
    )
  })
  
  output$GWP_Chart2_Tolerance <- renderPlot({fn.chart.GWP.chart2(rfn.data.GWP.chart2())})
  
  #### / ####
  #### Trading - WCR ####
  #### /- Segment Logo ####
  output$WCR_Segment_Info <- renderUI({
    attachmentBlock(
      src = df_Segment_Branding[df_Segment_Branding == input$Segment,"src"],
      title = input$Segment,
      #title_url = "",
      ""
    )
  })
  
  #### /- ValueBox - Profit Impact ####
  rfn.data.WCR.valueBox2 <- reactive({
    fn.data.WCR.valueBox2(
      df_P = df_Profit,
      segment = input$Segment,
      forecast.date = substr(input$As_At_Date,1,3),
      plan.version = input$Plan_Version
    )
  })
  
  output$vb_WCR_3 <- renderValueBox({
     shinydashboard::valueBox(
        value = rfn.data.WCR.valueBox2()
        ,color = if(rfn.data.WCR.valueBox2()>0){"green"}else{"red"}
        ,subtitle = HTML("Profit impact for this segment from Day 1 WCR &pound;/m")
        ,icon = icon("asterisk")
        ,width = NULL
     )
  })
  
  #### /- ValueBox - Long Term Average ####
  output$vb_WCR_1 <- renderValueBox({
    shinydashboard::valueBox(
      value = fn.data.WCR.valueBox1(segment = input$Segment)
      ,width = NULL
      ,color = "yellow"
      ,subtitle = "Long Term Average Day 1 WCR"
      ,icon = icon("asterisk")
    )
  })
  
  #### /- Chart 1 - Historic Data and Projections ####
  rfn.plot.WCR.chart1 <- reactive({
     fn.plot.WCR.chart1(
        fn.data.WCR.chart1(
           df_Tr = df_Trading
           ,df_F  = df_Forecast
           ,segment =input$Segment
           ,forecast.date = substr(input$As_At_Date,1,3)
           ,plan.version  = input$Plan_Version
           ,historic.years = input$WCR_historic_years
        )
        ,split_NB_RN = input$WCR_Split_NB_RN
     )
  })
  
  output$WCR_chart1 <- renderPlot({rfn.plot.WCR.chart1()})
  
  #### /- Chart 2 - Plan vs Forecast ####
  rfn.data.WCR.chart2 <- reactive({
    df_Tolerance = fn.Tolerance(df_Baseline, MaterialityPC = input$slider_Tolerance_WCR/100)
    fn.data.WCR.chart2(
      df_T  = df_Tolerance,
      df_Tr = df_Trading,
      df_F  = df_Forecast,
      segment = input$Segment,
      plan.version = input$Plan_Version,
      forecast.date = substr(input$As_At_Date,1,3)
    )
  })
  
  output$WCR_chart2 <- renderPlot({fn.chart.WCR.chart2(rfn.data.WCR.chart2())})
  
  #### / ####
  #### Claims  - Freq ####
  
  #### /- Segment Logo ####
  output$Claims_Freq_Segment_Info <- renderUI({
    attachmentBlock(
      src = df_Segment_Branding[df_Segment_Branding == input$Segment,"src"],
      title = input$Segment,
      #title_url = "",
      ""
    )
  })
  
  #### /- ValueBox - Profit Impact ####
  rfn.data.ValueBox.Profit_Impact.Claims.Freq <- reactive({
    fn.data.ValueBox.Profit_Impact(
      df_P = df_Profit,
      Metric = "Claims.Frequency", #static input from c("GWP", "Day1.WCR","Average.Cost", "Claims.Frequency")
      segment = input$Segment,
      forecast.date = substr(input$As_At_Date,1,3),
      plan.version = input$Plan_Version
    )
  })
  
  output$vb_Claims_Freq_Impact <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = rfn.data.ValueBox.Profit_Impact.Claims.Freq()
      ,subtitle = HTML("Total Profit Impact /m")
      ,icon     = icon("ambulance")
      ,color    = if(rfn.data.ValueBox.Profit_Impact.Claims.Freq()>=0){"green"}else{"red"}
    )
  })
  
  #### /- ValueBox - Long Term Average ####
  output$vb_LTA_Claims_Freq <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = paste(as.character(round(fn.claims.RnJ.LTAFrequency(rfn.data.Claims.Chart1.Actual())*100,3)),"%")
      ,subtitle = HTML("Long Term Average Claims Frequency %")
      ,icon     = icon("ambulance")
      ,color    = "yellow"
    )
  })
  
  #### /- Chart 1 - Historic Data and Projections ####
  rfn.data.Claims.Chart1.Forecast <- reactive({ 
    fn.data.claims.RnJ.Forecast(
      df_F = df_Forecast
      ,Class = input$Segment
      ,As_At_Date = input$As_At_Date
    )  
  })
  
  rfn.data.Claims.Chart1.Actual <- reactive({ 
    fn.data.claims.RnJ.Claims.Actuals(
      df_C = df_Claims
      ,Class = input$Segment
    )  
  })
  
  rfn.data.Claims.Chart1.Plan <- reactive({ 
    fn.data.claims.RnJ.Claims.Plan(
      df_C = df_Claims
      ,Class = input$Segment
      ,Plan_Version = input$Plan_Version
    )  
  })
  
  output$plot_Claims_Freq_1 <- renderPlot({
    fn.chart.claims.RnJ.Freq(
      df_act = rfn.data.Claims.Chart1.Actual()
      ,df_plan = rfn.data.Claims.Chart1.Plan ()
      ,df_fore = rfn.data.Claims.Chart1.Forecast()
      ,Class = input$Segment
    )
  })
  
  #### /- Chart 2 - Plan vs Forecast ####
  rfn.data.Claims.Chart2.Freq <- reactive({
    df_Tolerance <- fn.Tolerance(df_Baseline, MaterialityPC = input$slider_Tolerance_Claims_Freq/100)
    fn.data.claims.Devon(
      df_T             = df_Tolerance
      ,df_C            = df_Claims
      ,df_F            = df_Forecast
      ,segment         = input$Segment
      ,plan.version    = input$Plan_Version
      ,forecast.date   = substr(input$As_At_Date,1,3)
    )
  })
  
  output$plot_Claims_Freq_2 <- renderPlot({
    fn.chart.claims.Devon.Freq(rfn.data.Claims.Chart2.Freq())
  })
  
  #### / ####
  #### Claims  - AC   ####
  
  #### /- Segment Logo ####
  output$Claims_AC_Segment_Info <- renderUI({
    attachmentBlock(
      src = df_Segment_Branding[df_Segment_Branding == input$Segment,"src"],
      title = input$Segment,
      #title_url = "",
      ""
    )
  })
  
  #### /- ValueBox - Profit Impact ####
  rfn.data.ValueBox.Profit_Impact.Claims.AC <- reactive({
    fn.data.ValueBox.Profit_Impact(
      df_P = df_Profit,
      Metric = "Average.Cost", #static input from c("GWP", "Day1.WCR","Average.Cost", "Claims.Frequency")
      segment = input$Segment,
      forecast.date = substr(input$As_At_Date,1,3),
      plan.version = input$Plan_Version
    )
  })
  
  output$vb_Claims_AC_Impact <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = rfn.data.ValueBox.Profit_Impact.Claims.AC()
      ,subtitle = HTML("Total Profit Impact /m")
      ,icon     = icon("money")
      ,color    = if(rfn.data.ValueBox.Profit_Impact.Claims.AC()>=0){"green"}else{"red"}
    )
  })
  
  #### /- ValueBox - Long Term Average ####
  output$vb_LTA_Claims_AC <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( 
      value     = round(fn.claims.RnJ.LTAAverageCost(rfn.data.Claims.Chart1.Actual()),-0)
      ,subtitle = HTML("Long Term Average Claims Cost /&pound;")
      ,icon     = icon("money")
      ,color    = "yellow"
    )
  })
  
  #### /- Chart 1 - Historic Data and Projections ####
  output$plot_Claims_AC_1 <- renderPlot({
    fn.chart.claims.RnJ.AC(
      df_act   = rfn.data.Claims.Chart1.Actual()
      ,df_plan = rfn.data.Claims.Chart1.Plan ()
      ,df_fore = rfn.data.Claims.Chart1.Forecast()
      ,Class   = input$Segment
    )
  })

  #### /- Chart 2 - Plan vs Forecast ####
  rfn.data.Claims.Chart2.AC <- reactive({
    df_Tolerance <- fn.Tolerance(df_Baseline, MaterialityPC = input$slider_Tolerance_Claims_AC/100)
    fn.data.claims.Devon(
      df_T             = df_Tolerance
      ,df_C            = df_Claims
      ,df_F            = df_Forecast
      ,segment         = input$Segment
      ,plan.version    = input$Plan_Version
      ,forecast.date   = substr(input$As_At_Date,1,3)
    )
  })
  
  output$plot_Claims_AC_2 <- renderPlot({
    fn.chart.claims.Devon.AC(rfn.data.Claims.Chart2.AC())
  })
  
  #### / ####
  #### Claims - Weather ####
  #### /- Chart 1 - Weather Data ####
  output$Weather_Chart <- renderPlot({fn.plot.Weather(df_Weather)})
  #### /- Chart 2 - Claims #### 
  output$Weather_Claims <- renderPlot({fn.plot.Weather.Claims(df_Claims)})
  #### /- SocialFeed - Met Office Weather Events Catalogue ####
  output$Weather_Newsfeed <- renderUI({
   df_Weather_Newsfeed <- fn.data.Weather_Newsfeed()
   tagList(
     apply(df_Weather_Newsfeed,1,
       function(x){
         boxComment(
            title = paste(x["Index"], " ", x["Name"])
            ,date = x["Date"]
            ,src = x["src"]
            ,HTML(paste(
              "<span>"
              ,x["Description"]
              ,"<a href =",x["Hyperlink"],"> "
              ,"<b> read more </b>"
              ,"</a></span>"
              )
            )
         )
       }
     )
   )
  })
  
  #### / ####
  #### Claims - Large ####
  #### /- None ####
}
