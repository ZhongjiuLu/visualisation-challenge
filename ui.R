ui <- dashboardPage(
  #### Header ####
  dashboardHeader(
    title = ""
    ,tags$li(
      class = "dropdown"
      ,tags$img(src="Images/quantum_logo.png", height=50)
    )
  )
  #### / ####
  #### Sidebar ####
  #### /- Tab Menu ####
  ,dashboardSidebar(
    sidebarMenu(
      menuItem("Summary View", tabName = "Summary_View")
      ,menuItem(
        "Trading"
        ,tabName = "Trading"
        ,startExpanded = TRUE
        ,menuSubItem("GWP"          , tabName = "Metrics_Trading_GWP"     , icon = icon("gbp"))
        ,menuSubItem("Day1WCR"      , tabName = "Metrics_Trading_Day1_WCR", icon = icon("asterisk"))
      )
      ,menuItem(
        "Claims"
        ,tabName = "Claims"
        ,startExpanded = TRUE
        ,menuSubItem("Frequency"    , tabName = "Metrics_Claims_Freq"   , icon = icon("ambulance"))
        ,menuSubItem("Average Cost" , tabName = "Metrics_Claims_AC"     , icon = icon("money"))
        ,menuSubItem("Weather"      , tabName = "Metrics_Claims_Weather", icon = icon("umbrella"))
        #,menuSubItem("Large"        , tabName = "Metrics_Claims_Large"  , icon = icon("building"))
      )
   #### /- Controls ####
      ,menuItem(h3("Controls"),
                tabName = "Controls")
      ,pickerInput(
        "Segment"
        ,label = "Segment"
        ,choices = df_Segment_Branding$Segment
        ,choicesOpt = list(content = df_Segment_Branding$img)
        ,selected = "Total"
      )
      ,selectInput(
        inputId  = "As_At_Date"
        ,label   = "As at date"
        ,choices = c("Nov 2018", "Dec 2018")
        ,selected = "201812"
      )
      ,selectInput(
        inputId  = "Plan_Version"
        ,label   = "Plan Version"
        ,choices = c("Plan", "Plan V2")
        ,selected = "201812"
      ) 
    )
  )
  #### / ####
  #### Body ####
  ,dashboardBody(
    #### /- custom css Stylesheet ####
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
    #### /- Debugging ####
    #,dataTableOutput("debug")
    #,textOutput("debug")
    ,tabItems(
      #### / ####
      #### /- Tab - Profit Summary ####
      tabItem(
        tabName = "Summary_View"
        #### /-/- Top Row ####
        ,fluidRow(
          column(9,
           fluidRow(
             column(5,
               #### /-/-/- Left Side box ####
               box(
                 width = NULL
                 ,title = "Info"
                 ,status = "primary"
                 ,solidheader = TRUE
                 ,collapsible = TRUE
                 ,collapsed = FALSE
                 ,footer = NULL
                 ,fluidRow(
                   column(6,
                     uiOutput("Profit_Segment_Info")
                   )
                   ,column(6,
                     valueBoxOutput("vb_Total_Profit_Impact")
                   )
                 )
               )
             )
             ,column(7,
                #### /-/-/- Right Side box ####
                box(
                  width = NULL
                  ,title = "User Controls"
                  ,status = "primary"
                  ,solidheader = TRUE
                  ,collapsible = TRUE
                  ,collapsed = FALSE
                  ,footer = NULL
                  ,fluidRow(
                    column(6, 
                     box(
                       status = "warning"
                       ,solidHeader = FALSE
                       ,width = NULL
                       ,sliderInput("slider_forecast_horizon", label = "Forecast Horizon (months)", min = 1, max = 12, value = 12)
                     )
                    )
                    ,column(6,
                      box(
                        status = "warning"
                        ,solidHeader = FALSE
                        ,width = NULL
                        ,sliderInput("slider_Tolerance", label = "Tolerance %", min = 0, max = 50, value = 10)
                      )
                    )
                  )
                )
             )
           )
           #### /-/- Second Row ####
           ,fluidRow(
             column(5,
               #### /-/-/- Chart 1 ####
               box(
                 width = NULL
                 ,title = "Total Profit Impacts"
                 ,status = "success"
                 ,solidheader = TRUE
                 ,collapsible = TRUE
                 ,collapsed = FALSE
                 ,footer = NULL
                 ,dataTableOutput("DT_Profit_Impacts")
                )
             )
             ,column(7,
               #### /-/-/- Chart 2 ####
               box(
                 width = NULL
                 ,title = "Cumulative Profit Impacts"
                 ,status = "success"
                 ,solidheader = TRUE
                 ,collapsible = TRUE
                 ,collapsed = FALSE
                 ,footer = NULL
                 ,plotOutput("plot_Profit_Impacts")
               )
             )
           )
         )
         #### /-/- Performance Indicators ####
         ,column(3
           ,socialBox(
             width = 12
             ,title = "Performance Indicators"
             ,src = "Images/Aviva.png"
             ,flexdashboard::gaugeOutput("Tolerance_gauge")
             ,HTML("<span><b>Sort Order</b><span>")
               ,selectInput("tolerance_breach_sort_order"
                            ,label = ""
                            , choices = c("Materiality", "Breach_Period")
                            , selected = "Materiality"
               )
             ,comments = uiOutput("Tolerance_Newsfeed")
           ) 
         )
        )
      )
      #### / ####
      #### /- Tab - Trading - GWP ####
      ,tabItem(
        tabName = "Metrics_Trading_GWP"
        #### /-/- Top Row ####
        ,fluidRow(
          column(6,
           #### /-/-/- Left Side Box ####
           box(
             width = NULL
             ,title = "Info"
             ,solidHeader = TRUE
             ,status = "primary"
             ,collapsible = TRUE
             ,fluidRow(
               column(6,uiOutput("GWP_Segment_Info"))
               ,column(6,valueBoxOutput("vb_GWP_3"))
             )  
           )
          )
          ,column(6,
            #### /-/-/- Right Side Box ####
            box(
              width = NULL
              ,title = "Info"
              ,solidHeader = TRUE
              ,status = "primary"
              ,collapsible = TRUE
              ,fluidRow(
                column(6,
                   box(
                     status = "warning"
                     ,solidHeader = FALSE
                     ,width = NULL
                     ,sliderInput("slider_Tolerance_GWP", label = "Tolerance %", min = 0, max = 50, value = 10)
                   )
                )
                ,column(6,valueBoxOutput("value_box_gwp"))
              )
            )
          )
        )
        #### /-/- Second Row ####
        ,fluidRow(
          column(
           width = 6
           #### /-/-/- Chart 1 ####
             ,box(
               width = NULL
               ,status = "warning"
               ,solidHeader = TRUE
               ,title = "Gross Written Premium - Historic Data and Projections"
               ,plotOutput(outputId = "gwp_plot")
               ,fluidRow(
                 column(4
                   ,checkboxInput("GWP_chart1_RN_NB_Split", label = "Split Renewal/New Business", value = FALSE)
                 )
                 ,column(8
                   ,sliderInput("GWP_historic_years"
                   ,label = "Number of Historic Years"
                   ,min   = 1
                   ,max   = 3
                   ,value = 1
                   )
                 )
               )
            )
          )
          ,column(
            width = 6
            #### /-/-/- Chart 2 ####
            ,box(
              width = NULL
              ,status = "warning"
              ,solidHeader = TRUE
              ,title = "Gross Written Premium - Plan vs Forecast (cumulative)"
              ,plotOutput(outputId = "GWP_Chart2_Tolerance")
            )
          )
        )
      )
      #### / ####
      #### /- Tab - Trading - Day 1 WCR ####
      ,tabItem(
        tabName = "Metrics_Trading_Day1_WCR"
        #### /-/- Top Row ####
        ,fluidRow(
          column(6,
            #### /-/-/- Left Side Box ####
            box(
              width = NULL
              ,status = "primary"
              ,solidheader = TRUE
              ,title = "Info"
              ,fluidRow(
                column(6,uiOutput("WCR_Segment_Info"))
                ,column(6,valueBoxOutput("vb_WCR_3"))
              )  
            )
          )
          ,column(6,
            #### /-/-/- Right Side Box ####
            box(
              width = NULL
              ,status = "primary"
              ,title = ""
              ,solidheader = TRUE
              ,fluidRow(
                column(6
                  ,box(
                    width = NULL
                    ,status = "warning"
                    ,sliderInput(
                      "slider_Tolerance_WCR"
                      ,label = "Tolerance %"
                      ,min = 0
                      ,max = 50
                      ,value = 10
                    )
                  )
                )
                ,column(6,valueBoxOutput("vb_WCR_1"))
              )  
            )
          )
        )
        #### /-/- Second Row ####
        ,fluidRow(
          column(6,
            #### /-/-/- Chart 1 ####
            box(
              width = NULL
              ,status = "warning"
              ,solidHeader = TRUE
              ,title = "Day 1 WCR - Historic Data and Projections"
              ,plotOutput("WCR_chart1")
              ,fluidRow(
                column(5
                  ,checkboxInput(
                    "WCR_Split_NB_RN"
                    ,label = "Split New business/Renewal"
                    ,value = FALSE
                  )
                )
                ,column(7
                  ,sliderInput(
                    "WCR_historic_years"
                    ,label = "Number of Historic Years"
                    ,min = 1 , max = 3
                    ,value = 1
                  )
                )
              )
            )
          )
          ,column(6,
            #### /-/-/- Chart 2 ####
            box(
              width = NULL
              ,status = "warning"
              ,solidHeader = TRUE
              ,title = "Day 1 WCR - Plan vs Forecast (cumulative)"
              ,plotOutput("WCR_chart2")
            )
          )
        )
      )
      #### / ####
      #### /- Tab - Claims - Frequency ####
      ,tabItem(tabName = "Metrics_Claims_Freq"
        #### /-/- Top Row ####
        ,fluidRow(
          column(6
            #### /-/-/- Left Side Box ####
            ,box(
               width = NULL
              ,title = "Info"
              ,status = "primary"
              ,solidheader = TRUE
              ,collapsible = TRUE
              ,collapsed = FALSE
              ,footer = NULL
              ,fluidRow(
                column(6
                  ,uiOutput("Claims_Freq_Segment_Info")
                )
                ,column(6
                  ,valueBoxOutput("vb_Claims_Freq_Impact")
                )
              )
            )
          )
          ,column(6
            #### /-/-/- Right Side Box ####
            ,box(
              width = NULL
              ,title = ""
              ,status = "primary"
              ,solidheader = TRUE
              ,collapsible = TRUE
              ,collapsed = FALSE
              ,footer = NULL
              ,fluidRow(
                column(6,
                  box(
                    status = "warning"
                    ,solidHeader = FALSE
                    ,width = NULL
                    ,sliderInput("slider_Tolerance_Claims_Freq", label = "Tolerance %", min = 0, max = 50, value = 10)
                  )
                )
                ,column(6
                  ,valueBoxOutput("vb_LTA_Claims_Freq")
                )
              )
            )
          )
        )
        #### /-/- Second Row ####
        ,fluidRow(
          column(6,
            #### /-/-/- Chart 1 ####
            box(
              width = NULL
              ,status = "warning"
              ,title = "Claims Frequency - Historic Data and Projections"
              ,solidHeader = TRUE
              ,plotOutput("plot_Claims_Freq_1")
            )
          )
          ,column(6,
            #### /-/-/- Chart 2 ####
            box(
              width = NULL
              ,status = "warning"
              ,title = "Claims Frequency - Plan vs Forecast (cumulative)"
              ,solidHeader = TRUE
              ,plotOutput("plot_Claims_Freq_2")
            )
          )
        )
      )
      #### / ####
      #### /- Tab - Claims - Average Cost ####
      ,tabItem(
        tabName = "Metrics_Claims_AC"
        #### /-/- Top Row ####
        ,fluidRow(
          column(6
           #### /-/-/- Left Side Box ####
           ,box(
             width = NULL
             ,title = "Info"
             ,status = "primary"
             ,solidheader = TRUE
             ,collapsible = TRUE
             ,collapsed = FALSE
             ,footer = NULL
             ,fluidRow(
               column(6
                 ,uiOutput("Claims_AC_Segment_Info")
               )
               ,column(6
                 ,valueBoxOutput("vb_Claims_AC_Impact")
               )
             )
           )
          )
          ,column(6
            #### /-/-/- Right Side Box ####
            ,box(
              width = NULL
              ,title = ""
              ,status = "primary"
              ,solidheader = TRUE
              ,collapsible = TRUE
              ,collapsed = FALSE
              ,footer = NULL
              ,fluidRow(
                column(6,
                   box(
                     status = "warning"
                     ,solidHeader = FALSE
                     ,width = NULL
                     ,sliderInput("slider_Tolerance_Claims_AC", label = "Tolerance %", min = 0, max = 50, value = 10)
                   )
                )
                ,column(6
                  ,valueBoxOutput("vb_LTA_Claims_AC")
                )
              )
            )
          )
        )
        #### /-/- Second Row ####
        ,fluidRow(
          column(6,
             #### /-/-/- Chart 1 ####
             box(
               width = NULL
               ,status = "warning"
               ,title = "Claims Average Cost - Historic Data and Projections"
               ,solidHeader = TRUE
               ,plotOutput("plot_Claims_AC_1")
             )
          )
          ,column(6,
              #### /-/-/- Chart 2 ####
              box(
                width = NULL
                ,status = "warning"
                ,title = "Claims Average Cost  - Plan vs Forecast  (cumulative)"
                ,solidHeader = TRUE
                ,plotOutput("plot_Claims_AC_2")
              )
          )
        )
      )
      #### / ####
      #### /- Tab - Claims - Weather ####
      ,tabItem(
        tabName = "Metrics_Claims_Weather"
        ,fluidRow(
          column(6,
            #### /-/- Weather Chart ####
            box(
              width = NULL
              ,title = "Weather Data"
              ,solidHeader = TRUE
              ,status = "info"
              ,collapsible = TRUE
              ,plotOutput("Weather_Chart")
              #,tags$img(src="Weather_Legend.png")
            )
            #### /-/- Claims Chart ####
            ,box(
              width = NULL
              ,title = "Weather Claims"
              ,solidHeader = TRUE
              ,status = "primary"
              ,plotOutput("Weather_Claims")
            )
          )
          ,column(6,
            #### /-/- Catalogue of Weather Events (Met Office links) ####
            socialBox(
              width = 12
              ,title = "Weather Events"
              ,src = "Images/Weather_Storm.png"
              ,tags$img(src="Images/rain.gif")
              ,HTML("<span><br>Weather Data Webscraped From: <br> <a href =https://www.metoffice.gov.uk/weather/> www.metoffice.gov.uk/weather/</a></span>")
              ,comments = uiOutput("Weather_Newsfeed")
            )
          )
        )
      )
      #### / ####
      #### /- Tab - Claims - Large ####
      #,tabItem(tabName = "Metrics_Claims_Large"
      #         ,tags$img(src = "Work_in_progress.png", width = 400)
      #         ,tags$img(src = "Marvel_Lunch.gif",height = 600)
      #)
    )
  )
)