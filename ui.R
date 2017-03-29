
library(shiny)
library(plotly)
library(tidyr)
library(lubridate)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Impact analysis of severe weather between 1990 and 2011"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(

        dateRangeInput("yearsrange", "Years range:",
                       start  = "1990-01-01",
                       end    = "2011-12-31",
                       min    = "1990-01-01",
                       max    = "2011-12-31",
                       format = "yyyy-mm-dd"),
        plotOutput("histYears"),
        hr(),
        helpText("Data originaly from National Oceanic and Atmospheric Administration (NOAA), 
                 was cleanup a bit and prepared for this app.")
        
    ),
    # Main panel to show the data analysis through tables, plots and maps
    mainPanel(
               tabsetPanel(type = "tabs",
                   tabPanel("Introduction", withTags({
                       div(class="header", checked=NA,
                           h2("Context"),
                           p("This is an interactive application written in Shiny, 
                             that allows one to explore the storm events reported in 
                             the US. This application uses real data collected by 
                             the National Oceanic and Atmospheric Administration (NOAA) 
                             during the years, although for this application the data 
                             was prepared. It contains information of diverse storm 
                             events for the years shown in the histogram on the left."),
                           h2("How to use this app?"),
                           p("This application is reactive, meaning you will see 
                             different results being shown depending on the selection 
                             you do on the dates input on the left. There are only two 
                             steps involved: "),
                           br(),
                           p("1. Select a date range using the date inputs field on 
                             the left side.", 
                             br(),
                             "2. Choose one of the tabs above to show the information in different forms:"),
                           br(),
                           p(strong("Table:"), "shows the information in an interactive table. You can search the table and filter information in many convenient ways."),
                           p(strong("Harm:"), "On this tab you can find the information of the impact for the population of diverse storm events filtered for the date range specified. This information is shown using a bar plot and a map of the US."),
                           p(strong("Damage:"), "On this tab you can find the information of the impact of damages (in value) of diverse storm events filtered for the date range specified. This information is shown using a bar plot and a map of the US.")
                       )
                   })),
                   tabPanel("Table", br(), dataTableOutput("table")),
                   tabPanel("Harm", br(), plotOutput("plotHarm"), br(),
                            plotlyOutput("mapHarm")),
                   tabPanel("Damage", br(), plotOutput("plotDamage"), br(),
                            plotlyOutput("mapDamage"))
                   ) 
       )
    )
))