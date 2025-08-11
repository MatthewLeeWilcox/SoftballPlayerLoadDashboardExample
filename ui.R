#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(DT)
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggforce)
library(plotly)
library(dplyr)
library(zoo)

library(bslib)
library(shiny)
library(htmltools)
library(plotly)
library(leaflet)

if (!dir.exists("www")) {
  dir.create("www")
}

cat(
  # "body {
  #   background-color: #FFFFFF; /* Duke White */
  #   color: #000000; /* Duke Black */
  #   font-family: sans-serif; /* Use a standard font */
  # }
  "body {
    background-image: url('duke_softball.jpg'); /* Set background image */
    background-position: bottom left;
    background-repeat: no-repeat;
    background-attachment: fixed; /* Keeps image in place while scrolling */
    color: #000000; /* Duke Black text */
    font-family: sans-serif;
  }

  .navbar {
    background-color: #001369; /* Duke Blue navbar */
    border-color: #001369;
  }

  .navbar-default .navbar-brand {
    color: #FFFFFF !important; /* White title text */
  }

  .navbar-default .navbar-nav > li > a {
    color: #FFFFFF !important; /* White navbar links */
  }

  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus {
    background-color: #001055; /* Slightly darker blue for active tab */
    color: #FFFFFF !important;
  }

  .panel-heading {
    background-color: #001369; /* Duke Blue panel headers */
    color: #FFFFFF;
    border-color: #001369;
  }

  .well {
    background-color: #f0f0f5; /* Light gray for wells */
    border: 1px solid #ddd;
  }

  .btn-primary {
    background-color: #001369; /* Duke Blue buttons */
    border-color: #001369;
  }

  .btn-primary:hover {
    background-color: #001055;
    border-color: #001055;
  }

  .card { /* Style for your card divs */
    border: 1px solid #ddd;
    border-radius: 5px;
    padding: 10px;
    margin-bottom: 10px;
    background-color: #FFFFFF; /* White card background */
    box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); /* Subtle shadow */
  }

  /* Plot styling (example) */
  .plot-container {
    background-color: #FFFFFF; /* White plot background */
  }

  .plot-container .point {
    color: #001369; /* Duke Blue points */
  }
  .plot-container .axis-text, .plot-container .axis-title{
    color: #000000;
  }
  .plot-container .legend-title, .plot-container .legend-text{
    color: #000000;
  }
  .table { /* Style for tables (DTOutput) */
    width: 100%;
    border-collapse: collapse;
    margin-top: 20px;
  }

  .table th, .table td {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
  }

  .table th {
    background-color: #001369; /* Duke Blue table headers */
    color: #FFFFFF;
  }
  ",
  file = "www/styles.css"
)

ui <- navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  title = "Softball Player Load",
  
  tabPanel("Line Comparison",
            sidebarLayout(
              sidebarPanel(
                selectInput("selected_player", "Select a Player", choices = character(0), multiple = FALSE),
                dateRangeInput("date_range", "Select Date Range:",
                               start = Sys.Date() - 30, # Default: last 30 days
                               end = Sys.Date(),
                               format = "yyyy-mm-dd"),
                checkboxInput("smooth_plot", "Player Load Flags "),
                
              ),
              mainPanel(
                h3(textOutput("player_name_title")),
                conditionalPanel("input.smooth_plot",
                                 fluidRow(
                                   column(6, card(
                                     height = 500,
                                     full_screen = TRUE,
                                     card_body(plotlyOutput("swing_load_range_plot"))
                                   )),
                                   column(6, card(
                                     height = 500,
                                     full_screen = TRUE,
                                     card_body(plotlyOutput("throw_range_plot"))
                                   ))
                                 ),
                                 fluidRow(
                                   column(6, card(
                                     height = 500,
                                     full_screen = TRUE,
                                     card_body(plotlyOutput("lower_load_range_plot"))
                                   )),
                                   column(6, card(
                                     height = 500,
                                     full_screen = TRUE,
                                     card_body(plotlyOutput("sprint_load_range_plot"))
                                   )),
                                 ),
                                 fluidRow(
                                   card(
                                     height = 500,
                                     full_screen = TRUE,
                                     card_body(plotlyOutput("dml_load_range_plot"))
                                   )
                                 )),
                conditionalPanel("!input.smooth_plot",
                     fluidRow(
                       column(6, card(
                         height = 500,
                         full_screen = TRUE,
                         card_body(plotlyOutput("player_swing_load"))
                       )),
                       column(6, card(
                         height = 500,
                         full_screen = TRUE,
                         card_body(plotlyOutput("player_throw_load"))
                       ))
                  
                    ),
                    fluidRow(
                        column(6, card(
                          height = 500,
                          full_screen = TRUE,
                          card_body(plotlyOutput("player_lower_load"))
                        )),
                        column(6, card(
                          height = 500,
                          full_screen = TRUE,
                          card_body(plotlyOutput("player_sprint"))
                        ))
                    ),
                    fluidRow(
                      card(
                        height = 500,
                        full_screen = TRUE,
                        card_body(plotlyOutput("player_dml"))
                      )
                    ) ),
                    fluidRow(
                      card(
                        height = 750,
                        full_screen = TRUE,
                        card_body(DTOutput("testing_filtered_table"))
                      )
                   
 )
)
            )
  ),
  
  tabPanel("Player Load Flagging",
           sidebarLayout(
             sidebarPanel(
               dateInput("input_date_flag", "Select Date:",
                         value = Sys.Date(),
                         format = "yyyy-mm-dd")
               
             ),
           mainPanel(
             DTOutput("load_flag")
  )
  )),
  tabPanel("Positional Players Daily Report",
           sidebarLayout(
             sidebarPanel(
               dateInput("input_date_day_summary", "Select Date Range:",
                         value = Sys.Date(),
                         format = "yyyy-mm-dd"),
               downloadButton("download_player_report", "Download PDF")
               
               )
             ,
             mainPanel(
               fluidRow(
                 column(6, card(
                   height = 500,
                   full_screen = TRUE,
                   card_body(plotlyOutput("day_swing_load"))
                 )),
                 column(6, card(
                   height = 500,
                   full_screen = TRUE,
                   card_body(plotlyOutput("day_throw_load"))
                 ))
                 
               ),
               fluidRow(
                 column(12, card(
                   height = 750,
                   full_screen = TRUE,
                   card_body(plotlyOutput("day_lower_load"))
                 )
                 )),
               fluidRow(
                 column(12, card(
                   height = 750,
                   full_screen = TRUE,
                   card_body(plotlyOutput("sprint_load"))
                 )
                 )
               ),
               fluidRow(
                 column(12, card(
                   height = 500,
                   full_screen = TRUE,
                   card_body(DTOutput("weekly_table_output"))
                 )
                 )
                            )
           ))),
  tabPanel("Pitchers Daily Report",
         sidebarLayout(
           sidebarPanel(
         
             sliderInput(
               "week_number",
               "Select Week of Season (Relative to Start):",
               min = 1,  
               max = 52,  
               value = 0,  
               step = 1      
             ),
             verbatimTextOutput("date_range"),
             downloadButton("download_pitcher_report", "Download PDF")
           )
           ,
           mainPanel(
             column(6,  uiOutput("pitcher_daily_plot"), height = 4000),
             column(6, uiOutput("pitcher_weekly_plot"), height = 4000)
            

             )
             
           )
  ),

tabPanel("Acute:Chronic Loads",
         sidebarLayout(
           sidebarPanel(
             selectInput("selected_player_ac", "Select a Players:", choices = character(0), multiple = TRUE),
             actionButton("select_all", "Select All"),
             dateRangeInput("ac_date_range", "Select Date Range:",
                            start = Sys.Date() - 30, 
                            end = Sys.Date(),
                            format = "yyyy-mm-dd"),
           ),
           mainPanel(
             
             fluidRow(
               column(6, card(
                 height = 500,
                 full_screen = TRUE,
                 card_body(plotlyOutput("ac_swing_load_plot"))
               )),
               column(6, card(
                 height = 500,
                 full_screen = TRUE,
                 card_body(plotlyOutput("ac_throw_load_plot"))
               ))
             ),
             fluidRow(
               column(6, card(
                 height = 500,
                 full_screen = TRUE,
                 card_body(plotlyOutput("ac_lower_load_plot"))
               )),
               column(6, card(
                 height = 500,
                 full_screen = TRUE,
                 card_body(plotlyOutput("ac_sprint_load_plot"))
               )),
             ),
             fluidRow(
               card(
                 height = 500,
                 full_screen = TRUE,
                 card_body(plotlyOutput("ac_dml_load_plot"))
               )),
               fluidRow(
                 card(
                   height = 750,
                   full_screen = TRUE,
                   card_body(DTOutput("ac_table"))
                 )
                  )
          )
)),
tabPanel("Report Info",
         sidebarLayout(
           sidebarPanel(
             sliderInput(
               "week_number2",
               "Select Week of Season (Relative to Start):",
               min = 1,  
               max = 52,  
               value = 0,  
               step = 1      
             ),
             verbatimTextOutput("date_range2")
             
           )
           ,
           mainPanel(
               DTOutput("player_pos_report_table"),DTOutput("pitcher_report_table")
                 ))
)
  
)

