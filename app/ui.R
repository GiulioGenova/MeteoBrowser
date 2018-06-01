#devtools::install_git("https://gitlab.inf.unibz.it/earth_observation_public//MonalisR")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("tibble")) install.packages("tibble")
if (!require("tidyr")) install.packages("tidyr")
if (!require("spdplyr")) install.packages("spdplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("httr")) install.packages("httr")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("stringr")) install.packages("stringr")

library(dplyr)
library(lubridate)
#library(plotly)
library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(DT)
#library(shinycssloaders)
#library(rgdal)
#library(shinyBS)

ui <- dashboardPage(#useShinyjs(),
  skin = "blue",
  dashboardHeader(title = "Province Open Data stations"),
  dashboardSidebar(disable = T,
    sidebarMenu(
      
      menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o"))#,
      #menuItem("map", tabName = "map", icon = icon("info-circle"))#,
      #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
    )),
  dashboardBody(#tags$a(href="javascript:history.go(0)", 
                 #      popify(tags$i(class="fa fa-refresh fa-5x"),
                     #         title = "Reload", 
                    #          content = "Click here to restart the Shiny session",
                     #         placement = "right")),
    
    #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                #tags$style(type = "text/css", "#map2 {height: calc(100vh - 350px) !important;}"),
                #tags$style(type = "text/css", "#plotall {height: calc(100vh - 200px) !important;}"),
                #tags$style(type = "text/css", "#plotair {height: calc(100vh - 200px) !important;}"),
                # Boxes need to be put in a row (or column)
                tabItems(
                  
                  tabItem(tabName = "Data",
                          
                          fluidRow(
                           # box(width = 2,selectInput("Station", label = h4("Station"),#, 
                            #                          choices = getMeteoStat()$NAME_D,selected = "Bozen",multiple = T),collapsible = T),#,selected = 'ALL' 
                            #box(width = 2,selectInput("Sensor", label = h4("Sensor"),#, 
                             #                         choices = getMeteoSensor()$Sensor,selected = "LT",multiple = T)),#,selected = 'ALL' 
                            
                    box(width = 4,collapsible = T,dateRangeInput(label = h4("Pick a date range"),inputId = "daterange",separator = " - ",min = "2000-01-01",
                                                         start = Sys.Date()-3,
                                                         end = Sys.Date()+1),
                        selectInput("round",label = h4("Time aggregation"),choices = list("raw","hour","day","week","month","year")),
                        
                        #actionButton(label= "update selection","refresh"),
                        selectInput("gather",
                  label = h4("Choose between long or wide table to download"),choices = list("long","wide")),
                  conditionalPanel(condition = "output.rightdate",br(),actionButton(label= "Download selected data","refresh")) ,
                  #conditionalPanel(condition = "output.rightdate",br(),actionButton( "stop",label = "Stop Download (reload page)",class="btn-danger")),#,onclick="Shiny.onInputChange('stopThis',true)"
                  conditionalPanel(condition = "output.tablebuilt",br(),#"input.daterange[1]<=input.daterange[2]"
                                   downloadLink('downloadData', h4('Save as csv') ) 
                                                            ),
                
                 verbatimTextOutput("message"),
                 verbatimTextOutput("selected")),
                  box(width = 8,leafletOutput("map"),collapsible = T)#,
                            #box(width = 2,selectInput("round",label = h4("Time aggregation"),choices = list("hour","day","week","month","year","5 mins","15 mins"))),
                            #box(width = 2,downloadLink('downloadData', h4('Download')),actionButton(label= "update selection","refresh"))
                    
                          )#,fluidRow(verbatimTextOutput("Click_text")
                            
                          #)
                 #,fluidRow(conditionalPanel(condition = "output.tablebuilt",br(),
                  #                          downloadLink('downloadData', h4('Download'))
                  #                         )
                  #         ) 
                 ,fluidRow(
                            #box(width=12,
                   helpText("Select stations and parameters you want to dowload by filtering the table below.",
                             "To stop the download refresh the page"),
                                DTOutput('table')
                                #,collapsible = T)#,tableOutput('table')
                            )
                    #,
                    #fluidRow(box(width = 6,leafletOutput("map")))
                          
                  )
                  ,
                  tabItem(tabName = "map",fluidRow(downloadLink('downloadData2', h4('Download')))
                )
  )
))
