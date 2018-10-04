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
#about = source(file.path(getwd(),'about.R'))

header <- dashboardHeader(titleWidth = 480)

anchor <- tags$a(href='http://www.eurac.edu/',
                 tags$img(style="vertical-align: bottom;width: 270px;padding-right: 10px;",
                          #src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'),#, height='60', width='50'
                          src='LogoProvEurac.PNG'),
                 'Meteo Browser South Tyrol',class='tit')

header$children[[2]]$children <- tags$div(anchor,class = 'name')


ui <- dashboardPage(#useShinyjs(),
  title= "Meteo Browser Südtirol",
  skin = "blue",
  
  header             
    ,
  dashboardSidebar(
    disable = F,
    sidebarMenuOutput("Data"),
    sidebarMenuOutput("about"),
    #sidebarMenuOutput("legend"),
    sidebarMenu(
                     #uiOutput("Data"),
                     #menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
                     #uiOutput("about"),
                     #menuItem("README", tabName = "about", icon = icon("info-circle")),
                     radioButtons("language", NULL,
                                  c("English" = "en",
                                    "Deutsch" = "de",
                                    "Italiano" = "it"))
                     #menuItem("map", tabName = "map", icon = icon("info-circle"))#,
                     #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
                   )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "script", type = "js", href = "scr.js")
      ),
    
    # Boxes need to be put in a row (or column)
    tabItems(
      
      tabItem(tabName = "Data",
              
              fluidRow(
                
                box(width = 4,collapsible = T,
                    
                    uiOutput("statlist"),
                    uiOutput("sensorlist"),
                    uiOutput("altitudelist"),
                    uiOutput("daterange"),
                    uiOutput("message"),
                    div(style="display: inline-block;vertical-align:top; width: 43%;",uiOutput("round")),
                    div(style="display: inline-block;vertical-align:top; width: 55%;",uiOutput("gather")),
                    
                    textOutput("downloadInstructions"),
                    div(style=" width: 40%;",
                        uiOutput("refresh")
                       )
                    
                    ,
                    div(style=" width: 100%;",
                    div(style="display: inline-block;vertical-align:top; width: 55%;",
                        uiOutput("save")
                        ),
                    div(style="display: inline-block;vertical-align:top; width: 40%;",
                        uiOutput("tabChoice")
                        )
                   )
                    
                   #,
                    
                    #htmlOutput("tableInstructions")
                    
                    
                  
              ),
                   
                box(width = 8,
                    verbatimTextOutput("selected"),
                    leafletOutput("map"),
                    #uiOutput("map"),
                    #conditionalPanel(condition ="output.spatialSelection",actionButton("deletebtn", "remove drawn")),
                    #uiOutput("deletebtn"),
                    textOutput("youSelStations"),
                    verbatimTextOutput("selected_list"),
                    textOutput("andSensors"),
                    verbatimTextOutput("selected_listSensors"),
                    collapsible = T)
                    
              )
              ,fluidRow(

                #helpText("Select stations and parameters you want to dowload by filtering the table below.",
                #         "To stop the download refresh the page"),
                #htmlOutput("tableInstructions"),
                DTOutput('table')
               
              ),
            
              htmlOutput("d1scla1mer"),
              textOutput("Disclaimer")
              # ,
              # tags$a(href='http://www.provinz.bz.it/',
              #        tags$img(style="vertical-align: bottom;width: 200px;padding-right: 20px;padding-top: 20px;float: right;",
              #                 src='http://www.provinz.bz.it/news/de/news.asp?news_action=300&news_image_id=924130'),
              #        href='http://www.eurac.edu/',
              #        tags$img(style="vertical-align: bottom;width: 200px;padding-right: 20px;padding-top: 20px;float: left;",
              #                 src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg')
              #        )
      
      )
      ,
      tabItem(# the about page
    tabName = "about",
    tabPanel("About", box(width = NULL,
                          #about$value
                         htmlOutput("about_out")
                         ))
    )#,
    #  tabItem(# the about page
    #tabName = "legend",
    #DTOutput('legend_tab')
    #)
)))
