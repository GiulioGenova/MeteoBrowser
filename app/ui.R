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
about = source(file.path(getwd(),'about.R'))
translation<-read.csv(file.path(getwd(),"translation.csv"),header = T,sep = ",",stringsAsFactors = F)
scr <- tags$script(HTML(
  "
  Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
  console.log('deleting',x)
  // get leaflet map
  var map = HTMLWidgets.find('#' + x.elid).getMap();
  // remove
  map.removeLayer(map._layers[x.layerid])
  })
  "
))

# then our new app can do something like this




ui <- dashboardPage(#useShinyjs(),
  
  skin = "blue",
  dashboardHeader(title = "Open Data South Tyrol",titleWidth = 320),
  dashboardSidebar(disable = F,
                   sidebarMenu(
                     
                     menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
                     menuItem("README", tabName = "about", icon = icon("info-circle")),
                     radioButtons(inputId = "language", label = "",
                     choices = c("English" = "en", "Deutsch" = "de","Italiano"="it"),
                     selected = "en")
                     #menuItem("map", tabName = "map", icon = icon("info-circle"))#,
                     #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
                   )),
  dashboardBody(
    tagList(
  scr
  ),
    
    # Boxes need to be put in a row (or column)
    tabItems(
      
      tabItem(tabName = "Data",
              
              fluidRow(
                
                box(width = 4,collapsible = T,
                    
                    uiOutput("daterange"),
                    
                    #dateRangeInput(label = h4("Pick a date range"),inputId = "daterange",separator = " - ",min = "2000-01-01",
                    #                                         start = Sys.Date()-3,
                    #                                         end = Sys.Date()+1)
                    #conditionalPanel(condition = "output.rightdate",br(),actionButton(label= "Update selection","updateSelection")),
                    
                    uiOutput("refresh"),
                    #conditionalPanel(condition = "output.rightdate",br(),actionButton(label= "Download selected data","refresh")) ,
                    #conditionalPanel(condition = "output.rightdate",br(),actionButton( "stop",label = "Stop Download (reload page)",class="btn-danger")),#,onclick="Shiny.onInputChange('stopThis',true)"
                    conditionalPanel(condition = "output.tablebuilt",br(),#"input.daterange[1]<=input.daterange[2]"
                                     downloadButton('downloadData', h4('Save as csv'), class="btn-danger" ) 
                    ),
                    #helpText("First click \"download selected data\" then \"save as csv\""),
                    textOutput("downloadInstructions"),
                    verbatimTextOutput("message"),
                    verbatimTextOutput("selected"),
                    
                    #selectInput("round",label = h4("Time aggregation"),
                    #            choices = list("raw","hour","day","week","month","year")),
                    
                    uiOutput("round"),
                    
                    #actionButton(label= "update selection","refresh"),
                    
                    #selectInput("gather",
                    #            label = h4("Choose between long or wide table format"),
                    #            choices = list("wide","long")),
                    
                    uiOutput("gather"),
                    verbatimTextOutput("spatSel"),
                    checkboxInput("spatialSelection", label = "",
                                value = FALSE)
                    
                    #uiOutput("spatSel")
                    #selectInput("spatialSelection",
                    #            label = h4("Enable spatial selection"),
                  # choices = list("NO","YES"))
              ),
                   
                box(width = 8,leafletOutput("map"),
                    conditionalPanel(condition ="output.spatialSelection",actionButton("deletebtn", "remove drawn")),
                    textOutput("youSelStations"),
                    verbatimTextOutput("selected_list"),
                    textOutput("andSensors"),
                    verbatimTextOutput("selected_listSensors"),
                    collapsible = T)
                    
              )
              ,fluidRow(

                #helpText("Select stations and parameters you want to dowload by filtering the table below.",
                #         "To stop the download refresh the page"),
                textOutput("tableInstructions"),
                DTOutput('table')
               
              )
      
      )
      ,
      tabItem(# the about page
    tabName = "about",
    tabPanel("About", box(width = NULL,about$value))
    )
)))
