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
#translation<-read.csv(file.path(getwd(),"translation.csv"),header = T,sep = ",",stringsAsFactors = F)
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

header <- dashboardHeader(titleWidth = 480)
anchor <- tags$a(tags$style(HTML(".tit { color: black; padding-right: 5px;font-weight: bold;font-family: Arial;}")),#;font-size: 1vw;;font-size: 1.05em
                 tags$style(HTML(".tit:hover { color: #cd4e37;}")),
                 href='http://www.eurac.edu/',
                 tags$img(src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'),#, height='60', width='50'
                 'Meteo Browser South Tyrol',class='tit')

stile<-tags$li(class = "dropdown",
           tags$style( ".skin-blue .main-header .logo {
                        background-color: White; padding-left: 10px; padding-top: 10px; padding-bottom: 10px;
                                            }"),
           tags$style( ".skin-blue .main-header .navbar {
                              background-color: #dae0e0; 
                              }  "),
           tags$style( ".skin-blue .main-header .logo:hover {
                        background-color:  	#dae0e0;
                                            }"),
           #tags$style( ".skin-blue .main-header {
           #             background-color: #cd4e37;
           #                                 }"),
            #tags$style(".main-header {max-height: 75px}"),
            tags$style(".main-header .logo {height: 75px ; padding-top: 10px}"),
            tags$style(".skin-blue .main-header .navbar .sidebar-toggle:hover{
                        background-color:  	#cd4e37;
                                            }")
    )

header$children[[2]]$children <- tags$div(
    tags$head(tags$style(HTML(".name { color: black }"))),
    anchor,stile,
    class = 'name')



ui <- dashboardPage(#useShinyjs(),
  title= "Meteo Browser",
    
  skin = "blue",
  #dashboardHeader(tags$li(class = "dropdown",
  #         tags$style( ".skin-blue .main-header .logo {
  #                      background-color: #cd4e37;
  #                                          }"),
  #         tags$style( ".skin-blue .main-header .navbar {
  #                            background-color: #dae0e0;
  #                            }  "),
  #         tags$style( ".skin-blue .main-header .logo:hover {
  #                      background-color:  	#dae0e0;
  #                                          }"),
  #         #tags$style( ".skin-blue .main-header {
  #         #             background-color: #cd4e37;
  #         #                                 }"),
  #          tags$style(".main-header {max-height: 100px}"),
  #          tags$style(".main-header .logo {height: 100px ; padding-top: 10px}")
  #  ),
     
    #title =loadingLogo('http://www.eurac.edu/Style%20Library/','logoEURAC.jpg','buffpowa.gif')
    #title = tags$a(href='http://www.eurac.edu/',
    #               #tags$img(src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'))#,
    #               tags$img(src='logoMeteoBrowser.jpg'),'Meteo Browser South Tyrol')
    ##  titleWidth = 320
     #            ),
                  
    #title =  tags$a(href='http://www.eurac.edu/',
     #  #tags$img(src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'))#,
     #  tags$img(src='logoMeteoBrowser.jpg'))
    #  titleWidth = 320
 #                 ),
header             
    ,
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 110px}"),
    disable = F,
                   
    sidebarMenuOutput("Data"),
    sidebarMenuOutput("about"),
    sidebarMenu(
                     #uiOutput("Data"),
                     #menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
                     #uiOutput("about"),
                     #menuItem("README", tabName = "about", icon = icon("info-circle")),
                     radioButtons("language", "Language",
                                  c("English" = "en",
                                    "Deutsch" = "de",
                                    "Italiano" = "it"))
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
                    #verbatimTextOutput("message"),
                    uiOutput("message"),
                    textOutput("downloadInstructions"),
                    
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
                    
                    #uiOutput("spatialSelection")
                    #selectInput("spatialSelection",
                    #            label = h4("Enable spatial selection"),
                  # choices = list("NO","YES"))
              ),
                   
                box(width = 8,
                    leafletOutput("map"),
                    #uiOutput("map"),
                    #conditionalPanel(condition ="output.spatialSelection",actionButton("deletebtn", "remove drawn")),
                    uiOutput("deletebtn"),
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
    tabPanel("About", box(width = NULL,
                          #about$value
                         uiOutput("about_out")
                         ))
    )
)))
