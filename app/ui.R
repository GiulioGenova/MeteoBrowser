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

ui <- dashboardPage(#useShinyjs(),
  skin = "blue",
  dashboardHeader(title = "Province Open Data stations"),
  dashboardSidebar(disable = T,
                   sidebarMenu(
                     
                     menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
                     menuItem("about", tabName = "about", icon = icon("info-circle"))#,
                     #menuItem("map", tabName = "map", icon = icon("info-circle"))#,
                     #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
                   )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      
      tabItem(tabName = "Data",
              
              fluidRow(
                
                box(width = 4,collapsible = T,dateRangeInput(label = h4("Pick a date range"),inputId = "daterange",separator = " - ",min = "2000-01-01",
                                                             start = Sys.Date()-3,
                                                             end = Sys.Date()+1),
                    conditionalPanel(condition = "output.rightdate",br(),actionButton(label= "Download selected data","refresh")) ,
                    #conditionalPanel(condition = "output.rightdate",br(),actionButton( "stop",label = "Stop Download (reload page)",class="btn-danger")),#,onclick="Shiny.onInputChange('stopThis',true)"
                    conditionalPanel(condition = "output.tablebuilt",br(),#"input.daterange[1]<=input.daterange[2]"
                                     downloadLink('downloadData', h4('Save as csv') ) 
                    ),
                    
                    verbatimTextOutput("message"),
                    verbatimTextOutput("selected"),
                    selectInput("round",label = h4("Time aggregation"),
                                choices = list("raw","hour","day","week","month","year")),
                    
                    #actionButton(label= "update selection","refresh"),
                    selectInput("gather",
                                label = h4("Choose between long or wide table to download"),
                                choices = list("wide","long"))),
                
                box(width = 8,leafletOutput("map"),collapsible = T)
              )
              ,fluidRow(

                helpText("Select stations and parameters you want to dowload by filtering the table below.",
                         "To stop the download refresh the page"),
                DTOutput('table')
               
              )
      
      )
      ,
      tabItem(# the about page
    tabName = "about",
    tabPanel("About", box(width = NULL,about$value))
    )
  ))
