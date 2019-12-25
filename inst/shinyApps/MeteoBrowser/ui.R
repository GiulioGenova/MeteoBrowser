enableBookmarking("url")

ui <- function(request) {

  dashboardPage(#useShinyjs(),
    title= "Meteo Browser Südtirol",
    skin = "blue",

    header
    ,
    dashboardSidebar(
      disable = F,
      sidebarMenuOutput("Data"),
      sidebarMenuOutput("about"),
      sidebarMenuOutput("Tutorial"),
      sidebarMenu(
        #uiOutput("Data"),
        #menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
        #uiOutput("about"),
        #menuItem("README", tabName = "about", icon = icon("info-circle")),
        radioButtons("language", NULL,
                     c("Deutsch" = "de",
                       "Italiano" = "it",
                       "English" = "en"))
        #menuItem("map", tabName = "map", icon = icon("info-circle"))#,
        #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
      )),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        includeCSS("www/node_modules/cookies-eu-banner/css/cookies-eu-banner.default.css"),

        tags$link(rel = "script", type = "js", href = "scr.js"),
        #includeScript("https://www.googletagmanager.com/gtag/js?id=UA-147978948-1"),
        #includeScript("google-analytics.js"),
        #includeScript("node_modules/cookies-eu-banner/dist/cookies-eu-banner.js"),

      ),
      tags$a(
        HTML(
          '<div id="cookies-eu-banner" style="display: none;">
            By continuing your visit to this site, you accept the use of cookies by Google Analytics to make visits statistics.
          <a href="./read-more.html" id="cookies-eu-more">Read more</a>
            <button id="cookies-eu-reject">Reject</button>
            <button id="cookies-eu-accept">Accept</button>
            </div>'
        )#,
        #includeScript("www/cookies-eu-banner.js"),

      ),

      tags$a(
        href="https://github.com/GiulioGenova/MeteoBrowser",target="_blank",
        tags$img(style="position: absolute; top: 0; right: 0; border: 0;z-index: 5;z-index: 5000;",
                 src="https://github.blog/wp-content/uploads/2008/12/forkme_right_green_007200.png?resize=135%2C135",
                 alt="Fork me on GitHub",
                 width="135",
                 height="135",
                 class="attachment-full size-full"
        )
      ),
      # Boxes need to be put in a row (or column)
      tabItems(

        tabItem(tabName = "Data",

                fluidRow(

                  box(width = 4,collapsible = T,

                      uiOutput("statlist"),
                      uiOutput("altitudelist"),
                      uiOutput("sensorlist"),
                      uiOutput("daterange"),
                      div(style="color: red;",
                          uiOutput("message")
                      ),

                      div(style="display: inline-block;vertical-align:top; width: 43%;",uiOutput("round")),
                      div(style="display: inline-block;vertical-align:top; width: 55%;",uiOutput("gather")),

                      textOutput("downloadInstructions"),
                      div(style=" width: 40%;",
                          uiOutput("refresh")
                      )

                      ,
                      div(style="color: red;",
                          uiOutput("messageStatSens")
                      ),
                      div(style=" width: 100%;",
                          div(style="display: inline-block;vertical-align:top; width: 40%;",
                              uiOutput("save")
                          ),
                          div(style="display: inline-block;vertical-align:top; width: 22%;",
                              uiOutput("tabChoice")
                          ),
                          div(style="display: inline-block;vertical-align:top; width: 28%;",
                              uiOutput("dst")
                          )
                      )

                      #,

                      #htmlOutput("tableInstructions")



                  ),

                  box(width = 8,
                      leafletOutput("map"),
                      #uiOutput("map"),
                      #conditionalPanel(condition ="output.spatialSelection",actionButton("deletebtn", "remove drawn")),
                      #uiOutput("deletebtn"),
                      verbatimTextOutput("selected"),
                      textOutput("youSelStations"),
                      verbatimTextOutput("selected_list"),
                      textOutput("andSensors"),
                      verbatimTextOutput("selected_listSensors"),
                      collapsible = T)

                ),

                # fluidRow(
                #
                #   #helpText("Select stations and parameters you want to dowload by filtering the table below.",
                #   #         "To stop the download refresh the page"),
                #   #htmlOutput("tableInstructions"),
                #   DTOutput('table')
                #
                # ),

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
        ),
        tabItem(# the tutorial
          tabName = "Tutorial",
          box(width = 12,height = "100%",
              htmlOutput("tutorial")
          )
        )
      ),
      #includeScript("node_modules/cookies-eu-banner/dist/cookies-eu-banner.min.js"),
      tags$script(src = "node_modules/cookies-eu-banner/dist/cookies-eu-banner.min.js"),
      includeScript("www/google-analytics-cookies.js")
      #includeHTML('<script src="node_modules/cookies-eu-banner/dist/cookies-eu-banner.min.js"></script>'),
    )

  )
}
