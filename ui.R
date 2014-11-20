require(shiny)
require(leaflet)

shinyUI(
  navbarPage(img(src="http://news.bbcimg.co.uk/img/1_0_3/cream/hi/news/news-blocks.gif", width=84, height=24), # Delete row to get rid of navbar
             tabPanel("Map", #Delete row to get rid of navbar
  fluidPage(
  leafletMap(outputId="map",width = "100%",height=500,
             options=list(center=c(0,0),
                          zoom=2)),
  
  absolutePanel(
    right = 60, top = 80, width = 200, class = "floater",
    #right = 30, top = 10, width = 200, class = "floater", without NavBar
    uiOutput("basic_ui"),
    conditionalPanel("input.map_shape_click != null",
                     selectInput("scope", h6("Scrape scope:"),
                                 choices = c("National", "Regional"), "National"),
                     textInput("tags", h6("More search criteria/keywords: \n (seperate keywords by , )"), ""),
                     actionButton("submit", "Retrieve Stories"))
  ),
  tags$head(tags$style("
                       #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
                       .floater { background-color: white; padding: 8px; opacity: 0.8; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
                       ")),
  
  #fluidRow(invisible(actionButton("submit","Retrieve Stories"))),
  conditionalPanel("input.map_shape_click != null",
                   fluidRow(column(9,h4(div(textOutput("title",inline = TRUE), "stories retrieved on", span(date(), style="color:blue")))),
                            column(3, div(img(src="http://news.bbcimg.co.uk/img/1_0_3/cream/hi/news/news-blocks.gif")),
                                   div(p("All articles and content are the property of BBC © 2014"))))),
  fluidRow(h4(span(textOutput("message",inline = TRUE), style="color:red"))),
  fluidRow(column(9, dataTableOutput("news_stories"))),
  fluidRow(textOutput("junk"))
)),
tabPanel("Country Statistics",
         plotOutput("country_stats"))))