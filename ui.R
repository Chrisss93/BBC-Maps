require(shiny)
require(leaflet)

shinyUI(
  navbarPage(img(src="http://news.bbcimg.co.uk/img/1_0_3/cream/hi/news/news-blocks.gif", #Delete to hide navbar
                 width=84, height=24), # Delete to hide navbar
             tabPanel("Interactive Map", #Delete to hide navbar
  fluidPage(
  leafletMap(outputId="map",width = "100%",height=500,
             options=list(center=c(0,0),
                          zoom=2,
                          scrollWheelZoom=TRUE)),
  absolutePanel(
    right = 40, top = 60, width = 200, class = "floater", fixed = TRUE, draggable = TRUE,
    #right = 30, top = 10, width = 200, class = "floater", without NavBar
    uiOutput("basic_ui"),
    conditionalPanel("input.map_shape_click != null",
                     uiOutput("scrape_ui"),
                     selectInput("scope", h6("Scrape scope:"),
                                 choices = c("National", "Regional"), "National"),
                     textInput("tags", h6("More search criteria/keywords: \n (seperate keywords by , )")),
                     actionButton("submit", "Retrieve Stories"),
                     div(br(), img(src="http://news.bbcimg.co.uk/img/1_0_3/cream/hi/news/news-blocks.gif")),
                     div(p("All articles and content are the property of BBC © 2014")))
  ),
  tags$head(tags$style("#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
                       .floater { background-color: white; padding: 8px; opacity: 0.8;
                       border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }")),
  
  #fluidRow(invisible(actionButton("submit","Retrieve Stories"))),
  conditionalPanel("input.map_shape_click != null",
                   fluidRow(column(9,h4(div(textOutput("title",inline = TRUE),
                                            "stories retrieved on", span(date(), style="color:blue")))))),
  fluidRow(h4(span(textOutput("message",inline = TRUE)))),
  fluidRow(column(9, dataTableOutput("news_stories"))),
  fluidRow(textOutput("junk"))
)),
tabPanel("Country Statistics",
         h4("As of", span(date(), style="color:blue")),
         plotOutput("country_stats"))))