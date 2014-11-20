require(shiny)
require(leaflet)
require(ggplot2)


shinyServer(function(input, output, session) {
  values <- reactiveValues(highlight = c())
  map <- createLeafletMap(session, "map")
  
  drawMap <- function(df, highlight = FALSE) {
    map$addPolygon(lat = df$Latitude, 
                   lng = df$Longitude,
                   layerId = country_unique$Country,
                   lapply(as.list(country_unique$Color), function(x){list(fillColor=x)}),
                   list(fill=TRUE, stroke = TRUE, opacity = 1, color="white", fillOpacity=ifelse(highlight, 0.5, 0.2),
                        weight=ifelse(highlight, 3, 1))
    )
  }
  session$onFlushed(once=TRUE, function() {
    drawMap(country_df)
  })
  
  observe({
    values$hover <- input$map_shape_mouseover$id
  })
  observe({
    values$click <- input$map_shape_click$id
  })
  
  click_select <- reactive({
    country_unique[country_unique$Country == values$click,]
  })
  
  scope_select <- reactive({
    country <- click_select()
    if(length(input$scope)==0){return(country)}
    if(input$scope == "Regional")
      country <- country_unique[country_unique$Region == country$Region,]
    return(country)
  })
  
  tag_select <- reactive({
    country <- scope_select()
    if(is.null(input$tags)){return(country)}
    if(nchar(input$tags) > 0)
    country$Tag <- paste(country$Tag, input$tags, sep=",")
    return(country)
  })
  
  
  
  output$basic_ui <- renderUI({
    if(is.null(values$hover)) {
      return(tags$div("Hover over a country"))
    }
    reportString <- function(selected) {
      country <- country_unique[country_unique$Country == selected,]
      return(tags$div(tags$strong(selected), 
                      tags$strong("[",country$ADM0_A3,"]"),
                      if(is.na(country$NAME_FORMA)) {NULL} else{
                        tags$div("(",country$NAME_FORMA,")")},
                      tags$div(tags$strong("Region:",span(country$Region.proper,
                                                          style=paste("color",country$Color,sep=":"))))))
    }
    #Real shit code below. Must go back and shore it up.
    if(is.null(values$click)) {
      menu <- tags$div(reportString(values$hover), br(), br(),
                       tags$div("Click a country to begin scraping protocols"))
    } else {
      menu <- tags$div(reportString(values$click))  
    }
    return(menu)
  })
  
  output$title <- renderText({
    input$submit
    isolate({
      country <- tag_select()
      if(nrow(country) > 1) {
        return(unique(country$Region.proper[!is.na(country$Region)]))
      }
      return(unlist(strsplit(country$Tag,","))[1])
    })
  })
  
  output$message <- renderText({
    input$submit
    isolate({
      country <- click_select()
      if(nrow(country) == 0) {return()}
      if(is.na(country$Tag)) {return("No stories are available for this country")}
      if(length(prepare_news()) == 0) {return("There are no current stories regarding this country")}
    })
  })
  
  prepare_news <- reactive({
    input$submit
    isolate({
      country <- tag_select()
      if(length(country$Tag)==0) { return(NULL)}
      country_tags <- gsub(",", "|", country$Tag, fixed =TRUE)
      if(length(country_tags) > 1) {
        country_tags <- paste0(country_tags[!is.na(country_tags)],collapse = "|")
      }
      address <- "http://www.bbc.com/news/world/"
      address2 <- paste(address, unique(country$Region[!is.na(country$Region)]), "/", sep="")

      home_page <- scrapeBBC(address)
      regional_page <- scrapeBBC(address2)
      stories <- rbind(home_page, regional_page)
      stories <- stories[grepl(country_tags, stories$title) | grepl(country_tags, stories$text),]
      if(nrow(country) > 1) {
        stories <- rbind(stories, regional_page)
      }
      stories <- stories[!(duplicated(stories$href) | stories$title == values$click),]
      if(!nrow(stories)) {return(NULL)}
      Articles <- paste("<a href=", stories$href,">", stories$title, "<//a>")
      return(data.frame(Articles))
    })
  })
  
  output$news_stories <- renderDataTable({prepare_news()},
                                         options = list(pageLength=10, searcheable=FALSE))
  
  output$country_stats <- renderPlot({
    df <- country_unique[!is.na(country_unique$Tag),]
    df$Story.count <- 0 #Initialize vector
    country_tags <- gsub(",", "|", df$Tag, fixed =TRUE)
    main_address <- "http://www.bbc.com/news/world/"
    full_address <- c(main_address, paste(address, unique(df$Region), "/", sep=""))
    test <- ldply(full_address[-8], scrapeBBC)

    #Really shitty code
    for(i in seq(country_tags)) {
      indicator <- grep(country_tags[i], test$title)
      df$Story.count[i] <- df$Story.count[i] + length(indicator)
      indicator2 <- grep(country_tags[i], test$text)
      df$Story.count[i] <- df$Story.count[i] + length(indicator2) - sum(indicator2 %in% indicator)
    }
    #Dumbass mistake on United States virgin islands and Barbados. Gotta go back and fix my pluralPosession function
    plot_data <- df[df$Story.count  > 3 & !(df$Country %in% c("Barbados","United States Virgin Islands")),]
    #Shorten country name if name is more than 2 words long
    shorten <- sapply(plot_data$Country, function(x){length(unlist(strsplit(as.character(x), " ")))>2})
    #Or shorten name if name is more than 10 characters long
    #shorten <- nchar(as.character(plot_data$Country)) > 10
    plot_data$Country[shorten] <- plot_data$ADM0_A3[shorten]
    plot_data$Country <- reorder(plot_data$Country, plot_data$Story.count)

    ggplot(plot_data, aes(Country, Story.count, fill=Region.proper)) + geom_bar(stat="identity") + 
      theme(axis.text.x = element_text(angle=45),
            legend.position = "top", legend.direction = "vertical", legend.title.align = 0.5) +
    guides(fill=guide_legend("Region",ncol=3)) + ylab("Stories reported by BBC")
    })
})