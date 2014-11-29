require(shiny)
require(leaflet)
require(ggplot2)


shinyServer(function(input, output, session) {
  values <- reactiveValues()
  map <- createLeafletMap(session, "map")
  
  drawMap <- function(df, highlight = FALSE, select = FALSE) {
    country_data <- df[!(duplicated(df$Country)|is.na(df$Country)),]
    map$addPolygon(lat = I(df$Latitude), 
                   lng = I(df$Longitude),
                   layerId = I(country_data$Country),
                   I(lapply(country_data$Country, function(x){
                     list(fillColor = unique(country_data$Color[country_data$Country == x]))
                   })),
                   I(list(fill=TRUE, stroke = TRUE, opacity = 1,
                          color = ifelse(select, "white", "black"),
                          fillOpacity = ifelse(highlight, 0.5, 0.2),
                          weight = ifelse(highlight, 2, 0.5)))
    )
  }
  session$onFlushed(once=TRUE, function() {
    drawMap(country_df)
  })
  
  observe({
    values$hover <- input$map_shape_mouseover$id
  })
  lastHover <- c()
  observe({
    if(length(lastHover) > 0) {
      drawMap(country_df[country_df$Country %in% lastHover,])
    }
    lastHover <<- values$hover
    if(!is.null(values$hover)) {
      isolate({drawMap(country_df[country_df$Country %in% values$hover,], TRUE)})
    }
  })
  lastClick <- c()
  observe({
    if(!is.null(values$click)) {
      isolate({drawMap(country_df[country_df$Country %in% values$click,], TRUE, TRUE)})
    }
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
      return(div(br(),tags$div("Hover over a country"),br()))
    }
    if(is.null(values$click)) {
      message <- tags$div("Click a country to begin scraping protocols")
    } else { message <- tags$div(hr(style="border-color: black;"), h4("Scraping protocols"))}
    tags$div(reportString(values$hover), message)
  })
  
  output$scrape_ui <- renderUI({
    #Real shit code below. Must go back and shore it up.
    if(is.null(values$click)) {
      return(NULL)
    }
    title <- reportString(values$click)
    title[[3]][[3]] <- NULL
    tags$div(title) 
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
  
  all_news <- reactive({
    df <- country_unique[!is.na(country_unique$Tag),]
    main_address <- "http://www.bbc.com/news/world/"
    full_address <- c(main_address, paste(main_address, unique(df$Region), "/", sep=""))
    test <- ldply(full_address[-8], scrapeBBC)
    return(test)
  })
    
  output$country_stats <- renderPlot({
    all_news <- all_news()
    df <- country_unique[!is.na(country_unique$Tag),]
    country_tags <- gsub(",", "|", df$Tag, fixed =TRUE)
    #Really shitty code
    df$Story.count <- 0 #Initialize vector
    for(i in seq(country_tags)) {
      indicator <- grep(country_tags[i], all_news$title)
      df$Story.count[i] <- df$Story.count[i] + length(indicator)
      indicator2 <- grep(country_tags[i], all_news$text)
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

    ggplot(plot_data, aes(Country, Story.count, fill=Region.proper)) + geom_bar(stat="identity", alpha=0.7) + 
      theme(axis.text.x = element_text(angle=45,hjust=1),
            legend.position = "top", legend.direction = "vertical", legend.title.align = 0.5) +
    guides(fill=guide_legend("Region",ncol=3)) + ylab("Stories reported by BBC")
    })
})