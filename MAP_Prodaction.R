library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
#library(venneuler) 
library(VennDiagram)
library(gplots)
library(ggplot2)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("limma", version = "3.8")



library('limma')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Page Rank", min(result$rnk), max(result$rnk),
                            value = range(result$rnk), step = 0.0001
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                verbatimTextOutput("out"),
                plotOutput("VennDiag"),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    result[result$rnk >= input$range[1] & result$rnk <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, result$rnk)
  })
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(result) %>% 
      addTiles() %>%
      setView(59.888294655100005,	30.4803147437, zoom=5) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      onRender(
        "function(el,x){
        this.on('mousedown', function(e) {
        var lat = e.latlng.lat;
        var lng = e.latlng.lng;
        var coord = [lat, lng];
        Shiny.onInputChange('hover_coordinates', coord)
        });
        this.on('mouseout', function(e) {
        Shiny.onInputChange('hover_coordinates', null)
        })
  }"
      )
    })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      #addCircles(radius = ~1000^rnk, weight = sum(result$snvolumeamtipbytesuplink, result$snvolumeamtipbytesdownlink), color = "#777777",
      addCircles(radius = ~1000^rnk, weight = 5, color = "#777777",      
                 fillColor = ~pal(rnk), fillOpacity = 0.7, popup = ~paste(rnk)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = result)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~rnk
      )
    }
  })
  output$out <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1], 
             "\nLng: ", input$hover_coordinates[2])
    }
  })
  #output$VennDiag <- renderPlot({venneuler(dfVenn[-1])})
  output$VennDiag <- renderPlot({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      df <- result[c(7,13,14)]
      colnames(df) <- c("rnk", "lat", "long")
      rownames(df) <-NULL
      df$rnk <-abs(log(df$rnk))/max(abs(log(df$rnk)))
      df$rnk <- ifelse(df$rnk == 0.0, 0.999, df$rnk)
      #df_between_lat <-df[between(df$lat, min(df$lat), max(df$lat)),]
      df_between_lat <-df[between(df$lat, (input$hover_coordinates[1])*0.9, (input$hover_coordinates[1])*1.1),]
      #df_between_location <-df_between_lat[between(df_between_lat$long, min(df_between_lat$long), max(df_between_lat$long)),]
      df_between_location <-df_between_lat[between(df_between_lat$long, (input$hover_coordinates[2])*0.9, (input$hover_coordinates[2])*1.1),]
      df_between_location <- df_between_location[-c(2,3)]
      df_more <-df_between_location$rnk > 0.6
      df_midle <- df_between_location$rnk <= 0.6 & df_between_location$rnk > 0.5
      df_Total <- df_between_location$rnk <=1
      #c3 <- cbind(df_Total, df_more, df_midle)
      df_venn <- data.frame(Total=df_Total, Buy=df_more, CanBuy=df_midle)
      #plot(venneuler(df_venn[]), alpha=0.1)
      #length(df_Total)
      #venn.diagram(df_venn, filename = "Venn_2set_complex.tiff")
      
      venn.plot <- venn.diagram( list(
        A = sum(as.integer(df_venn$Total)), 
        B = sum(as.integer(df_venn$Buy)),
        C = sum(as.integer(df_venn$CanBuy))),
        NULL, fill=c("darkmagenta", "darkblue", "grey"), alpha=c(0.5,0.5, 0.5), 
        cex = 2, cat.fontface=4, 
        category.names=c("A", "B", "C"), 
        main="Random Gene Lists")
      
      #grid.draw(venn.plot)
      
      #venn.diagram(venn)
      
      vennDiagram(vennCounts(df_venn), include="both", counts.col=c("red", "blue"), circle.col = c("red", "blue", "green3"))
      
    }
  })
  }

shinyApp(ui, server)









