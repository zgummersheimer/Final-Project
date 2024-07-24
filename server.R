library(shiny)
library(tidyverse)
library(readxl)

df.ap.region <- read_excel("ap-exam-volume-by-region-2012-2022.xls", 
                           na = "NA")
df.ap.region <- df.ap.region %>%
  filter(Region != "NA", `2012` != "NA") %>%
  filter(Region != "Dist. of Columbia",
         Region != "National Total",
         Region != "Canada",
         Region != "Non-U.S.",
         Region != "U.S. Territories",
         Region != "Grand Total") %>%
  mutate(Region = tolower(Region))

map <- map_data("state")



shinyServer(
  function(input, output) {
    
    in_start <- reactive(input$startdate)
    in_end <- reactive(input$enddate)
    
    output$error <- renderText(
    if (as.numeric(in_start()) > 2022 |
        as.numeric(in_start()) < 2012 |
        as.numeric(in_end()) > 2022 |
        as.numeric(in_end()) < 2012) {
      paste("Choose a year between 2012 and 2022") 
    } else {
      paste("No Errors")
    })
    newtbl.1 <- reactive(df.ap.region %>%
            dplyr::select(Region, in_start(), in_end()) %>%
            rename(Start = in_start(), End = in_end()) %>%
            mutate(count = End - Start) %>%
            arrange(Region))
    
    newtbl.2 <- reactive(df.ap.region %>%
            dplyr::select(Region, in_start(), in_end()) %>%
            rename(Start = in_start(), End = in_end()) %>%
            mutate(percent = (End / Start) -1) %>%
            arrange(Region))
    
    output$table1 <- renderTable(newtbl.1())
    output$table2 <- renderTable(newtbl.2())
    
    output$cmap <- renderPlot({
      newtbl.1() %>%
        ggplot(aes(fill = count)) +
        geom_map(aes(map_id = Region), map = map) +
        expand_limits(x = map$long, y = map$lat) +
        scale_fill_viridis_c(option = "viridis") +
        labs(fill = "Change in\nAP Tests Given",
             caption = "Data from AP Central\nhttps://apcentral.collegeboard.org/about-ap/ap-data-research") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, width = 800, height = 500)
    
    output$pmap <- renderPlot({
      newtbl.2() %>%
        ggplot(aes(fill = percent)) +
        geom_map(aes(map_id = Region), map = map) +
        expand_limits(x = map$long, y = map$lat) +
        scale_fill_viridis_c(option = "plasma") +
        labs(fill = "Percent Change in\nAP Tests Given",
             caption = "Data from AP Central\nhttps://apcentral.collegeboard.org/about-ap/ap-data-research") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, width = 800, height = 500)
    
    output$mymap <- renderUI({
      switch(input$maptype, 
             "Count" = plotOutput("cmap"),
             "Percent" = plotOutput("pmap")
      )
    })
    
    output$table <- renderUI({
      switch(input$maptype,
             "Count" = tableOutput("table1"),
             "Percent" = tableOutput("table2")
      )
    })
  }
)
