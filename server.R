
library(shiny)

# Read the data
stormData <- read.csv("stormdata.txt", stringsAsFactors = FALSE)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # Define a reactive behaviour on the input fields for the date
  selectedData <- reactive({
    stormData %>% filter(DATE > as.character(input$yearsrange[1]) & DATE < as.character(input$yearsrange[2]))
  })
  
  # Plot a histogram of the dates to show for which data is available
  output$histYears <- renderPlot({
      hist(year(stormData$DATE), xlab = "Years", main = "Histogram of Years")
  })
  
  # render a data table so user can navigate on the date
  output$table <- renderDataTable(selectedData())
    
  # render a segmented plot to show the numbers of harm produced by each event 
  # in the selected dates interval
  output$plotHarm <- renderPlot({

    aggregatedResultsHarm <- selectedData() %>%
        group_by(EVTYPE) %>%
        summarise( Fatality = sum(FATALITIES), Injury = sum(INJURIES), total = Fatality + Injury)

    resultsHarm <- gather(aggregatedResultsHarm, Fatality, Injury, key = Harm, value = cases)

    resultsHarmGraph <- ggplot(resultsHarm, aes(x = reorder(EVTYPE, -cases), y = cases, fill = Harm)) + geom_bar(stat = "identity")

    resultsHarmGraph <- resultsHarmGraph + labs(x = "Event type", y = "Number of people affected", title = "Impact analysis of severe weather on public health")

    resultsHarmGraph <- resultsHarmGraph + scale_fill_manual(values = c("skyblue", "royalblue")) + theme(axis.text.x=element_text(size=6.6))

    resultsHarmGraph <- resultsHarmGraph + coord_flip()

    resultsHarmGraph
  })

  # render a segmented plot to show the values of damage by each event
  # in the selected dates interval
  output$plotDamage <- renderPlot({

    aggregatedResultsDamage <- selectedData() %>%
        group_by(EVTYPE) %>%
        summarise( Property = sum(PROPDMGSTDUNIT), Crop = sum(CROPDMGSTDUNIT), total = Property + Crop)

    resultsDamage <- gather(aggregatedResultsDamage, Property, Crop, key = Damage, value = cases)

    resultsDamageGraph <- ggplot(resultsDamage, aes(x = reorder(EVTYPE, -cases), y = cases, fill = Damage)) + geom_bar(stat = "identity")

    resultsDamageGraph <- resultsDamageGraph + labs(x = "Event type", y = "Value of the damage (in Billions)", title = "Impact analysis of severe weather on property")

    resultsDamageGraph <- resultsDamageGraph + scale_fill_manual(values = c("skyblue", "royalblue"))

    resultsDamageGraph <- resultsDamageGraph + coord_flip()

    resultsDamageGraph
  })

  # render a choropleth map to show the numbers of harm produced by each event
  # in the selected dates interval in a map
  output$mapHarm <- renderPlotly({

      summaryHarm <- selectedData() %>%
          group_by(STATE) %>%
          summarise( Fatality = sum(FATALITIES), Injury = sum(INJURIES), total = Fatality + Injury) %>%
          mutate(hover = paste("Fatality: ", Fatality, '<br>', "Injury: ", Injury))

      g <- list(
          scope = 'usa',
          projection = list(type = 'albers usa')
          )

      plot_geo(summaryHarm, locationmode = 'USA-states') %>%
          add_trace(
              z = ~total,
              locations = ~STATE,
              text = ~hover,
              color = ~total, colors = 'Blues'
          ) %>%
          colorbar(title = "Total Number") %>%
          layout(
              title = 'Impact analysis of severe weather on public health per state',
              geo = g
          ) %>%
          config(displayModeBar = F)

        })

  # render a choropleth map to show the values of damage by each event
  # in the selected dates interval in a map
  output$mapDamage <- renderPlotly({

      summaryDamage <- selectedData() %>%
          group_by(STATE) %>%
          summarise( Property = sum(PROPDMGSTDUNIT), Crop = sum(CROPDMGSTDUNIT), total = Property + Crop) %>%
          mutate(hover = paste("Property: ", round(Property, digits = 0), '<br>', "Crop: ", round(Crop, digits = 0)))

      g <- list(
          scope = 'usa',
          projection = list(type = 'albers usa')
      )

      plot_geo(summaryDamage, locationmode = 'USA-states') %>%
          add_trace(
              z = ~round(total, digits = 0),
              locations = ~STATE,
              text = ~hover,
              color = ~total, colors = 'Blues'
          ) %>%
          colorbar(title = "Billions USD") %>%
          layout(
              title = 'Impact analysis of severe weather on property',
              geo = g
          ) %>%
        config(displayModeBar = F)

  })
  
})
