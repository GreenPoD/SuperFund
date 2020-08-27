#Required Libraries

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)


#Shiny bootstrap page user interface
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("lmap", width = "100%", height = "100%"),
    #fixed panel with selections (range, class, and full name)
    absolutePanel(top = 10, right = 10,
                  titlePanel(h3("Superfund Sites")),
                  sliderInput("range", "Select Timespan", min(contaminants_map$fiscal_year),
                              max(contaminants_map$fiscal_year),
                              value = range(contaminants_map$fiscal_year), step = 0.1),
                  
                  selectizeInput('contaminant_type', label = NULL, 
                                 choices = unique(contaminants_map$contaminant_name), 
                                 multiple = TRUE,
                                 options = list(placeholder = "Select Contaminant",
                                                onInitialize = I('function() { this.setValue(""); }')))
    )
)

#Server function call
server <- function(input, output, session) {
    
    classData <- reactive({
        contaminants_map %>%
            filter(contaminant_name %in% input$contaminant_type)
    })
    
    filteredData <- reactive({
        classData()[classData()$fiscal_year >= input$range[1] & 
                        classData()$fiscal_year <= input$range[2],]
    })
    
subsetData <- reactive({
        classData()%>% 
            filter(complaint %in% input$complaint_type)
    })

    
    #renders the static leaflet map ~ nothing that changes
    output$lmap <- renderLeaflet(
        leaflet(contaminants_map) %>% 
            addProviderTiles(provider = "Esri.WorldGrayCanvas") %>%
            #fitBounds(~min(lon)+0.5, ~min(lat) +0.5, ~max(lon), ~max(lat)) %>%
            setView(lng = 137.4219949, lat = -25.8570246, zoom = 5) %>% 
            addResetMapButton()
    )
    
    observe({
        #2nd level proxy that renders the animal data  
        leafletProxy("lmap", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~lon, lat = ~lat, 
                       radius = ~animal_count * 50, weight = 1,
                       color = ~colour, fillColor = ~complaint_colour,
                       fillOpacity = 0.3, popup = ~paste(suburb, animal_count, complaint, "Complaints"))
    })
    
    #expression filters the choices available in the selectInput('name')
    observe({
        updateSelectizeInput(session, "complaint_type", choices = unique(classData()$complaint))
    })
    
    #leaflet map observer / proxy for filtered subset data
    observe({
        leafletProxy("lmap", data = filteredSubsetData()) %>%
            clearShapes() %>%
            addCircles(lng = ~lon, lat = ~lat, radius = ~animal_count * 50, weight = 1,
                       color = ~colour, fillColor = ~complaint_colour,
                       fillOpacity = 0.3, popup = ~paste(suburb, animal_count, complaint, "Complaints"))
    })
    
}

shinyApp(ui = ui, server = server)