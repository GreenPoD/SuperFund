library(shiny)
library(DT)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(ggridges)

ui <- bootstrapPage(theme = shinytheme(theme = "lumen"),
        tabsetPanel(
          # leaflet map user interface
            tabPanel("Map",
            leafletOutput('map', width = 1200, height = 1000),
            absolutePanel(top = 40, right = 10,
               titlePanel(h3("Superfund Sites")),
               sliderInput("range", "Select Timespan", min(state_contaminants$fiscal_year),
                  max(state_contaminants$fiscal_year),
                  value = range(state_contaminants$fiscal_year), step = 1),
               selectizeInput('media_type', label = NULL, 
                  choices = unique(state_contaminants$media),
                  multiple = TRUE,
                  options = list(placeholder = "Select Media Type")),
               selectizeInput('contaminant_type', label = NULL, 
                  choices = unique(state_contaminants$contaminant_name), 
                  multiple = TRUE,
                  options = list(placeholder = "Select Contaminant",
                                 onInitialize = I('function() { this.setValue(""); }'))))),
          tabPanel("Plot",
            plotOutput('plot', width = 1200, height = 1000)),
          tabPanel("Table",
            DTOutput('table')
        )
    )
)

server <- function(input, output, session) {
    
    mediaData <- reactive({
        state_contaminants %>%
            filter(media %in% input$media_type)
    }) 
    
    filteredData <- reactive({
        mediaData()[mediaData()$fiscal_year >= input$range[1] & 
                        mediaData()$fiscal_year <= input$range[2],]
    })
    
    subsetData <- reactive({
        mediaData()%>% 
            filter(contaminant_name %in% input$contaminant_type)
    })
    
    filteredSubsetData <- reactive({
        subsetData()[subsetData()$fiscal_year >= input$range[1] & 
                         subsetData()$fiscal_year <= input$range[2],]
    })
    
    # output leaflet map
    output$map <- renderLeaflet(
        leaflet() %>% 
            addProviderTiles(provider = "Stamen.TonerLite") %>% 
            setView(lng = -97.909299, lat = 36.395046, zoom = 4) %>% 
            addResetMapButton() 
    )
    
    observe({
        #2nd level proxy that renders the animal data  
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, 
                       radius = ~log_odds_weighted * 250, weight = 1,
                       color = "#000000", fillColor = ~colour,
                       fillOpacity = 0.3, popup = ~paste(contaminant_name, site_name, state_abr))
    })
    
    #leaflet map observer / proxy for filtered subset data
    observe({
        leafletProxy("map", data = filteredSubsetData()) %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, 
                       radius = ~log_odds_weighted * 250, weight = 1,
                       color = "#000000", fillColor = ~colour,
                       fillOpacity = 0.3, popup = ~paste(contaminant_name, site_name, state_abr))
    })
    

    output$plot <- renderPlot(
        state_contaminants %>% 
            filter(contaminant_name %in% filteredSubsetData()$contaminant_name) %>%
            ggplot(aes(x = log_odds_weighted, y = contaminant_name, fill = contaminant_name)) +
            geom_density_ridges(alpha = 0.9, scale = 5, show.legend = FALSE) +
            scale_fill_brewer(type = "seq", palette = 4, aesthetics = "fill") +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text.x = element_blank()) +
            labs(title = "Weighted Log Odds",
                 subtitle = "Ratio of contaminant distribution within a state versus all other states",
                 y = NULL,
                 x = NULL)
         
    )
    
    output$table <- renderDT(
        filteredSubsetData() %>% 
            select(state_name, contaminant_name, site_name, fiscal_year, media,
                   log_odds_weighted) %>% 
            arrange(fiscal_year)
        
        )
    
    observe({
        updateSelectizeInput(session, "contaminant_type", 
                             choices = unique(mediaData()$contaminant_name))
    })

}


shinyApp(ui = ui, server = server)
