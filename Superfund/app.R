library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(ggplot2)
library(ggridges)
library(highcharter)

ui <- bootstrapPage(theme = shinytheme(theme = "lumen"),
        tabsetPanel(
          # leaflet map user interface
          tabPanel("Map",
            leafletOutput('map', width = 1200, height = 1200),
            absolutePanel(top = 0, right = 10,
              draggable = TRUE,
              titlePanel(h3("EPA Superfund Properties")),
              sliderInput("range", "Fiscal Year Property Added to NPL", 
                          min(state_contaminants$fiscal_year),
                          max(state_contaminants$fiscal_year),
                          value = range(state_contaminants$fiscal_year), step = 1),
              selectizeInput('media_type', label = NULL,  
                          choices = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Select Media Type",
                          onInitialize = I('function() { this.setValue(""); }'))),
              selectizeInput('contaminant_type', label = NULL, 
                          choices = unique(state_contaminants$contaminant_name), 
                          multiple = TRUE,
                          options = list(placeholder = "Select Contaminant",
                          onInitialize = I('function() { this.setValue(""); }')))),
              absolutePanel(bottom = 40, left = 10,
                          draggable = TRUE,
                          tags$text("Website"),
                          tags$a("EPA Superfund Search", href = "https://www.epa.gov/superfund/search-superfund-sites-where-you-live"),
                          class = 'card'),
              absolutePanel(top = 200, left = 20, 
                          width = 300, height = 600,
                          draggable = TRUE,
                          highchartOutput('log_odds'))),
          tabPanel("Density Plot",
            plotOutput('plot', width = 1000, height = 800)),
          tabPanel("Filtered Table",
            DTOutput('table')
                        )
                    )
)

server <- function(input, output, session) {
    # optimize loading time media_type 'choices = NULL'
    updateSelectizeInput(session, 'media_type',
                      choices = unique(state_contaminants$media))
    # pushing the selections through reactive functions
    mediaData <- reactive({
        state_contaminants %>%
            filter(media %in% input$media_type)
    }) 
    # filtering the timespan slider
    filteredData <- reactive({
        mediaData()[mediaData()$fiscal_year >= input$range[1] & 
                        mediaData()$fiscal_year <= input$range[2],]
    })
    # pushing the filtered 'media_data' to sort out the contaminants
    subsetData <- reactive({
        mediaData()%>% 
            filter(contaminant_name %in% input$contaminant_type)
    })
    # pushing the subset data through another 'timespan' function
    filteredSubsetData <- reactive({
        subsetData()[subsetData()$fiscal_year >= input$range[1] & 
                         subsetData()$fiscal_year <= input$range[2],]
    })
    # eliminate and empty data error using req() for the highcharter data
    hcData <- reactive({
        
        req(input$media_type)
        req(input$contaminant_type)
        
        filteredSubsetData()
    })
    
    #output leaflet map
    output$map <- renderLeaflet(
        leaflet() %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
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
                       fillOpacity = 0.3, popup = ~paste(contaminant_name, ": ", 
                                                         site_name, state_abr))
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
    
    observeEvent(input$contaminant_type, {
    
    output$plot <- renderPlot(
        state_contaminants %>% 
            filter(contaminant_name %in% filteredSubsetData()$contaminant_name) %>%
            filter(!log_odds_weighted < 3) %>% 
            ggplot(aes(x = log_odds_weighted, y = contaminant_name, fill = contaminant_name)) +
            geom_density_ridges(alpha = 0.9, scale = 5, show.legend = FALSE) +
                scale_fill_brewer(type = "seq", palette = 2, direction = 1,
                                  aesthetics = "fill") +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text.x = element_blank(),
                  plot.title = element_text(size = 16),
                  plot.subtitle = element_text(size = 14)) +
            labs(title = "Weighted Log Odds",
                 subtitle = "Frequency of contaminant distribution within a state versus all other states using Dirichlets prior",
                 y = NULL,
                 x = NULL))
        
    })
    
    output$table <- renderDT(
        filteredSubsetData() %>% 
            select(state_name, contaminant_name, site_name, fiscal_year, media) %>% 
            arrange(fiscal_year)
        
    )
    
    observeEvent(input$contaminant_type, {

           
    output$log_odds <- renderHighchart({
        
        hchart(hcData(), "bar", hcaes(contaminant_name, log_odds_weighted)) %>% 
            hc_colors(colors = "#000000") %>% 
            hc_title(text = paste("Contaminant Frequency by State")) %>% 
            hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
            hc_yAxis(title = list(text = "Weighted Log Odds"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
            hc_legend(enabled = FALSE) %>% 
            hc_tooltip(pointFormat = "Contaminants: <b>{point.y}</b>") %>% 
            hc_plotOptions(series = list(cursor = "default")) %>% 
            hc_add_theme(hc_theme_smpl()) %>% 
            hc_chart(backgroundColor = "transparent")
            })
    })
    
    observeEvent(input$media_type, {
        updateSelectizeInput(session, "contaminant_type", 
                             choices = unique(mediaData()$contaminant_name))
    })
    
    
}
shinyApp(ui = ui, server = server)
