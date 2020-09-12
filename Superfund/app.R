library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(ggplot2)
library(ggridges)
library(highcharter)

ggrpal <- c("#6B0077", "#713A8E", "#765CA5", "#7B7ABB",
            "#8096CF", "#86B0E1", "#8DC9EF", "#97DFFB",
            "#A4F1FF", "#B3FDFF", "#6B0077", "#713A8E",
            "#765CA5", "#7B7ABB", "#8096CF", "#86B0E1",
            "#8DC9EF", "#97DFFB")

ui <- fluidPage(theme = shinytheme(theme = "lumen"),
                    
        tabsetPanel(
          # leaflet map user interface
          tabPanel("Map", leafletOutput('map', width = 1175, height = 839),
            absolutePanel(top = 30, right = 10,
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
              absolutePanel(bottom = 5, left = 5,
                          draggable = TRUE,
                          tags$text("Data::"),
                          tags$a("EPA Superfund", href = "https://www.epa.gov/superfund/search-superfund-sites-where-you-live"),
                          tags$text(" Stats::"),
                          tags$a("Tidylo", href = "https://github.com/juliasilge/tidylo/"),
                          tags$text(" App::"),
                          tags$a("Shiny", href = "https://shiny.rstudio.com/"),
                          tags$text("Mapping::"),
                          tags$a("Leaflet", href = "https://rstudio.github.io/leaflet/"),
                          tags$text("Chart::"),
                          tags$a("Highcharter", href = "https://github.com/jbkunst/highcharter"),
                          class = 'card'),
              absolutePanel(top = 200, left = 20, 
                          width = 300, height = 600,
                          draggable = TRUE,
                          highchartOutput('log_odds'))),
          tabPanel("Density Plot",
            plotOutput('plot', width = 1000, height = 800)),
              # absolutePanel(bottom = 5, left = 5,
              #               draggable = TRUE,
              #               tags$text("Dirichlet Distribution"),
              #               tags$a("Wikipedia Page", href = "https://en.wikipedia.org/wiki/Dirichlet_distribution"),
              #               tags$text("Plot::"),
              #               tags$a("ggridges", href = "https://github.com/wilkelab/ggridges"),
              #               class = 'card'),
          tabPanel("Filtered Table",
            DTOutput('table')),
              # absolutePanel(bottom = 5, left = 5,
              #               draggable = TRUE,
              #               tags$text("Datatable"),
              #               tags$a("DT", href = "https://rstudio.github.io/DT/"),
              #               class = 'card')
          tabPanel("Information",
              textOutput("EPA Cleanup Efforts", inline = TRUE),
                br(),
                h4("National Superfund Properties List"),
                p("The Environmental Protection Agency has enjoyed many years of progress against the",
                  "historical damage to our natural environment from unregulated, unconscionable abuse",
                  "and polution.",
                  "All of the damage has been done in the name of profits and progress, leaving the voiceless",
                  "and vulnerable to suffer for generations."),
                br(),
                p("I reflect back to the accounts of the health of the forests when New York was young;",
                  "springs flowing as the ancient trees lifted the water table with their roots. The surrounding",
                  "lakes that were poisoned and eventually filled in by the manufacturing filth that followed" ,
                  "the colonizers wherever they travelled.",
                  "The Indigenous peoples must have been shocked by our complete disregard for the natural world."),
                br(),
                p("Begin by selecting the media category",
                  "Layer as many contaminants as you wish",
                  "The highcharter widget will update based on your selections displaying the frequency or pervasiveness",
                  "of a contaminant.",
                  "Popups include links to the site and contaminant information",
                  "Refresh the page if you get stuck",
                  "The density plot is an extension of the frequency of a contaminant",
                  "The datatable is organized chronologically")
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
            setView(lng = -107.005776, lat = 43.039165, zoom = 4) %>%  
            addResetMapButton() 
    )
    
    observe({
    
        #2nd level proxy that renders the animal data  
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, 
                       radius = ~log_odds_weighted * 250, weight = 1,
                       color = "#000000", fillColor = ~colour,
                       fillOpacity = 0.3, 
                       popup = ~paste(sep = "<br/>",
                                      paste(pubclink, contaminant_name, "</a></b>"),
                                      paste(sitelink, site_name, "</a></b>")))
    })
    
    #leaflet map observer / proxy for filtered subset data
    observe({
        
        leafletProxy("map", data = filteredSubsetData()) %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, 
                       radius = ~log_odds_weighted * 250, weight = 1,
                       color = "#000000", fillColor = ~colour,
                       fillOpacity = 0.3,
                       popup = ~paste(sep = "<br/>",
                                      paste(pubclink, contaminant_name, "</a></b>"),
                                      paste(sitelink, site_name, "</a></b>")))
    })
    
    observeEvent(input$contaminant_type, {
    
    output$plot <- renderPlot(
        state_contaminants %>% 
            filter(contaminant_name %in% filteredSubsetData()$contaminant_name) %>% 
            add_count(contaminant_name) %>% 
            filter(n > 2) %>% 
            ggplot(aes(x = log_odds_weighted, y = contaminant_name,
                       fill = contaminant_name)) +
            geom_density_ridges(alpha = 0.9, scale = 2, show.legend = FALSE) +
            scale_fill_manual(values = ggrpal) +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text.x = element_blank(),
                  plot.title = element_text(size = 16),
                  plot.subtitle = element_text(size = 14)) +
            labs(title = "Contaminant Frequency",
                 subtitle = NULL,
                 y = NULL,
                 x = "Weighted Log Odds using Dirichlet's prior"))
        
    })
    
    output$table <- renderDT(
        filteredSubsetData() %>% 
            select(state_name, contaminant_name, site_name, fiscal_year, media) %>% 
            arrange(fiscal_year)
        
    )
    
    observeEvent(input$contaminant_type, {

           
    output$log_odds <- renderHighchart({
        
        hchart(hcData(), "bar", hcaes(contaminant_name, log_odds_weighted)) %>% 
            hc_colors(colors = "SteelBlue") %>% 
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
