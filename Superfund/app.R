library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(ggplot2)
library(ggridges)
library(highcharter)

state_contaminants <- read.csv("state_contaminants.csv")


ggrpal <- c("#6B0077", "#713A8E", "#765CA5", "#7B7ABB",
            "#8096CF", "#86B0E1", "#8DC9EF", "#97DFFB",
            "#A4F1FF", "#B3FDFF", "#6B0077", "#713A8E",
            "#765CA5", "#7B7ABB", "#8096CF", "#86B0E1",
            "#8DC9EF", "#97DFFB", "#765CA5", "#7B7ABB",
            "#8096CF", "#86B0E1", "#8DC9EF", "#97DFFB",
            "#A4F1FF", "#B3FDFF", "#6B0077", "#713A8E",
            "#765CA5", "#7B7ABB", "#8096CF", "#86B0E1",
            "#8DC9EF", "#97DFFB")

ui <- bootstrapPage(theme = shinytheme(theme = "lumen"),
                    
        tabsetPanel(
          # leaflet map user interface
          tabPanel("Map", leafletOutput('map', height = 825),
            absolutePanel(top = 30, right = 10,
              draggable = TRUE,
              titlePanel(h3("EPA Superfund Properties")),
              sliderInput("range", "Year Added to NPL", 
                          min(state_contaminants$fiscal_year),
                          max(state_contaminants$fiscal_year),
                          value = range(state_contaminants$fiscal_year), step = 1),
              selectizeInput('media_type', label = NULL,  
                          choices = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Media Types  ~  Tab Select",
                          onInitialize = I('function() { this.setValue(""); }'))),
              selectizeInput('contaminant_type', label = NULL, 
                          choices = unique(state_contaminants$contaminant_name), 
                          multiple = TRUE,
                          options = list(placeholder = "Contaminants  ~  Tab Select",
                          onInitialize = I('function() { this.setValue(""); }')))),
              absolutePanel(bottom = 10, left = 5,
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
              absolutePanel(top = 220, left = 20, 
                          width = 300, height = 600,
                          draggable = TRUE,
                          highchartOutput('log_odds'))),
          tabPanel("Density Plot",
            plotOutput('plot', height = 800, width = 1200)),

          tabPanel("Filtered Table",
            DTOutput('table')),
            
          tabPanel("Info",
            absolutePanel(top = 40, left = 20, height = 960,
              textOutput("EPA Cleanup Efforts", inline = FALSE),
                br(),
                h5(tags$a("National Superfund Properties List", href = "https://www.epa.gov/superfund/superfund-data-and-reports")),
                p("The Environmental Protection Agency has enjoyed many years of progress controlling and", 
                  "correcting historical damage to the natural environment from unregulated, unconscionable",
                  "abuse, laziness, ignorance and lasting pollution.",
                  "All of the damage has been done in the name of profits and progress, leaving the voiceless",
                  "and vulnerable to suffer for generations."),
                p("The National Properties List includes sites that are being monitored to ensure that the",
                  "construction or control measures in place are meeting their environmental requirements.",
                  "I invite you to take the time to review a few sites of interest and the great work",
                  "involved in minimizing the impact of the pollutants."),
                p("Diminishing the regulatory powers of the Environmental Protection Agency will invite",
                  "polluters to degrade the progress that has been made since 1970, deferring the health effects,",
                  "complicated and costly cleanups to future generations."),
                p("Contaminants need to be controlled or captured prior to their release into the environment.",
                  "Investments through tax incentives or grants would be much cheaper than! medical and cleanup",
                  "costs; not to mention the natural worlds potential to support human life."),
                p("Reflect back to the accounts of the health and abundance of the natural world when New York was",
                  "young; springs flowing in the nearby forests as the ancient trees lifted the water table with their",
                  "roots. The surrounding lakes that were poisoned and eventually filled in by the garbage and manufacturing",
                  "waste that followed. The earth needs to be quickly restored to its once previous state, it was destroyed by a",
                  "few generations and can be revitalized by the current stewards and subsequent generations."), br(),
                h5(tags$a("Interactive Leaflet Map", href = "https://rstudio.github.io/leaflet/")),
                p("Select one or more media categories; relevant contaminants will be available directly below.",
                  "Layer as many contaminants as you wish (tab to select), circle markers will begin to appear based on the", 
                  "filtered data. Zoom to an area of interest, selecting a circle marker will display the site and contaminant",
                  " name. Follow the links to National Library of Medicine and EPA Superfund Properties Database.",
                  "The highcharter barplot (Used with permission under the",
                  tags$a("Creative Commons Licence", href = "https://mautic.highsoft.com/r/4b0d3077ad7001951f2e314db?ct=YTo1OntzOjY6InNvdXJjZSI7YToyOntpOjA7czo0OiJmb3JtIjtpOjE7aToxMDt9czo1OiJlbWFpbCI7aToxMDtzOjQ6InN0YXQiO3M6MjI6IjVmNjE5OWVlMzkxN2U5ODk2NDUyNzEiO3M6NDoibGVhZCI7aToyMDcxMDY7czo3OiJjaGFubmVsIjthOjE6e3M6NToiZW1haWwiO2k6MTA7fX0%3D&"), 
                  ") displays the weighted log odds; frequency or pervasiveness of a contaminant",
                  "in each state weighted by the number of contaminated sites."),
                  p("Refresh the shiny application if you get stuck, remember tab to complete your selections"), br(),
                h5(tags$a("Density Plot", href = "https://github.com/wilkelab/ggridges")), 
                 p("The geometry from the ggridges package expands on the previous", 
                  tags$a("highcharter", href = "https://github.com/jbkunst/highcharter"), 
                  "bar plot showing the frequency or pervasiveness of a contaminant across each state. The",
                  tags$a("Tidylo", href = "https://github.com/juliasilge/tidylo/"),
                  "package was used to generate this Bayesian prior using", 
                  tags$a("Dirichlet's Distribution", href = "https://en.wikipedia.org/wiki/Dirichlet_distribution"),"."), br(),
                h5(tags$a("Data Table", href = "https://rstudio.github.io/DT/")),
                p("Displays the filtered contaminants from the year added to the National Superfund Properties List"), br(),
                h5(tags$a("Environmental Stewardship", href = "https://en.wikipedia.org/wiki/Environmental_stewardship")),
                 p("The future or our species will depend on how quickly we adapt to our reality by",
                   "transitioning to a", 
                   tags$a("circular economy", href = "https://www.canada.ca/en/services/environment/conservation/sustainability/circular-economy.html"),
                   ". Methods, institutions, products, resources generated by dirty",
                   "manufacturing will be forgotten and history will highlight our current age as a time when our species avoided extinction."), br(),
                h5(tags$a("Superfund GITHUB Repository", href = "https://github.com/GreenPoD/SuperFund"))
            )     
        )                    
    )
)

server <- function(input, output, session) {
    # optimize loading time media_type 'choices = NULL'
    updateSelectizeInput(session, 'media_type',
                      choices = unique(state_contaminants$media))
    # pushing the selections through reactive functions
    mediaData <- reactive({
        state_contaminants <- state_contaminants %>%
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
            arrange(desc(n.contaminants_state)) %>% 
            ggplot(aes(x = log_odds_weighted, y = contaminant_name,
                       fill = contaminant_name)) +
            geom_density_ridges(alpha = 0.9, scale = 2, show.legend = FALSE) +
            scale_fill_manual(values = ggrpal) +
            theme_ridges() +
            theme(panel.grid = element_line(color = "SteelBlue", lineend = "round"),
                  plot.title = element_text(size = 14),
                  plot.subtitle = element_text(size = 15)) +
            labs(title = NULL,
                 subtitle = "Contaminant Prevalance in EPA National Superfund Properties",
                 y = NULL,
                 x = "Weighted Log Odds using Dirichlet Distribution n = count(state, contaminants)"))
        
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
            hc_title(text = paste("Contaminant Prevalence Superfund Sites")) %>% 
            hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
            hc_yAxis(title = list(text = "Weighted Log Odds"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
            hc_legend(enabled = FALSE) %>% 
            hc_tooltip(pointFormat = "Value: <b>{point.y}</b>") %>% 
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
