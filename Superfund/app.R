
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(highcharter)
library(dplyr)

state_contaminants <- read.csv("state_contaminants.csv")

ui = dashboardPagePlus(skin = "black-light",
                       collapse_sidebar = TRUE,
                       sidebar_fullCollapse = TRUE,
         
       dashboardHeaderPlus(title = "EPA Superfund Sites",
                           enable_rightsidebar = TRUE,
                           rightSidebarIcon = "exchange"),
       
       dashboardSidebar(collapsed = TRUE,
                        chooseSliderSkin("Modern", color = "#8b109e"),
                       
         sliderInput("range", label = NULL, ticks = FALSE,
           min(state_contaminants$fiscal_year),
           max(state_contaminants$fiscal_year),
           value = range(state_contaminants$fiscal_year), step = 1),
         selectizeInput('media_type', label = NULL,  
           choices = NULL,
           multiple = TRUE,
           options = list(placeholder = "Select Contaminated Media",
           onInitialize = I('function() { this.setValue(""); }'))),
         selectizeInput('contaminant_type', label = NULL, 
           choices = unique(state_contaminants$contaminant_name), 
           multiple = TRUE,
           options = list(placeholder = "Layer Contaminants",
           onInitialize = I('function() { this.setValue(""); }'))),
         textOutput("Selections", inline = FALSE),
         p("Use the slider to select the range of years contaminated properties",
           "were added to the EPA National Superfund List"),
         p("Circlemarkers contain links to contaminant and site information"),
         p("Stats and info tabs in the right sidebar")
         
         ),
       dashboardBody(
           tags$head(
               tags$style("p{color: black;
                             padding-left: 10px;}
                           label{color: black;
                                 text.align: center;}
                           a{color:#8b109e;}")),
           leafletOutput("map", width = "100%", height = 825)),
       rightSidebar(background = 'light', width = 240,
           rightSidebarTabContent(
               id = "Highcharter",
               title = NULL,
               icon = "bar-chart",
               highchartOutput('log_odds'),
               textOutput("tidylo", inline = FALSE),
               p("The", tags$a("Highcharter", href = "https://jkunst.com/highcharter/"),
                 "bar plot will update based on current selections showing the pervasiveness of",
                 "a contaminant across each state. Used with permission",
                 tags$a("Creative Commons Licence", href = "https://mautic.highsoft.com/r/4b0d3077ad7001951f2e314db?ct=YTo1OntzOjY6InNvdXJjZSI7YToyOntpOjA7czo0OiJmb3JtIjtpOjE7aToxMDt9czo1OiJlbWFpbCI7aToxMDtzOjQ6InN0YXQiO3M6MjI6IjVmNjE5OWVlMzkxN2U5ODk2NDUyNzEiO3M6NDoibGVhZCI7aToyMDcxMDY7czo3OiJjaGFubmVsIjthOjE6e3M6NToiZW1haWwiO2k6MTA7fX0%3D&")),
               p("n = count(state, contaminants)"),
               p("The",
                 tags$a("Tidylo", href = "https://github.com/juliasilge/tidylo/"),
                 "package was used to generate this Bayesian prior using", 
                 tags$a("Dirichlet's Distribution", href = "https://en.wikipedia.org/wiki/Dirichlet_distribution")),
               p("Citations Packages, ",
                 tags$a("Shiny", href = "https://shiny.rstudio.com/"), "+",
                 tags$a("Widgets", href = "https://github.com/dreamRs/shinyWidgets"), "+",
                 tags$a("DashboardPlus,", href = "https://github.com/RinteRface/shinydashboardPlus"),
                 tags$a("Leaflet,", href = "https://rstudio.github.io/leaflet/"),
                 tags$a("Tidyverse,", href = "https://www.tidyverse.org/"))
           ),
           rightSidebarTabContent(
               id = "Links",
               title = tags$a("National Superfund Properties", href = "https://www.epa.gov/superfund/superfund-data-and-reports"),
               icon = "file-text-o",
               textOutput("Links", inline = FALSE),
               p("The National Properties List includes sites that are being monitored to ensure that the",
                 "construction or control measures in place are meeting their environmental requirements.",
                 "I invite you to take the time to review a few sites of interest and the great work",
                 "involved in minimizing the impact of the pollutants."),
               p("Diminishing the regulatory powers of the Environmental Protection Agency will invite",
                 "polluters to degrade the progress that has been made since 1970, deferring the health effects,",
                 "complicated and costly cleanups to future generations."),
               p("Contaminants need to be controlled or captured prior to their release into the environment.",
                 "Investments through tax incentives or grants would be much cheaper than medical and cleanup",
                 "costs; not to mention the natural worlds potential to support human life.")
           )
        )
    )


server = function(input, output, session) {
    
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
            setView(lng = -98.585522, lat = 39.8333333, zoom = 3) %>%  
            addResetMapButton() 
    )
    
    observe({
        
        #2nd level proxy that renders the animal data  
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, 
                       radius = ~log_odds_weighted * 400, weight = 1,
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
                       radius = ~log_odds_weighted * 400, weight = 1,
                       color = "#000000", fillColor = ~colour,
                       fillOpacity = 0.3,
                       popup = ~paste(sep = "<br/>",
                                      paste(pubclink, contaminant_name, "</a></b>"),
                                      paste(sitelink, site_name, "</a></b>")))
    })
    
    observeEvent(input$media_type, {
        updateSelectizeInput(session, "contaminant_type", 
                             choices = unique(mediaData()$contaminant_name))
    })

    observeEvent(input$contaminant_type, {


        output$log_odds <- renderHighchart({

            hchart(hcData(), "bar", hcaes(contaminant_name, log_odds_weighted)) %>%
                hc_colors(colors = "#8b109e") %>%
                hc_title(text = paste("Contaminant Prevalence")) %>%
                hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>%
                hc_yAxis(title = list(text = "Weighted Log Odds"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
                hc_legend(enabled = FALSE) %>%
                hc_tooltip(pointFormat = "Value: <b>{point.y}</b>") %>%
                hc_plotOptions(series = list(cursor = "default")) %>%
                hc_add_theme(hc_theme_smpl()) %>%
                hc_chart(backgroundColor = "transparent")
        })
    })
    
}
        
shinyApp(ui = ui, server = server)
