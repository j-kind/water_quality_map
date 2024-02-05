# ---- Load Libraries ----
# Required libraries for the application
library(shiny)
library(leaflet)
library(plotly)
library(readr)

# ---- Load and preprocess data ----
# Reading in and preparing data 
# Adding column for positive or negative result
data <- read_csv("Full_Ecoli_Data.csv") %>%
  mutate(across(4, ~ as.POSIXct(., format = "%m/%d/%Y %I:%M:%S %p"))) %>%
    group_by(`Site Code`) %>%
    mutate(exceeded_threshold = any(Result > 236)) %>%
    ungroup()

max_result <- plyr::round_any(max(data$Result, na.rm = TRUE), 50, ceiling)

# ---- Define Shiny UI ----
# User Interface of the Shiny app
ui <- fluidPage(
  # Styling and Title
  tags$div(style = "text-align: center; color: #1d3259;",
           titlePanel("Water Quality Map")),
  # Welcome message
  HTML(
    '<div style="padding: 10px; font-size: 20px; color: #1d3259; text-align: center;">
      <p>Welcome to the Water Quality Map! Check E. Coli levels in your local waterways.</p>
      <p>Click on a marker to view a history of E.coli levels for that site.</p>
     
    </div>'
  ),
  # Leaflet map output
  tags$div(style = "padding: 5px 20px 0px 20px; margin: 5px 20px;",
           leafletOutput("map")),
  # Plotly plot output
  HTML('<div style="padding: 0px 20px 0px 20px; margin: 0px 20px 0px 20px; color: #1d3259; font-size: 12px;">
   <p>Red markers indicate sites where at least one violation has been found</p>
  </div>
       <br>'),
  tags$div(style = "padding: 5px 20px; margin: 5px 20px;", 
           plotlyOutput("plot"))
)

# ---- Define Server ----
# Server logic of the Shiny app
server <- function(input, output, session) {
  # Store the selected site ID
  selectedSite <- reactiveVal(NULL)
  
  # Leaflet Map Rendering
  # Sets up and renders the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -68.97, lat = 45.52, zoom = 8) %>%
      addCircleMarkers(
        data = data, 
        lat = ~ Latitude, 
        lng = ~ Longitude, 
        layerId = ~ `Site Code`, 
        radius = 5, 
        color = ~ifelse(exceeded_threshold, "#b81604", "#0254eb"),
        fillColor = ~ifelse(exceeded_threshold, "#b81604", "#0254eb"),
        fillOpacity = 0.8, 
        stroke = TRUE, 
        weight = 3
      ) %>%
      setMaxBounds(lng1 = -69.5, lat1 = 45, lng2 = -68.5, lat2 = 46)
  })
  
  # Observe map click event
  # Updates the selected site based on user's map interaction
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click)
    selectedSite(input$map_marker_click$id)
  })
  
  # Plotly Plot Rendering
  # Prepares and renders the plotly plot based on the selected site
  plotData <- reactive({
    req(data)
    if (is.null(selectedSite())) {
      data
    } else {
      data %>% filter(`Site Code` == selectedSite())
    }
  })
  
  output$plot <- renderPlotly({
    req(plotData())
    plot_ly(
      data = plotData(),
      x = ~ `Visit Datetime`,
      y = ~ Result,
      type = "scatter",
      mode = "markers",
      marker = list(color = '#1066d0')
    ) %>%
      layout(
        title = if (is.null(selectedSite()))
        list(text = "E. coli Levels - All Sites", font = list(color = '#1d3259'))
        else
          list(text = paste("E. coli Levels - Site", selectedSite()), font = list(color = '#1d3259')),
        xaxis = list(title = "Date", titlefont = list(color = '#1d3259'), tickfont = list(color = '#1d3259')),
        yaxis = list(title = "E. coli (MPN/100mL)", titlefont = list(color = '#1d3259'), 
                     tickfont = list(color = '#1d3259'), range = c(0, max_result)),
        shapes = list(
          list(
            type = 'line',
            y0 = 236, y1 = 236, x0 = 0, x1 = 1,
            xref = 'paper', yref = 'y',
            line = list(color = "#b81604", width = 2)
          )
        )
      )
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)