# Load necessary libraries
library(shiny)
library(leaflet)
library(plotly)
library(readr)

# Load and preprocess data
data <- read_csv("Full_Ecoli_Data.csv") %>%
  mutate(across(4, ~as.POSIXct(., format = "%m/%d/%Y %I:%M:%S %p"))) 

# Define user interface
ui <- fluidPage(
  titlePanel("Water Quality Map"),  
  leafletOutput("map"),             # For displaying the map
  plotlyOutput("plot")              # For displaying the plot
) 

# Server logic
server <- function(input, output, session) {
  
  # Store the selected site ID from map
  selectedSite <- reactiveVal(NULL)
  
  # Create and render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%                
      setView(lng = -68.97, lat = 45.52, zoom = 8) %>%
      addCircleMarkers(data = data, lat = ~Latitude, lng = ~Longitude, layerId = ~`Site Code`)
  })
  
  # Update selected site ID when clicked on map
  observeEvent(input$map_marker_click, {
    selectedSite(input$map_marker_click$id)
  })
  
  # Filter data based on selected site
  plotData <- reactive({
    if (is.null(selectedSite())) {
      data
    } else {
      data %>% filter(`Site Code` == selectedSite())
    }
  })
  
  # Create and render the plot
  output$plot <- renderPlotly({
    plot_ly(
      data = plotData(), x = ~`Visit Datetime`, y = ~Result, type = "scatter", mode = "markers"
    ) %>%
      layout(title = if (is.null(selectedSite())) "E. coli Levels - All Sites" else paste("E. coli Levels - Site", selectedSite()))
  })
}

# Run the app
shinyApp(ui = ui, server = server)