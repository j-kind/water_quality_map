# Load required Shiny and Leaflet libraries
library(shiny)
library(leaflet)
library(plotly)

# Load and preprocess the data from a CSV file
data <- read_csv("Full_Ecoli_Data.csv") %>%
  # Convert date and time columns to POSIXct format
  mutate(across(4, ~as.POSIXct(., format = "%m/%d/%Y %I:%M:%S %p"))) 

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Water Quality Map"),          # Application title
  
  leafletOutput("map"),                     # Output area for the Leaflet map
  
  verbatimTextOutput("click_info"),         # Output area for displaying click information
  plotlyOutput("plot")
) 


# Define the Shiny server function
server <- function(input, output, session) {
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%                        # Add a generic base tile layer (your map)
      setView(lng = -68.97, lat = 45.52, zoom = 8) %>%  # Set initial map view parameters
      # Add bounds
      addCircleMarkers(
        data = data,                        # Use the 'data' dataframe
        lat = ~Latitude,                    # Specify latitude column
        lng = ~Longitude,                   # Specify longitude column
        layerId = ~`Site Code`              # Specify which column will be used for the id of the markers
      )
  })

  # Define the click event handler
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click)) {
      # If no marker was clicked, exit the event handler
      return()
    }
    
    # Retrieve the ID of the clicked marker (the column that was set above)
    clicked_id <- click$id
    
    # Prepare the text to display in the 'click_info' output area
    output$click_info <- renderText({
      # Construct a message indicating the selected site
      paste("You have selected site", clicked_id)
    })
  })
  
  output$plot <- renderPlotly(
    data %>% plot_ly(
      x = ~`Visit Datetime`, 
      y = ~Result, 
      type = "scatter", 
      mode = "markers", 
      source = "plot"
    )
    
  )
  
}
  
# Run the Shiny application
shinyApp(ui = ui, server = server)

# Example popup HTML https://github.com/rstudio/shiny-examples/blob/main/063-superzip-example/server.R
# https://matt-dray.github.io/earl18-crosstalk/04_leaflet-flexdash-dt-crosstalk.html
