# Load required libraries
library(sf)
library(tidyverse)
library(shiny)
library(rsconnect)

# Source helper functions
source("helpers.R")

# Load the US state boundaries data
us_states <- st_read("data/state_shapefiles/cb_2022_us_state_500k.shp")

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("WIATT Census Tract Finder"),
  sidebarLayout(
    sidebarPanel(
      strong("Use this app to find the portion of all census tracts that lie within a given radius of a given latitude/longitude within the United States."),
      br(),
      br(),
      p("You can find the latitude and longitude of an address or a point on a map at ",
        a("this website", href = "https://www.latlong.net/", target = "_blank"), "."),
      p("You will get an error message if the latitude/longitude you enter is outside the United States."),
      p("Results may take several minutes to load, especially in populous states.", style = "color:red"),
      numericInput("latitude", "Enter Latitude:", 39.0932062),
      numericInput("longitude", "Enter Longitude:", -94.595614),
      numericInput("radius", "Enter Radius (in miles):", 5),
      downloadButton("downloadCSV", "Download CSV"),
      br(),
      br(),
      strong("Sources"),
      br(),
      a("2020 Census Tract Shapefiles", href = "https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=Census+Tracts", target = "_blank"),
      br(),
      a("1:500,000 State Shapefiles", href = "https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html", target = "_blank")
    ),
    mainPanel(
      h4("State(s) your radius falls within:"),
      textOutput("state_names"),
      br(),
      tableOutput("resultTable")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  
  # Reactive function to process census tracts
  identify_tracts_calculate_percentages <- reactive({
    
    # Convert radius to meters
    radius_in_meters <- input$radius * 1609.34

    # Create a data frame with the provided longitude and latitude
    point <- data.frame(lon = input$longitude, lat = input$latitude)

    # Define the point as a spatial data frame with a specific CRS (WGS 84 - EPSG:4326)
    point_sf <- st_as_sf(point, coords = c("lon", "lat"), crs = 4326)

    # Ensure both datasets have the same CRS
    us_states <- st_transform(us_states, st_crs(point_sf))

    # Define the circle around the point with the given radius
    circle_buffer <- st_buffer(point_sf, dist = radius_in_meters)

    # Identify intersecting states
    state_intersections <- st_intersects(circle_buffer, us_states)
    intersecting_states <- us_states[unlist(state_intersections), ]
    statefps <- unique(intersecting_states$STATEFP)

    # Process census tracts for each intersecting state
    all_areas <- data.frame()
    for (statefp in statefps) {
      processed_tracts <- process_census_tracts(statefp, circle_buffer)
      all_areas <- bind_rows(all_areas, processed_tracts)
    }

    # Calculate percentage
    area_calculator <- all_areas %>%
      mutate(portion_of_tract_within_radius = intersection_area / tract_area) %>%
      select(state_code, census_tract, geoid, portion_of_tract_within_radius) %>%
      arrange(state_code, census_tract)

    return(area_calculator)
  })
  
  # Reactive function to get state name(s)
  get_state_names <- reactive({
    
    intersecting_state_codes <- identify_tracts_calculate_percentages()
    
    us_states_filtered <- us_states %>%
      filter(STATEFP %in% intersecting_state_codes$state_code) %>%
      select(NAME)
    
    us_states_filtered$geometry <- NULL
    
    intersecting_state_names <- us_states_filtered %>% pull
      
    return(intersecting_state_names)
    
  })

  # Output table of results
  output$resultTable <- renderTable({
    identify_tracts_calculate_percentages()
  })
  
  # Output text of state names
  output$state_names <- renderText({
    get_state_names()
  })

  # Downloadable CSV of results
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("census_tracts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(identify_tracts_calculate_percentages(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)