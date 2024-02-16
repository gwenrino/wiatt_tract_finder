library(sf)
library(tidyverse)
library(rsconnect)

# Define a function to process census tract data
process_census_tracts <- function(statefp, circle_buffer) {
  
  # File path to census tract file for this state
  census_tract_file_path <- paste("data/census_tract_shapefiles/tl_2020_", statefp, "_tract.shp", sep = "")
  
  # Read census tract file, confirm same format as circle_buffer
  census_tracts <- st_read(census_tract_file_path) %>%
    st_transform(st_crs(circle_buffer))
  
  # Find intersections between tracts and circle buffer
  intersections <- st_intersection(census_tracts, circle_buffer)
  
  # Find area of intersections
  intersections$intersection_area <- as.numeric(st_area(intersections))
  
  # Filter to only those tracts that intersect
  tract_geometries <- census_tracts %>% filter(GEOID %in% intersections$GEOID)
  
  # Find area of intersecting tracts
  tract_geometries$tract_area <- as.numeric(st_area(tract_geometries))
  
  # Simplify intersections dataset
  intersections <- intersections %>% select(STATEFP, NAME, GEOID, intersection_area) %>% rename(state_code = STATEFP, census_tract = NAME, geoid = GEOID)
  intersections$geometry <- NULL
  
  # Simplify tracts dataset
  tract_geometries <- tract_geometries %>% select(STATEFP, NAME, GEOID, tract_area) %>% rename(state_code = STATEFP, census_tract = NAME, geoid = GEOID)
  tract_geometries$geometry <- NULL
  
  # Merge data from all states
  areas <- merge(intersections, tract_geometries, by = c("state_code","census_tract", "geoid"))

}
