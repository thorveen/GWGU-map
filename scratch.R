# Install and load required packages
#install.packages(c("sf", "leaflet"))
library(sf)
library(leaflet)

# Define the coordinates of your polygon
coords <- matrix(c(
  8.6853, 49.4061,   # Point 1
  8.6875, 49.4054,   # Point 2
  8.6878, 49.4036,   # Point 3
  8.6848, 49.4046,   # Point 4
  8.6853, 49.4061    # Closing Point (Same as Point 1)
), ncol = 2, byrow = TRUE)


# Create an sf polygon object
polygon <- st_polygon(list(coords))




# Define the coordinates of your polygon
coords <- matrix(c(
  8.6853, 49.4061,   # Point 1
  8.6875, 49.4054,   # Point 2
  8.6878, 49.4036,   # Point 3
  8.6848, 49.4057,   # Point 4
  8.6853, 49.4061    # Closing Point (Same as Point 1)
), ncol = 2, byrow = TRUE)


# Create an sf polygon object
polygon <- st_polygon(list(coords))

# Plot the polygon using leaflet
leaflet() %>%
  addTiles() %>%
  addPolygons(data = coords, color = "red", fillOpacity = 0.5) %>%
  fitBounds(lng1 = min(coords[,1]), lat1 = min(coords[,2]),
            lng2 = max(coords[,1]), lat2 = max(coords[,2]))



library(sf)
library(dplyr)
library(leaflet)


shape <- st_read("northern.shx") # included with sf package
str(shape)

cities <- data.frame(name = c("Raleigh", "Greensboro", "Wilmington"),
                     x = c(-78.633333, -79.819444, -77.912222),
                     y = c(35.766667, 36.08, 34.223333),
                     population = c(467665, 299035, 115451)) %>% 
  st_as_sf(coords = c("x", "y", "population"), crs = 4326, dim = "XYZ")


leaflet() %>% 
  addTiles() %>% # genric OSM basemap
  addPolygons(data = shape) %>% # county polygons in blue
 # addCircleMarkers(data = cities, color = "red") # 3 semi 
