# file for functions

#SCoBC2023
#KBB2020


# 1 f.map.matched.pairs. 
#   makes map of the matched pairs locations


# === 1 f.map.matched.pairs. ============================
# to be used on the matched and corrected dataset = has the 'final' columns 
f.map.matched.pairs <- function(data_for_map){
  
  # Create a leaflet map to check the different points
  # blue is Louise and red is the 
  map <- leaflet() %>%
    addTiles()  # Add default OpenStreetMap tiles
  
  # Add circular markers for matched locations in dataset A
  for (i in 1:nrow(data_for_map)) {
    map <- map %>%
      addCircleMarkers(lng = data_for_map[i, "Longitude_A"], 
                       lat = data_for_map[i, "Latitude_A"],
                       popup = data_for_map[i, "Location_A"],
                       radius = 5,  # Set the radius of the circle
                       color = "blue",  # Set the border color of the circle
                       fillColor = "blue",  # Set the fill color of the circle
                       fillOpacity = 1)  # Set the opacity of the fill
  }
  
  
  # Add circular markers for matched locations in dataset B
  for (i in 1:nrow(data_for_map)) {
    map <- map %>%
      addCircleMarkers(lng = data_for_map[i, "Longitude_final"], 
                       lat = data_for_map[i, "Latitude_final"],
                       popup = data_for_map[i, "Location_final"],
                       radius = 5,  # Set the radius of the circle
                       color = "red",  # Set the border color of the circle
                       fillColor = "red",  # Set the fill color of the circle
                       fillOpacity = 1)  # Set the opacity of the fill
  }
  
  # Add lines connecting matched locations
  for (i in 1:nrow(data_for_map)) {
    map <- map %>%
      addPolylines(lng = c(data_for_map[i, "Longitude_A"], 
                           data_for_map[i, "Longitude_final"]),
                   lat = c(data_for_map[i, "Latitude_A"], 
                           data_for_map[i, "Latitude_final"]),
                   color = "green")  # Set color for connecting lines
  }
  
  # Print the map
  map
}


# === x =======
