#
# script to make a map of the colonies
# visualize size and make data range selection
#


# libraries
#install.packages("tidyverse")
#install.packages("leaflet")
#install.packages("leaflet.extras")


# load libraries
library(tidyverse)
library(leaflet)
library(leaflet.extras)


# load data
wkdir <- getwd()


# load data
all.GWGU <- read.csv(paste(wkdir,"/KBB2020andSCoBC2023combined.csv", sep ="") )

coord.na <- which(is.na(all.GWGU$Latitude))
all.GWGU[coord.na,]
# exclude data from VictoriaBBS as it has no coords
all.GWGU <- all.GWGU[-coord.na,]

# hist(all.GWGU$Year, breaks = 30)

# --- 1 maps of all breeding colonies + numbers --------

# Threshold year
threshold_year <- 2005
radius_basic_dot <- 2

# Extract unique location names and coordinates
unique_locations <- all.GWGU %>%
  distinct(LocationName, Latitude, Longitude)

# Filter data to keep only the data from threshold year onwards
filtered_data <- all.GWGU %>%
  filter(Year >= threshold_year)

# Keep only the most recent count for each LocationName
filtered_data <- filtered_data %>%
  arrange(LocationName, desc(Year)) %>%
  distinct(LocationName, .keep_all = TRUE)
filtered_data$CountScaled <- ((filtered_data$Count - min(filtered_data$Count)) / 
                                (max(filtered_data$Count) - min(filtered_data$Count)) * 5) + radius_basic_dot

# map making.
# Create a Leaflet map
map <- leaflet() %>%
  addTiles()  # Add default OpenStreetMap tiles

# Add a dot for each unique location
map <- map %>%
  addCircleMarkers(data = unique_locations,  # Use unique locations data
                   lng = ~Longitude,          # Longitude
                   lat = ~Latitude,           # Latitude
                   color = "black",            # Dot color
                   radius = radius_basic_dot,                # Dot radius
                   stroke = FALSE,            # No stroke
                   fillOpacity = 1)           # Full opacity

map <- map %>%
  addCircleMarkers(data = filtered_data,  # Use unique locations data
                   lng = ~Longitude,          # Longitude
                   lat = ~Latitude,           # Latitude
                   color = "red",            # Dot color
                   radius = ~CountScaled,                # Dot radius
                   stroke = FALSE,            # No stroke
                   fillOpacity = 1)           # Full opacity

# Print the map
map
