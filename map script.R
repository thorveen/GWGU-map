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
all.GWGU <- read.csv(paste(wkdir,"/KBB2020andSCoBC2023combined_final.csv", sep ="") )
str(all.GWGU)
length(which(is.na(all.GWGU$Latitude)))


# hist(all.GWGU$Year, breaks = 30)

# --- 1 maps of selection  --------

# Threshold year
threshold_year <- 2004
radius_basic_dot <- 4



# threshold to include colonies before the threshold_year
threshold_count <- 10

# length(unique(all.GWGU$LocationName)) #145 colonies total

# Filter locations with at least one Count > 0 on or after the threshold year
locations_above_threshold_year <- all.GWGU %>%
  filter(Year >= threshold_year, Count > 0) %>%
  distinct(LocationName)

# Filter locations with count >= threshold_count in any year before the threshold year
locations_above_threshold_count <- all.GWGU %>%
  group_by(LocationName) %>%
  filter(any(Count >= threshold_count & Year < threshold_year)) %>%
  distinct(LocationName)

# Combine both conditions to get the final selection of locations
final_locations <- union(locations_above_threshold_year$LocationName, locations_above_threshold_count$LocationName)

length(final_locations)

# Filter the original data frame to include only the final selected locations and all their entries
GWGU.select <- all.GWGU %>%
  filter(LocationName %in% final_locations)

#print(final_data)

# Group data by LocationName, sort by Year, and concatenate counts for each year
popup_data <- GWGU.select %>%
  group_by(LocationName) %>%
  arrange(Year) %>%
  summarise(Counts = paste(Year, Count, sep = ": ", collapse = "<br>"))

# Create a leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = GWGU.select,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "black",            # Dot color
    radius = radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 1,
    popup = ~paste(
      "<strong>Location:</strong> ", LocationName, "<br>",
      "<strong>Counts:</strong><br>", popup_data$Counts[match(LocationName, popup_data$LocationName)]
    ),
    group = "locations"
  )

# Display the map
map
