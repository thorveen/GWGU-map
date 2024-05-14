


# A) add new data gather for the 2024 survey
# B) make a selection for the sites for the survey.



# === A extra information ====
all.comb <- read.csv(paste(p.data.clean, "KBB2020andSCoBC2023combined.csv", sep ="") )
colnames(all.comb)
# location of all locations in the reference SCoBC2023
loc.all <- read.csv(paste(p.data.clean, "coordinates.TableA1-11.SalishSea.csv", sep ="") )
head(loc.all)

loc.t <- which(grepl("^Insk.*", all.comb$LocationName))
all.comb[loc.t,]

# --- eBird data ----
# make function to find ID, name, Lat, lon via location names
f.location.data <- function(location.name, df.name){
  loc.dat <- df.name[which(df.name$LocationName == location.name)[1], 
                     c("ID", "LocationName","Latitude", "Longitude")]
}

# Eagle Rocks: 1 breeding with 2 eggs 1999 --> already included in SCoBC2023
#source <-  "eBird https://ebird.org/checklist/S35474080" 
#location <- "Eagle Rocks"
#new.entry <- f.location.data(location, all.comb)
#EagleRock <- data.frame(
#  ID = new.entry$ID,
#  LocationName = location,
#  Latitude = new.entry$Latitude,
#  Longitude = new.entry$Longitude,
#  Year = 1999,  
#  Count = 1,   
#  Source = source
#)
#all.comb <- rbind(all.comb, EagleRock)

# Major Islet, confirmed recent fledgling. 145 gwgu total
source <-  "eBird https://ebird.org/canada/checklist/S114668604 1 based on a fledged young"
location <- "Major Islet"
new.entry <- f.location.data(location, all.comb)
MajorIslet <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2022,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, MajorIslet)
tail(all.comb)

# Maude Island, confirmed 74 nests
source <- "eBird https://ebird.org/checklist/S18522740"
location <- "Maude Island"
new.entry <- f.location.data(location, all.comb)
MaudeIsland <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 1995,  
  Count = 74,   
  Source = source  
)
all.comb <- rbind(all.comb, MaudeIsland)
tail(all.comb)

# Powell Islets
source <- "eBird https://ebird.org/checklist/S92324895 confirmed nests 50 GWGU seen"
location <- "Powell Islets"
new.entry <- f.location.data(location, all.comb)
PowelIsl21 <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2021,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, PowelIsl21)

source <- "eBird https://ebird.org/canada/checklist/S146032308 fledge young 300 GWGU seen"
new.entry <- f.location.data(location, all.comb)
PowelIsl23 <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2023,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, PowelIsl23)

# Sea Egg Rocks
source <- "eBird https://ebird.org/checklist/S92400395 nest with young, 112 GWGU"
location <- "Sea Egg Rocks"
new.entry <- f.location.data(location, all.comb)
SeaEgg <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2021,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, SeaEgg)


# Hoskyn Rock (misnamed Seal Rock)
source <- "eBird https://ebird.org/checklist/S114523146 Resting and nesting on small islet at south entrance to Hoskins Channel"
location <- "Hoskyn Rock"
# new GWGU site so look up location in reference
new.entry <- f.location.data(location, loc.all)
HoskynRock <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2022,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, HoskynRock)

# new location --> misnamed Hoskyn Rock
source <- "eBird https://ebird.org/checklist/S115267492 may birds sitting on nests , no sign of baby birds yet"
location <- "Dunsertville Islet NW rock"
Dunster <- data.frame(
  ID = NA,
  LocationName = location,
  Latitude = 50.146447,
  Longitude = -125.164574,
  Year = 2022,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, Dunster)
tail(all.comb)

# Centre Islet
source <- " eBird https://ebird.org/checklist/S47724664 juvenile most likely fledged on Centre Islet"
location <- "Centre Islet"
# new GWGU site so look up location in reference
new.entry <- f.location.data(location, loc.all)
CentreIslet <- data.frame(
  ID = new.entry$ID,
  LocationName = location,
  Latitude = new.entry$Latitude,
  Longitude = new.entry$Longitude,
  Year = 2018,  
  Count = 1,   
  Source = source  
)
all.comb <- rbind(all.comb, CentreIslet)



# ---- new column for sites which are covered by other groups
loc.covered <- c("Victoria", "VictoriaBBS","Trial Islands", "Harris Island",
                 "Mary Tod Island",
                 "Mandarte Island", "Mandarte South Islet","Arbutus Island", 
                 "Mitlenatch Island", 
                 "Vancouver Harbour","False Creek", "Granville Bridge", 
                 "Burrard Bridge", "Lions Gate Bridge", "White Rock", 
                 "Chain Islets/Great Chain Island", "North Vancouver",
                 "Second Narrows", "Vesuvius Bay", "Brothers Islands")
# add word 'covered' in a new column for locations covered
all.comb <- all.comb %>%
  mutate(Covered = ifelse(LocationName %in% loc.covered, "covered", ""))

# -- National Park
# all locations which are in Gulf Islands National Park (NatPark)
loc.NatPark <- c("Channel Islands", "Belle Chain Islets",
                 "East Point - Cliffs", "Java Islets", "Imrie Island",
                 "Greig Island","Little Group", "Sallas Rocks",
                 "Anniversary Islet", "Reay Island")
# add word NationalPark in a new column for locations covered
all.comb <- all.comb %>%
  mutate(NationalPark = ifelse(LocationName %in% loc.NatPark, "NationalPark", ""))

# -- Provincial Parks
loc.ProvPark <- c("Norris Rocks", "Finnerty Islands")
# add word Provincial Parks in a new column for locations covered
all.comb <- all.comb %>%
  mutate(ProvincialPark = ifelse(LocationName %in% loc.ProvPark, "ProvincialPark", ""))

# -- BC Parks Ecological Reserve
loc.EcolRes <- c("Hudson Rocks","Canoe Islet", "Rose Islets",
                 "Ballingall Islets","Chain Islets/Great Chain Island", 
                 "Trial Islands")
# "Race Rocks" is not in the Salish Sea
# add word EcolReserve in a new column for locations covered
all.comb <- all.comb %>%
  mutate(EcolReserve = ifelse(LocationName %in% loc.EcolRes, "EcolReserve", ""))


# === B site selection ====

# Threshold year
threshold_year <- 2004

# threshold to include colonies of a certain number of breeding pairs or higher before the threshold_year (not to miss large colonies)
threshold_count <- 10

# Filter locations with at least one Count > 0 on or after the threshold year
locations_above_threshold_year <- all.comb %>%
  filter(Year >= threshold_year, Count > 0) %>%
  distinct(LocationName)


# Filter locations with count >= threshold_count in any year before the threshold year
locations_above_threshold_count <- all.comb %>%
  group_by(LocationName) %>%
  filter(any(Count >= threshold_count & Year < threshold_year)) %>%
  distinct(LocationName)

# Combine both conditions to get the final selection of locations
final_locations <- union(locations_above_threshold_year$LocationName, locations_above_threshold_count$LocationName)

# make a new column where there is a 1 if the LocationName matches one in 
# final_locations and a 0 if nt.
all.comb <- all.comb %>%
  mutate(Selected = ifelse(LocationName %in% final_locations, 1, 0))

head(all.comb)

write.csv(all.comb, paste(p.data.clean, "KBB2020andSCoBC2023combined_final.csv", sep = ""), row.names = F)




# === C map experimenting

radius_basic_dot <- 4
scale_max <- 10       # normalize count size between 0 and scale_max


all.GWGU <- read.csv(paste(p.data.clean, "KBB2020andSCoBC2023combined_final.csv", sep = ""))

final_locations <- unique(all.GWGU$LocationName[all.GWGU$Selected == 1])
n.colonies <- length(final_locations)

# Filter the original data frame to include only the final selected locations and all their entries
GWGU.select <- all.GWGU %>%
  filter(LocationName %in% final_locations)

# and a selection of locations not selected
GWGU.not.select <- all.GWGU %>%
  filter(!(LocationName %in% final_locations))


# Group data by LocationName, sort by Year, and concatenate counts for each year
popup_data <- all.GWGU %>%
  group_by(LocationName) %>%
  arrange(Year) %>%
  summarise(Counts = paste(Year, Count, sep = ": ", collapse = "<br>"))



# make selection for sites which are covered
locations_covered <- all.GWGU %>%
  filter(Covered == "covered")

# make selection for sites which are covered
locations_NationalPark <- all.GWGU %>%
  filter(NationalPark == "NationalPark")

# make selection for sites which are covered
locations_ProvincialPark <- all.GWGU %>%
  filter(ProvincialPark == "ProvincialPark")

# make selection for sites which are covered
locations_EcolReserve <- all.GWGU %>%
  filter(EcolReserve == "EcolReserve")


# read shapefile
PowelRiver <- st_read("shp/PowelRiver.shx") # included with sf package
central <- st_read("shp/central.shx") # included with sf package
HoweSound <- st_read("shp/HoweSound.shx") # included with sf package
Nanaimo <- st_read("shp/Nanaimo.shx") # included with sf package
SunshineCoast <- st_read("shp/SunshineCoast.shx") # included with sf package


# read data for boat launches
boat.launch <- read.csv(paste(p.data.clean, "boat_launches.csv", sep = ""))
str(boat.launch)


line.w <- 1.5 # line thickness of polygons

# Create a Leaflet map
map <- leaflet() %>%
  addTiles() # Add default OpenStreetMap tiles

# add polygons for regions
map <- map %>%
  addPolygons(data = PowelRiver, fillOpacity = 0, weight = line.w) %>%
  addPolygons(data = central, fillOpacity = 0, weight = line.w) %>%
  addPolygons(data = HoweSound, fillOpacity = 0, weight = line.w) %>%
  addPolygons(data = Nanaimo, fillOpacity = 0, weight = line.w) %>%
  addPolygons(data = SunshineCoast, fillOpacity = 0, weight = line.w)

# add boat launches
map <- map %>%
  addCircleMarkers(
    data = boat.launch,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "blue",            # Dot color
    radius = 1.7 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 0.7,
    popup = ~~Name,
  )
  
# layer for sites which are National Parks jurisdiction 
map <- map %>%
  addCircleMarkers(
    data = locations_NationalPark,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "green",            # Dot color
    radius = 1.5 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 0.7,
    popup = ~~LocationName,
    #group = "locations"
  )

# layer for sites which are Provincial Parks jurisdiction 
map <- map %>%
  addCircleMarkers(
    data = locations_ProvincialPark,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "limegreen",            # Dot color
    radius = 1.7 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 0.7,
    popup = ~~LocationName,
    #group = "locations"
  )

# layer for sites which are ecological reserves jurisdiction 
map <- map %>%
  addCircleMarkers(
    data = locations_EcolReserve,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "lightgreen",            # Dot color
    radius = 1.7 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 0.7,
    popup = ~~LocationName,
    #group = "locations"
  )

# Add a dot for each unique location
map <- map %>%
  addCircleMarkers(
    data = GWGU.not.select,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "gray",            # Dot color
    radius = 1 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 0.6,
    popup = ~paste(
      "<strong>Location:</strong> ", LocationName, "<br>",
      "<strong>Counts:</strong><br>", popup_data$Counts[match(LocationName, popup_data$LocationName)]
    ),
    group = "locations"
  )


map <- map %>%
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

# sites that are covered in some way or another
map <- map %>%
  addCircleMarkers(
    data = locations_covered,  # Use all.GWGU as data
    lng = ~Longitude, 
    lat = ~Latitude,
    color = "yellow",            # Dot color
    radius = 0.5 * radius_basic_dot,                # Dot radius
    stroke = FALSE,            # No stroke
    fillOpacity = 1,
    popup = ~~LocationName,
    #group = "locations"
  )


# Print the map
map

