
# === description ===========================================================================
## brief description what the script file is about
#
# working with various dataset
# - KBB2020 Louise dataset  is from the reference
#   King, A.M., L.K. Blight, and D.F. Bertram. 2020. Glaucous-winged Gull 
#   (Larus glaucescens) Nesting Sites in the Strait of Georgia. Map. 
#   Environment and Climate Change Canada, Wildlife Research Division, Institute of Ocean Sciences, Sidney, BC, Canada.
# - SCoBC2023 the tables come from   Seabird Colonies of British Columbia & Haida Gwaii 2023.
#   http://www.wildlifebc.org/pdfs/HAIDAGWAIlowResAA.pdf


# TO DO URGENT
# - section D get coordinated for VictoriaBBS from Louise: now Na and deleted 
# - KBB2020andSCoBC2023combined.csv has weird NAs in it
# Argyle Rocks	E Beecher Bay Argyle Rocks	
# Church Island
# Race Rock
# Sooke Bay
# South Bedford Island
# VictoriaBBS
# - KBB2020andSCoBC2023combined.csv has different codes for same locations
#   depending on the source... should be SCoBC2023

# ******************************************************************************
## use descriptive names 
# 1) read raw data  load data from clean data folder and ...
# 2) outliers      test for outliers and entry errors
# 3) .....

# === A pdf wrangling
# === 1) read raw data =========================================================
# read pdf 


# //////////////////////////////////////////////////////////////////////////////
# === 2)  ==============================================================
#PDE_analyzer_i()



# //////////////////////////////////////////////////////////////////////////////
# === 3) ..... =================================================================



# //////////////////////////////////////////////////////////////////////////////
# === B reference coordinates TableA1-11 ====
# data from pdf TableA1-11
# from TableA1-11 
p.coord <- paste(p.data.raw, "coordinates.raw.csv", sep ="")
coord <- read.csv(p.coord)
head(coord)
str(coord)


# Function to extract components from the string
extract_components <- function(input_string) {
  # Extract ID
  id <- sub("^([A-Z]{2}-\\d{3}).*", "\\1", input_string)
  
  # Extract Location name
  location_name <- sub("^[A-Z]{2}-\\d{3} (.*) \\d+\\.\\d+ \\d+\\.\\d+$", "\\1", input_string)
  
  # Extract Latitude and Longitude
  coordinates <- sub("^[A-Z]{2}-\\d{3} .* (\\d+\\.\\d+) (\\d+\\.\\d+)$", "\\1 \\2", input_string)
  coordinates <- strsplit(coordinates, " ")
  latitude <- as.numeric(coordinates[[1]][1])
  longitude <- as.numeric(coordinates[[1]][2])
  
  return(data.frame(ID = id, LocationName = location_name, Latitude = latitude, Longitude = longitude))
}

# Apply the function to the dataframe column
coord_split <- do.call(rbind, lapply(coord$name, extract_components))

# Print the result
head(coord_split)

# make new dataframe
coord.all <- cbind.data.frame(coord[,1:2],coord_split)
coord.all$Longitude <- coord.all$Longitude * -1
head(coord.all)

# save new coordinates
write.csv(coord.all,paste(p.data.clean,"coordinates.TableA1-11.SalishSea.csv", sep = ""), 
          row.names =  F)



# //////////////////////////////////////////////////////////////////////////////
# === C clean SCoBC2023 table 6 ====
table6 <- read.csv(paste(p.data.raw, "table6.csv", sep =""))
head(table6)

# start of new dataframe
table6_clean <- as.data.frame(table6$REGION)
colnames(table6_clean) <- "Region"

# Notes column for new dataframe
Notes <- rep("", nrow(table6))

# a) column 2 and 3 : CODE and NAME are in separate columns
code.name <- data.frame(ID = rep("",nrow(table6)), 
                        LocationName = rep("",nrow(table6)))
str(code.name)

for( i in 1:nrow(table6)){
    if(nchar(table6$CODE[i]) == 6){
      code.name$ID[i] <- table6$CODE[i]
      code.name$LocationName[i] <- table6$NAME[i]
    }
    if(nchar(table6$CODE[i]) > 6){
      code.name$ID[i] <- substr(table6$CODE[i], 1,6)
      code.name$LocationName[i] <- substr(table6$CODE[i], 8, nchar(table6$CODE[i]))
    }
}

table6_clean <- cbind.data.frame(table6_clean, code.name)
head(table6_clean)

# column 4 COUNT
# this is a complex variable which I leave untouched
# with all the abbreviations provides information about general counts


# column Year
# make sure they are all four characters
year.odd <- which(nchar(table6$Year) > 4)
table6$Year[year.odd]
# [1] "before 2000" "c1914"       "1940s"       "‾1961"     [5] "c1960"
Notes[year.odd] <- table6$Year[year.odd]
Year <-table6$Year # make copy of Year
Year[year.odd] <- c(2000, 1914, 1940, 1961, 1960)
Year <- as.numeric(Year)
table6_clean <- cbind.data.frame(table6_clean, Year)


# go through years
colnames(table6)
year.names <- c("1959.1960", "1974", "1977", "1978", "1981", 
                "1986", "1999", "2009.2010")
all.years <- as.data.frame(matrix(NA, nrow = nrow(table6), ncol = length(year.names)))

# 1 - "1959.1960"
  yr <- table6$X1959.1960
  # change entires with multiple counts
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  # "600-1,200e"  "300-500e"    "1,800-2,000" "700-1,000e" 
  yr[which.minus] <- c(900, 400, 1900, 850)
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,1] <- as.numeric(yr)
  
# 2 - "1974"
  yr <- table6$X1974
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,2] <- as.numeric(yr)
  
# 3 - "1977"
  yr <- table6$X1977
  # change entires with multiple counts
  which.minus <- grep("\\d+-\\d+", yr)
  # says none but row 92 is
  yr[92]
  # "259[252] 400e^[f]" 
  yr[92] <- c(330)
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,3] <- as.numeric(yr)  
  
# 4 - "1978" 
  yr <- table6$X1978
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,4] <- as.numeric(yr)  
  
# 5 - "1981"
  yr <- table6$X1981
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  # "3-7e"
  yr[which.minus] <- 5
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,5] <- as.numeric(yr)
  
# 6 - "1986" 
  yr <- table6$X1986
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,6] <- as.numeric(yr)  
  
# 7 - "1999"
  yr <- table6$X1999
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
 
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,7] <- as.numeric(yr)  
  
# 8 - "2009.2010"
  yr <- table6$X2009.2010..b.
  # change entires with multiple years
  which.minus <- grep("\\d+-\\d+", yr)
  yr[which.minus]
  
  #remove anything between [ ] and only select the numbers
  yr <- gsub("\\[.*?\\]", "", yr)
  yr <- gsub("[^0-9]", "", yr)
  all.years[,8] <- as.numeric(yr)   

colnames(all.years) <- year.names
  
# put files together
table6_clean <- cbind.data.frame(table6_clean, all.years)  
str(table6_clean)

# add the coordinates and use same ID and LocationName as 
# coordinates.SalishSea.csv
str(coord.all)
table6_clean2 <- merge(table6_clean, coord.all[, 
                      c("ID", "LocationName", "Latitude", "Longitude")], 
                      by = "ID", all.x = TRUE)
table6_clean2 <- table6_clean2[, -3]
colnames(table6_clean2)[12] <- "LocationName"
table6_clean2 <- table6_clean2[, c(1:2, 12:14, 4:11)]
head(table6_clean2)

write.csv(table6_clean2, paste(p.data.clean, "SCoBC2023_table6_clean.csv", 
                               sep = ""), row.names = FALSE )
# change it into a long format
table6_clean3 <- table6_clean2
colnames(table6_clean3)[c(6,13)] <- c("1960", "2010")
str(table6_clean3)

# Reshape data to long format
table6.long <- pivot_longer(table6_clean3, 
                          cols = matches("\\d{4}"), 
                          names_to = "Year", 
                          values_to = "Count",
                          values_drop_na = TRUE) %>%
  mutate(Year = as.numeric(gsub("[^0-9.]", "", Year)))  # Extract year from column names

head(table6.long)

write.csv(table6.long, paste(p.data.clean, "SCoBC2023_table6_long_clean.csv", 
                               sep = ""), row.names = FALSE )



# //////////////////////////////////////////////////////////////////////////////
# === D check KBB2020: correction + coordinates against reference SCoBC2023 ====

# SCoBC2023
Seabirds.loc <- read.csv(paste(p.data.clean, "coordinates.TableA1-11.SalishSea.csv", sep ="") )
head(Seabirds.loc)

# KBB2020
KBB2020.data <- read.csv(paste(p.data.raw, "KBB2020.data.csv", sep ="") )
head(KBB2020.data)
str(KBB2020.data)
which(is.na(KBB2020.data$Lat))

# VictoriaBBS needs coordinates
KBB2020.data <- KBB2020.data %>%
  mutate(Lat = if_else(Location == "VictoriaBBS", 48.4045900, Lat),
         Long = if_else(Location == "VictoriaBBS", -123.3495700, Long))
which(is.na(KBB2020.data$Lat))
# no more na's, good to go.
colnames(KBB2020.data)

# add Toby Island
TobyIsl <- tibble(Year = 2010,
             Location = "Toby Island", 
             Count = 1,
             Type.of.Info = "Nest",
             Lat = 49.4877,
             Long = -124.6596,
             Source = "Blight and Osler 2010",
             Notes = "Missing from original data sheet; error found and corrected April 2024")
KBB2020.data <- bind_rows(KBB2020.data, TobyIsl)
tail(KBB2020.data)

# Denman Isl year error
loc.t <- which(KBB2020.data$Location == "Denman Island")
KBB2020.data[loc.t,]
# the year for the second entry should be 2010
KBB2020.data$Year[loc.t[2]] <- 2010
# *******

# get an overview of all the unique coordinate sets
# each should have its own location name or be merged.
# Function to get unique coordinates for each location
unique_coordinates <- function(df, location) {
  unique(df[df$Location == location, c("Location", "Location.Description", "Lat", "Long")])
}

# Apply the function to each location
unique_coords_list <- lapply(unique(KBB2020.data$Location), function(x) unique_coordinates(KBB2020.data, x))

# Convert the list to a data frame
unique_coords_df <- do.call(rbind, lapply(unique_coords_list, as.data.frame))

write.csv(unique_coords_df, 
          paste(p.data.clean, "KBB2020.locations.and.coord.csv", sep = ""), 
          row.names = F)

# ---

# make changes to location names with different coordinates ---> split these
# Location is 2nd column and Location.Description 3rd
update_location <- function(df, target_location, target_description, new_location_name) {
  # Find the row indices where Location matches target_location and Location.Description matches target_description
  matching_rows <- which(df$Location == target_location & df$Location.Description == target_description)
  
  # Update the Location name for the matching rows
  df$Location[matching_rows] <- new_location_name
  
  # Return the updated data frame
  return(df)
}
update_location_coord <- function(df, target_location, target_lat, target_long, new_location_name) {
  # Find the row indices where Location matches target_location and Lat and Long match target_lat and target_long
  matching_rows <- which(df$Location == target_location & df$Lat == target_lat & df$Long == target_long)
  
  # Update the Location name for the matching rows
  df$Location[matching_rows] <- new_location_name
  
  # Return the updated data frame
  return(df)
}
# Galiano Island
KBB2020.data <- update_location(KBB2020.data, "Galiano Island", "Cliffs", "Galiano Island Cliffs")
KBB2020.data <- update_location(KBB2020.data, "Galiano Island", "Cliffs NW of Gray Pen.", "Galiano Island Cliffs Gray Pen.")
# Hornby
KBB2020.data <- update_location(KBB2020.data, "Hornby Island", "Bluffs", "Hornby Island Bluffs")
KBB2020.data <- update_location(KBB2020.data, "Hornby Island", "St. John Point", "Hornby Island St. John Point")
# Little Rock --> northern end
KBB2020.data <- update_location_coord(KBB2020.data, "Little Rock", 50.0526,	-124.9136, "Little Rock 1")
KBB2020.data <- update_location_coord(KBB2020.data, "Little Rock", 50.1581, -125.0945, "Little Rock 2")
# Race Rock
KBB2020.data <- update_location_coord(KBB2020.data, "Race Rocks all", 48.304,	-123.5314, "North Race Rock")
#Tsawassen
KBB2020.data <- update_location(KBB2020.data, "Tsawwassen", "Breakwater", "Tsawwassen Breakwater")
KBB2020.data <- update_location(KBB2020.data, "Tsawwassen", "Jetty", "Tsawwassen Jetty")
# end of KBB2020.data location modifications
#write.csv(KBB2020.data, paste(p.data.clean, "KBB2020.check.csv", sep = ""), 
#          row.names = F)


unique_locations <- KBB2020.data %>%
  distinct(Location, .keep_all = TRUE)
str(unique_locations)

KBB2020.data.loc <- unique_locations[, c(2,9,10)]
head(KBB2020.data.loc)
length(which(is.na(KBB2020.data.loc$Lat) == TRUE))
length(which(is.na(KBB2020.data.loc$Long) == TRUE))
# no missing values, all locations have data

# make smaller dataframe of the SCoBC2023 reference locations 
reference.loc <- data.frame(Seabirds.loc$LocationName,
                            Seabirds.loc$Latitude,
                            Seabirds.loc$Longitude,
                            Seabirds.loc$ID)
    
colnames(reference.loc)[1:4] <- c("Location", "Lat", "Long", "Site.Code")
head(reference.loc)

dataset_A <- KBB2020.data.loc # dataset that needs names cross checked
dataset_B <- reference.loc # reference names and coordinates
tail(dataset_A)
head(dataset_B)


# Function to calculate distance between two coordinates
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  earth_radius <- 6371  # Earth radius in kilometers
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat/2) * sin(dLat/2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- earth_radius * c
  return(distance)
}

# Initialize an empty data frame to store matched pairs
matched_pairs <- data.frame(Location_A = character(),
                            Location_Code = character(),
                            Location_B = character(),
                            Latitude_A = numeric(),
                            Latitude_B = numeric(),
                            Longitude_A = numeric(),
                            Longitude_B = numeric(),
                            smallest.distance = numeric(),
                            stringsAsFactors = FALSE)

# Iterate through each entry in dataset A
for (i in 1:nrow(dataset_A)) {

  min_distance <- Inf  # Initialize minimum distance to infinity
  matched_index <- NULL  # Initialize index of matched entry in dataset B
  
  # Calculate distance to each entry in dataset B
  for (j in 1:nrow(dataset_B)) {
    distance <- calculate_distance(dataset_A[i, "Lat"], dataset_A[i, "Long"],
                                   dataset_B[j, "Lat"], dataset_B[j, "Long"])
    
    # Check if this distance is smaller than the current minimum
    if (distance < min_distance) {
      min_distance <- distance
      matched_index <- j
    }
  }
  
  # Add the closest match to the data frame
  matched_pairs <- rbind(matched_pairs, data.frame(Location_A = dataset_A[i, "Location"],
                                                   Location_Code = dataset_B[matched_index, "Site.Code"],
                                                   Location_B = dataset_B[matched_index, "Location"],
                                                   Latitude_A = dataset_A[i, "Lat"],
                                                   Latitude_B = dataset_B[matched_index, "Lat"],
                                                   Longitude_A = dataset_A[i, "Long"],
                                                   Longitude_B = dataset_B[matched_index, "Long"],
                                                   smallest.distance = min_distance))
}


# get an idea of distance threshold by distribution of distances for sites
# with matching names (assuming these are the same sites)
match.names <- which(matched_pairs$Location_A == matched_pairs$Location_B)
dist.threshold <- matched_pairs$smallest.distance[match.names]
hist(dist.threshold)
max(dist.threshold)
# visually looks like 1 is a good cut off --> check everything > 1
matched_pairs$check.location.match <- rep(NA, nrow(matched_pairs)) 
for(i in 1:nrow(matched_pairs)){
  if(matched_pairs$smallest.distance[i] > 1){
    matched_pairs$check.location.match[i] <- "check"
  } else matched_pairs$check.location.match[i] <- NA
}

hist(matched_pairs$smallest.distance, breaks = 100)


# make a copy of matched pairs to be corrected
# the B dataset is changed to become the final corrected set
matched_pairs_corr <- matched_pairs
head(matched_pairs)
matched_pairs_corr$Location_Code_final <- matched_pairs_corr$Location_Code
matched_pairs_corr$Location_final <- matched_pairs_corr$Location_B
matched_pairs_corr$Latitude_final <- matched_pairs_corr$Latitude_B
matched_pairs_corr$Longitude_final <- matched_pairs_corr$Longitude_B
matched_pairs_corr$Notes <- rep(NA, nrow(matched_pairs_corr))
head(matched_pairs_corr)

# dataset A /  KBB2020   = blue 
# dataset B  / SCoBC2023 = red => use are reference
f.map.matched.pairs(matched_pairs_corr)

# different changes can be made based on the situation

# CHANGE 1 - look up using reference name but keep KBB2020's entries
# e.g. Mandarte has one B location but to A (KBB2020) -> keep 2 KBB2020 entries
# for instances where KBB2020 has more detailed info
f.keep.KBB2020.ref <- function(reference.location.name, dataSet){
  #location.name <- "Mandarte Island"
  #dataSet <- matched_pairs_corr
  # get row positions in the dataframe 
  loc.t <- which(dataSet$Location_B == reference.location.name)
  dataSet$Notes[loc.t] <- "coordinates set to KBB2020"
  dataSet$Location_Code_final[loc.t] <- NA
  dataSet$Location_final[loc.t] <- dataSet$Location_A[loc.t]
  dataSet$Latitude_final[loc.t] <- dataSet$Latitude_A[loc.t]
  dataSet$Longitude_final[loc.t] <- dataSet$Longitude_A[loc.t]
  return(dataSet)
}

# CHANGE 2a - keep KBB2020, not closest reference location
# similar to f.keep.KBB2020.ref
# new location, no location code, name, and coord from KBB2020
# change the final name to keep KBB2020's entries but search on 
# KBB2020 name. For instance KBB2020 location is not in reference dataset
f.keep.KBB2020 <- function(KBB2020.location.name, dataSet){
  # get row positions in the dataframe 
  loc.t <- which(dataSet$Location_A == KBB2020.location.name)
  dataSet$Notes[loc.t] <- "name and coordinates set to KBB2020"
  dataSet$Location_Code_final[loc.t] <- NA
  dataSet$Location_final[loc.t] <- dataSet$Location_A[loc.t]
  dataSet$Latitude_final[loc.t] <- dataSet$Latitude_A[loc.t]
  dataSet$Longitude_final[loc.t] <- dataSet$Longitude_A[loc.t]
  return(dataSet)
}

# CHANGE 2b same as A but the location code of  SCoBC2023 is used
f.keep.KBB2020.with.SCoBC2023.code.name <- function(KBB2020.location.name, dataSet){
  # get row positions in the dataframe 
  loc.t <- which(dataSet$Location_A == KBB2020.location.name)
  dataSet$Notes[loc.t] <- "name and coordinates set to KBB2020"
  dataSet$Location_Code_final[loc.t] <- dataSet$Location_Code[loc.t]
  dataSet$Location_final[loc.t] <- dataSet$Location_B[loc.t]
  dataSet$Latitude_final[loc.t] <- dataSet$Latitude_A[loc.t]
  dataSet$Longitude_final[loc.t] <- dataSet$Longitude_A[loc.t]
  return(dataSet)
}


# CHANGE 3 - keep KBB2020 name, use SCoBC2023 coordinates
f.keep.KBB2020.name.only <- function(KBB2020.location.name, dataSet){
  loc.t <- which(dataSet$Location_A == KBB2020.location.name)
  dataSet$Notes[loc.t] <- "name KBB2020, coordinates set to SCoBC2023"
  dataSet$Location_Code_final[loc.t] <- NA
  dataSet$Location_final[loc.t] <- dataSet$Location_A[loc.t]
  dataSet$Latitude_final[loc.t] <- dataSet$Latitude_B[loc.t]
  dataSet$Longitude_final[loc.t] <- dataSet$Longitude_B[loc.t]
  # not really needed to return but added for clarity
  return(dataSet)
}

# CHANGE 4 - change entire KBB2020 location into a new SCoBC2023 location+coord
# NOTE uses reference.loc dataset
f.change.KBB2020.to.new.SCoB2023 <- function(KBB2020.name, new.SCoB2023.name, refDataset, dataSet){
  #KBB2020.name <- "Valdes Island"
  #new.SCoB2023.name <- "Valdes Island - West Cliffs"
  #refDataset <- reference.loc
  #dataSet <- matched_pairs_corr
     
  new.loc <- which(refDataset$Location == new.SCoB2023.name)
  new.row <- refDataset[new.loc,]
  # location KBB2020 Valdes Island 
  loc.t <- which(dataSet$Location_A == KBB2020.name)
  #matched_pairs_corr[loc.t,]
  dataSet$Location_Code_final[loc.t] <- new.row$Site.Code 
  dataSet$Location_final[loc.t] <- new.row$Location
  dataSet$Latitude_final[loc.t] <- new.row$Lat
  dataSet$Longitude_final[loc.t] <- new.row$Long
  dataSet$Notes[loc.t] <- c("wrongly named in KBB2020 and changed to new SCoB2023 location")
  return(dataSet)
}

# CHANGE 5 - change to SCoBC2023 : no action needed as this is default

# helper functions to show the entry in the dataset
show.Blue <- function(BlueName){
  loc.t <- which(matched_pairs_corr$Location_A == BlueName)
  matched_pairs_corr[loc.t,]
}
#show.Blue("Little Group Islets")

show.Red <- function(RedName){
  loc.t <- which(matched_pairs_corr$Location_B == RedName)
  matched_pairs_corr[loc.t,]
}
#show.Red("Little Group Islets")


# use the map code below to try to resolve the coordinates
# with large differences between them. Focus on reference location as Louise 
# has split some in multiple
# *** working clockwise from south end to check sites


# Coburg Peninsula --> keep Sooke sites (remove later)
matched_pairs_corr <- f.keep.KBB2020.ref("Coburg Peninsula", matched_pairs_corr)

# "VictoriaBBS" --> keep KBB2020 site
matched_pairs_corr <- f.keep.KBB2020("VictoriaBBS", matched_pairs_corr)

# - Chain Islets/Great Chain Island - is colony at KBB2020's location?
#     use SCob coordinates

# - Mandarte Island -> Mandarte & Mandarte South split -> check Mandarte coord Louise but keep split
matched_pairs_corr <- f.keep.KBB2020("Mandarte South Islet", matched_pairs_corr)
# Mandarte main island will take the SCoBC2023 automatically

# - Little group (2 points KBB2020) and on Dock Isl in SCoBC2023
# change Little Group Islets to KBB2020
# UPDATE keep it simple and lump all under Little group
#matched_pairs_corr <- f.keep.KBB2020.with.SCoBC2023.code.name("Little Group", matched_pairs_corr)
# for KBB2020 Dock Island, change coord SCoBC2023 
#matched_pairs_corr <- f.keep.KBB2020.name.only("Dock Island", matched_pairs_corr)

# - Channel Islands --> not sure: check after survey for now stick to SCoB2023
   
# - Java Islets -> gulls all over: keep SCoBC. 

# - Saturna and East-point cliffs -> the right site (it is not island)
#   check where the colony is and use those coordinates at East Point
#   for now keep SCoB2023
# Belle Chain Islets --> keep the two KBB2020 entries
matched_pairs_corr <- f.keep.KBB2020.with.SCoBC2023.code.name("Belle Chain Islets", matched_pairs_corr)
matched_pairs_corr <- f.keep.KBB2020("Anniversary Islet", matched_pairs_corr)

# - Lion Islets --> change to CSoB2023

# - KBB2020 Galiano Island site --> change to SCoBC2023 cliff name
#   --> take SCoB2023, have to search the whole area

# - check KBB2020 Wallace Island site --> change to SCoB 
#   nests were on islet in the 80's: check around. 
 
# -  KBB2020 Tent Island site 
#   keep  SCoB for Tent. Kuper for KBB2020
matched_pairs_corr <- f.keep.KBB2020("Kuper Island", matched_pairs_corr)

# - Whaleboat -> Vermeer says unnamed off Ruxton -> prob Whaleboat
# change to SCoB 

# - KBB2020 Valdes Island -> SCoB2023 Valdes Island - West Cliffs (crossref #'s)
matched_pairs_corr <- f.change.KBB2020.to.new.SCoB2023("Valdes Island", "Valdes Island - West Cliffs",reference.loc, matched_pairs_corr)
matched_pairs_corr
matched_pairs_corr[which(matched_pairs_corr$Location_A == "Valdes Island"), ]
   

# -  KBB2020 De Courcy Island site and SCoB2023 Link Island
#   two different sites   
matched_pairs_corr <- f.keep.KBB2020("De Courcy Island", matched_pairs_corr)


# !!!!! Link has code 140

# - where are KBB2020 Gabriola Island nests located?
#   --> change KBB2020 to SCoB2023 "Gabriola Island - West Cliffs" 
matched_pairs_corr <- f.change.KBB2020.to.new.SCoB2023("Gabriola Island", "Gabriola Island - West Cliffs", reference.loc, matched_pairs_corr)
matched_pairs_corr
matched_pairs_corr[which(matched_pairs_corr$Location_A == "Gabriola Island"), ]

# - KBB2020 Inskip Rock should not be SCoBC2023 Brandon Islands
matched_pairs_corr <- f.keep.KBB2020( "Inskip Rock", matched_pairs_corr)

# - Ada Islands: KBB2020 coords South Winchelsea Isl
#   change to SCoB

# Yeo Islands -> should remain KBB20202 Nanoose Bay Island
matched_pairs_corr <- f.keep.KBB2020("Nanoose Bay Island", matched_pairs_corr)

# - Ballenas Isl coord KBB2020 in the sea
#  --> change to SCoB


# - is Hornby Island KBB2020 same as St. John Point
#  --> KBBB2020  Bluffs as own location
matched_pairs_corr <- f.keep.KBB2020("Hornby Island Bluffs", matched_pairs_corr)

# Toby Island in KBB2020 should be a new site
matched_pairs_corr <- f.keep.KBB2020("Toby Island", matched_pairs_corr)

#  KBB2020 Dinner Islet should not be SCoBC2023's Major 
matched_pairs_corr <- f.keep.KBB2020( "Dinner Islet", matched_pairs_corr)

# - location Hodgson Islands KBB2020 is on Pearson Isl --> keep KBB2020 name
matched_pairs_corr <- f.keep.KBB2020( "Hodgson Islands", matched_pairs_corr)

# KBB2020 Whyte Islet should get coordinates of SCoB2023 Whyte Islet
matched_pairs_corr <- f.change.KBB2020.to.new.SCoB2023("Whyte Islet", "Whyte Islet", reference.loc, matched_pairs_corr)

#  keep KBB2020 Eagle Harbour for now
matched_pairs_corr <- f.keep.KBB2020( "Eagle Harbour", matched_pairs_corr)

# Prospect Point
# Vancouver Lions Gate Bridge should be new site
matched_pairs_corr <- f.keep.KBB2020( "Vancouver Lions Gate Bridge", matched_pairs_corr)


# show the map with final results. Note that red SCoBC2023 locations may now be 
# include KBB2020 locations; red is the 'final' location.
f.map.matched.pairs(matched_pairs_corr)

colnames(matched_pairs_corr) 
# new column names to make clear that A is KBB2020 data and B is the reference 
# from SCoBC2023
names.adj <- c("Location_A_KBB2020", 
               "Location_Code_SCoBC2023", 
               "Location_B_SCoBC2023")
colnames(matched_pairs_corr)[1:3] <- names.adj

# as final step save the cleaned file of the matched pairs
write.csv(matched_pairs_corr, paste(p.data.clean, "KBB2020.table.coord.checked.csv", 
                               sep = ""), row.names = F)


# //////////////////////////////////////////////////////////////////////////////
# === E adjust KBB2020 coordinates where needed ====
KBB2020.coord <- read.csv(paste(p.data.clean, "KBB2020.table.coord.checked.csv", sep ="") )
str(KBB2020.data)
str(KBB2020.coord)
KBB2020.data.coord.corr <- KBB2020.data
colnames(KBB2020.data)
colnames(KBB2020.coord)

merged_data <- merge(KBB2020.data, KBB2020.coord, by.x = "Location", 
                     by.y = "Location_A_KBB2020", all.x = TRUE)
head(merged_data)
colnames(merged_data)
merged_final <- merged_data[,c(1:15,21:26)]
colnames(merged_final)
is.na(merged_final$Location_Code_final)

# - remove 6 sites on Sooke / south end of island
rm.Sooke.sites <- c("Sooke Bay", "Argyle Rocks", "South Bedford Island", 
                    "Church Island", "Race Rocks all", "North Race Rock")
matching_rows <- which(merged_final$Location %in% rm.Sooke.sites)
merged_final[matching_rows,]
# remove sites 
merged_final <- merged_final[-matching_rows,]


#
write.csv(merged_final, paste(p.data.clean, "KBB2020.data.corrected.csv", 
                                    sep = ""), row.names = F)


# //////////////////////////////////////////////////////////////////////////////
# === G combine KBB2020 data with SCoBC2023 table 6 long ====
KBB2020.corr <- read.csv(paste(p.data.clean,"KBB2020.data.corrected.csv", 
                               sep ="") )
colnames(KBB2020.corr)
head(KBB2020.corr)
SCoBC2023.corr <- read.csv(paste(p.data.clean,"SCoBC2023_table6_long_clean.csv", 
                               sep ="") )

# make selection of KBB2020.corr to minimal and add source to both
SCoBC2023.corr$Source <- rep("SCoBC2023", nrow(SCoBC2023.corr))
SCoBC2023.corr <- SCoBC2023.corr[,-2] # delete the Region

colnames(SCoBC2023.corr)

KBB2020.corr.subset <- subset(KBB2020.corr, select = c("Location_Code_final",
                                "Location_final",
                                "Latitude_final",
                                "Longitude_final",
                                "Year",
                                "Count"))
KBB2020.corr.subset$Source <- rep("KBB2020", nrow(KBB2020.corr.subset))
# set names of KBB2020.corr.subset to be the same as SCoBC2023.corr
colnames(KBB2020.corr.subset) <- colnames(SCoBC2023.corr)

# add to KBB2020.corr.subset data from SCoBC2023.corr which is not present
# base this on the "ID" 

# * combine both dataframes so that entries "ID" which are unique to 
# SCoBC2023.corr are added to KBB2020.corr.subset and saved as a new dataframe

# Identify the IDs in SCoBC2023.corr that are not present in KBB2020.corr.subset
new_IDs <- setdiff(SCoBC2023.corr$ID, KBB2020.corr.subset$ID)

# Filter rows from SCoBC2023.corr based on the new IDs
new_rows <- SCoBC2023.corr[SCoBC2023.corr$ID %in% new_IDs, ]

# Create a new dataframe by appending the new rows to KBB2020.corr.subset
KBB2020andSCoBC2023.combined <- rbind(KBB2020.corr.subset, new_rows)

write.csv(KBB2020andSCoBC2023.combined, paste(p.data.clean, 
          "KBB2020andSCoBC2023combined.csv", sep = ""), row.names = F)

# //////////////////////////////////////////////////////////////////////////////





#___ end _______________________________________________________________________



