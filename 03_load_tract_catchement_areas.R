##### load_catchment_areas.R #####
# Writes two shapefiles (12-mi map distance and 30min driving distance)
# catchement areas for the tracts


library(tidyverse)
library(sf)
library(osrm)
library(tictoc)
library(tmap)
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")


# Load OSRM function
source("er_osrmIsochrone.R")

# Buffers for census tracts
message("Buffering census tracts")
library(tigris)


# Use population-weighted centroids
census_data <- rbind(
  curl::curl("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR06.txt") %>% read_csv(),
  curl::curl("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR04.txt") %>% read_csv(),
  curl::curl("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR32.txt") %>% read_csv()
) 
census_data <- census_data %>% rename(GEOID = TRACTCE)
census_data <- census_data %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 


# Project objects to NAD83 (in meters)
message("Projecting to NAD83")
proj4 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=mi +no_defs"
census_data <- st_transform(census_data, proj4)

message("Creating distance buffers")

# Add buffers (12 mi)
message("Creating 12mi buffers")
tracts_12mi_buffers <- census_data %>% st_buffer(dist = units::set_units(12, mi))
save(tracts_12mi_buffers, file = "data/tracts_12mi_buffers.Rdata")



# Now doing driving time
library(parallel)
detectCores()

buffer_time <- 30 

fx <- function(i) {
  message(paste("Running tract", i))
  tryCatch({
    return(st_set_geometry(census_data[i,],
                           er_osrmIsochrone(
                             loc = census_data[i,],
                             breaks = c(buffer_time),
                             res = buffer_time,
                             returnclass =  "sf") %>% 
                             filter(max == buffer_time) %>% 
                             st_geometry()))
  }, error = function(e) {
    message(paste("Failure at clinic", i, ":", e))
    print(census_data[i, ])
    return()
  })
}


starts <- 1:nrow(census_data)

system.time(
  tracts_driving_buffers <- mclapply(starts, fx, mc.cores = 10)
)
tracts_driving_buffers <- data.table::rbindlist(tracts_driving_buffers) %>% st_as_sf

save(tracts_driving_buffers, file = "data/tracts_driving_buffers.Rdata")





