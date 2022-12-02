##### load_clinic_catchment_areas.R #####
# Writes two shapefiles (25-mi map distance and 30min driving distance)
# catchement areas for the clinics

library(tidyverse)
library(sf)
rm(list = ls())
setwd("~/BIMI_SAI")

# Load OSRM function
source("er_osrmIsochrone.R")

# Loading data
message("Loading data")
load("data/clinics_preload.Rdata")


# Project objects to NAD83 (in meters)
message("Projecting to NAD83")
# st_crs(clinics_preload) <-4326
proj4 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=mi +no_defs"
clinics_preload <- st_transform(clinics_preload, st_crs(proj4))

message("Creating distance buffers")




# 12 mi
message("Creating 12mi buffers")
clinics_12mi_buffers <- clinics_preload %>% st_buffer(dist = units::set_units(12, mi))
save(clinics_12mi_buffers, file = "data/clinics_12mi_buffers.Rdata")

# Add driving time buffers

#
# Note: osrm-routed needs to be running in order for this to work. On keyfitz,
# run:
#
# cd ~/90days/eroubenoff/osrm
# tmux
# osrm-routed us-west-latest.osm
# ctrl-b d
#
# This will run an osrm-routed server in the background.



message("Creating Driving Time buffers")
library(osrm)
library(tictoc)
library(tmap)
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")


# Parallelizing 
# Also runs on er_osrmIsochrone
# Note that the osrm instance can only do one at a time, but the bottleneck
# seems to be on turning the osrm output into an r object


library(parallel)
detectCores()

buffer_time <- 30 

fx <- function(i) {
  message(paste("Running clinic", i))
  tryCatch({
    return(st_set_geometry(clinics_preload[i,],
                    er_osrmIsochrone(
                      loc = clinics_preload[i,],
                      breaks = c(buffer_time),
                      res = buffer_time,
                      returnclass =  "sf") %>% 
                      filter(max == buffer_time) %>% 
                      st_geometry()))
  }, error = function(e) {
    message(paste("Failure at clinic", i, ":", e))
    print(clinics_preload[i, ])
    return(clinics_preload[i, ])
  })
}

starts <- 1:nrow(clinics_preload)
# starts <- 1:20

system.time(
  clinics_driving_buffers <- mclapply(starts, fx, mc.cores = 10)
)




clinics_driving_buffers <- do.call(rbind, clinics_driving_buffers)
# qtm(clinics_driving_buffers)
save(clinics_driving_buffers, file = "data/clinics_driving_buffers.Rdata")

