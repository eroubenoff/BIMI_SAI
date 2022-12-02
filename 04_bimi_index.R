
# There is a new version of the Process script that considers the "exclude" column
# This version of the script accounts for that

library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
rm(list = ls())


##### 1: Load buffers #####
load("data/clinics_preload.Rdata")
load("data/clinics_12mi_buffers.Rdata")

tract_data <- read_csv("data/msi_tractdata_acs2018.csv") %>% rename(geometry = WKT) %>% st_as_sf(wkt = "geometry", crs = 4326)

load("data/clinics_driving_buffers.Rdata")
clinics_30min_buffers <- clinics_driving_buffers

load("data/clinics_driving_buffers_15min.Rdata")
clinics_15min_buffers <- clinics_driving_buffers

load("data/clinics_driving_buffers_60min.Rdata")
clinics_60min_buffers <- clinics_driving_buffers

# We have 4 buffer objects:
clinic_buffers <- list("12mi" = clinics_12mi_buffers,
                       "15min" = clinics_15min_buffers,
                       "30min" = clinics_30min_buffers,
                       "60min" = clinics_60min_buffers)
rm(clinics_12mi_buffers, clinics_30min_buffers)


# Load tract buffers
load("data/tracts_12mi_buffers.Rdata")
load("data/tracts_driving_buffers.Rdata")
tracts_30min_buffers <- tracts_driving_buffers
load("data/tracts_driving_buffers_15min.Rdata")
tracts_15min_buffers <- tracts_driving_buffers
load("data/tracts_driving_buffers_60min.Rdata")
tracts_60min_buffers <- tracts_driving_buffers

tracts_buffers <- list("AFi_12mi" = tracts_12mi_buffers,
                       "AFi_15min" = tracts_15min_buffers,
                       "AFi_30min" = tracts_30min_buffers,
                       "AFi_60min" = tracts_60min_buffers
                       )





# Reproject
proj4 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
clinic_buffers <- lapply(clinic_buffers, function(x) st_transform(x , proj4))
tract_data <- tract_data %>% st_set_crs(4326) %>% st_transform(proj4)
clinics_preload <- clinics_preload  %>% st_set_crs(4326) %>% st_transform(proj4)

# reproject the tracts_buffers into the crs of clinic_buffers
tracts_buffers <- map(tracts_buffers, ~st_transform(., st_crs(proj4)))

##### Get the variables in order #####
pop_vars  = c("p_noncit_count", 
              "p_fb_pov_count", 
              "p_fb_nohi_count")

tract_data <- tract_data %>% mutate_at(vars(all_of(pop_vars)), ~as.numeric(.))



##### Run the intersect Operations ######
# Run the intersect operations: takes maybe 10 minutes

calc_clinic_rj <- function(b, tract_data, pop_vars) {
  message(paste("Buffering"))
  
  # Select the clinic ID (and implicitly the geometry) from each buffer
  buffer <- b %>% select(index, sj, type)
  
  # Select the correct population variable and calculate the tract total area
  pop <- tract_data %>% select(all_of(pop_vars)) %>% mutate(tot_area = st_area(tract_data))
  
  # Run the intersect operation 
  clinics_intersect <- st_intersection(pop, buffer) # This is also the slow line

  
  # Add intersection area 
  clinics_intersect$intersect_area <- clinics_intersect %>% st_area()
  
  # Calculate proportion overlap
  clinics_intersect <- clinics_intersect %>% mutate(area_prop = intersect_area/tot_area)
  hist(clinics_intersect$area_prop)
  
  # Proportion of tracts contained 100% within the buffer:
  sum(as.numeric(clinics_intersect$area_prop) == 1) / nrow(clinics_intersect)
  
  clinics_out <- clinics_intersect %>%
    group_by(index, sj, type) %>%
    mutate_at(vars(-group_cols(), -geometry), as.numeric) %>%
    summarize(p_noncit_count = sum(p_noncit_count * area_prop),
              p_fb_pov_count = sum(p_fb_pov_count * area_prop),
              p_fb_nohi_count = sum(p_fb_nohi_count * area_prop),
              geometry = st_union(geometry) # This is the slow line
    )
  

  
  # Calculate the ratio
  clinics_out <- clinics_out %>% mutate_at(vars(all_of(pop_vars)), ~sj/.) %>%
    rename(
      Rj_p_noncit_count = p_noncit_count,
      Rj_p_fb_pov_count = p_fb_pov_count,
      Rj_p_fb_nohi_count = p_fb_nohi_count,
    )
  
  # Replace infinite ratios with 0
  clinics_out <-  clinics_out %>% mutate_at(vars(-type, -index, -geometry), ~ifelse(is.infinite(.), 0, .))
  b <- b %>% left_join(clinics_out %>% st_drop_geometry(), by = c("index", "type", "sj"))
  return(b)
}

# for (b in 1:length(clinic_buffers)){ 
for (b in c(1:4)){ 
  clinic_buffers[[b]] <- calc_clinic_rj(clinic_buffers[[b]], tract_data, pop_vars)
}


# Replace clinic buffer geometry with clinic point location
clinic_buffers <- map(clinic_buffers, ~st_set_geometry(., st_geometry(clinics_preload)))


##### 5: Total up by tract buffer #####



# Total up each Rj variable within each buffer
calc_afi <- function(tract_buffer, clinics, tract_data) {
    message("buffering")
  
    buffer <- tract_buffer
    
    
    health_list <- st_join(buffer, clinics %>% filter(type == "health")) %>% 
      group_by(STATEFP, COUNTYFP, GEOID) %>%
      summarize_at(vars(contains("Rj")), ~sum(., na.rm = TRUE)) 
    
    legal_list <- st_join(buffer, clinics %>% filter(type == "legal")) %>% 
      group_by(STATEFP, COUNTYFP, GEOID) %>%
      summarize_at(vars(contains("Rj")), ~sum(., na.rm = TRUE))
    
    
    health_list <- health_list %>% 
      ungroup() %>%
      rename_at(vars(contains("Rj_")), ~str_replace(., "Rj_", "health_AFi_")) %>%
      st_drop_geometry()
    
    legal_list <- legal_list %>% 
      ungroup() %>%
      rename_at(vars(contains("Rj_")), ~str_replace(., "Rj_", "legal_AFi_")) %>%
      st_drop_geometry()
    
    
    AFi_list <- left_join(health_list, legal_list, by = c("STATEFP", "COUNTYFP", "GEOID"))
    
    AFi_list <- AFi_list %>% mutate(GEOID = paste0(STATEFP, COUNTYFP, GEOID)) %>%
      select(-COUNTYFP, -STATEFP)
    
    return(left_join(
      tract_data,
      AFi_list,
      by = "GEOID"))
    

}

# for (b in 1:length(tracts_buffers)) {
AFi <- list()
for (b in c(1:4)) { 
 AFi[[b]] <- calc_afi(tract_buffer = tracts_buffers[[b]], 
                                  # clinics_preload = clinics_preload, 
                                  clinics = clinic_buffers[[b]], 
                                  tract_data = tract_data)
}


##### 6: Create output object #####
names(AFi) <- c("AFi_12mi", "AFi_15min", "AFi_30min","AFi_60min")
attach(AFi)
save(list = c("AFi_12mi", "AFi_15min", "AFi_30min", "AFi_60min"),
     file = "data/SAI_index_2stepFCA_allvars.Rdata")


detach(AFi)







