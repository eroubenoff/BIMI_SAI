# Speeding up the catchment area calculations using rcpp
library(tidyverse)
library(sf)
library(Rcpp)
library(osrm)
library(tictoc)
library(tmap)
# rm(list = ls())
# setwd("~/bimi_index")



er_osrmIsochrone <- function(loc, breaks = c(30), exclude = NULL, res = 30, returnclass = "sf"){
  
  
  
  isopoly <- function(x, breaks, 
                      xcoords = "COORDX", ycoords = "COORDY", var = "OUTPUT"){
    
    # get initial min and max values
    vmin <- min(x[[var]], na.rm = TRUE)
    vmax <- max(x[[var]], na.rm = TRUE)
    breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
    # data points to matrix
    m <- matrix(data = x[[var]], nrow = length(unique(x[[xcoords]])), 
                dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]])))
    # compute isobands
    lev_low = breaks[1:(length(breaks)-1)]
    lev_high = breaks[2:length(breaks)]
    raw <- isoband::isobands(x = as.numeric(rownames(m)), 
                             y = as.numeric(colnames(m)), z = t(m), 
                             levels_low = lev_low,
                             levels_high = c(lev_high[-length(lev_high)], 
                                             vmax + 1e-10))
    bands <- isoband::iso_to_sfg(raw)
    iso <- st_sf(id = 1:length(bands), 
                 min = lev_low, 
                 max = lev_high,
                 geometry = sf::st_sfc(bands), 
                 crs = st_crs(x))
    iso[1,"min"] <- 0
    iso$center = iso$min + (iso$max - iso$min) / 2
    
    # invalid polygons mgmnt
    st_geometry(iso) <- st_make_valid(st_geometry(iso))  
    
    
    if(methods::is(st_geometry(iso),c("sfc_GEOMETRYCOLLECTION", "sfc_GEOMETRY"))){
      st_geometry(iso) <-   sf::st_collection_extract(st_geometry(iso), "POLYGON")
    }
    # get rid of out of breaks polys
    iso <- iso[-nrow(iso),]
    return(iso)
  }
  
  
  rgrid <- function(loc, dmax, res){
    # create a regular grid centerd on loc
    boxCoordX <- seq(from = sf::st_coordinates(loc)[1,1] - dmax,
                     to = sf::st_coordinates(loc)[1,1] + dmax,
                     length.out = res)
    boxCoordY <- seq(from = sf::st_coordinates(loc)[1,2] - dmax,
                     to = sf::st_coordinates(loc)[1,2] + dmax,
                     length.out = res)
    sgrid <- expand.grid(boxCoordX, boxCoordY)
    sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1),
                        COORDX = sgrid[, 1],
                        COORDY = sgrid[, 2])
    sgrid <- sf::st_as_sf(sgrid,  coords = c("COORDX", "COORDY"),
                          crs = st_crs(loc), remove = FALSE)
    return(sgrid)
  }
  
  oprj <- st_crs(loc)
  loc <- loc[1,]
  loc <- st_transform(loc, 3857)
  row.names(loc) <- "0"
  
  # max distance mngmnt to see how far to extend the grid to get measures
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if(options('osrm.profile')=="walk"){
    speed =  10 * 1000/60
  }
  if(options('osrm.profile')=="bike"){
    speed =  20 * 1000/60
  }
  if(options('osrm.profile')=="driving"){
    speed =  130 * 1000/60
  }
  dmax <- tmax * speed
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  
  # dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude)
  dmat <- osrmTable(src = loc, dst = sgrid)
  durations <- dmat$durations
  destinations <- dmat$destinations
  
  rpt <- st_as_sf(destinations, coords = c('lon', 'lat'), crs = 4326)
  rpt <- st_transform(rpt, st_crs(loc))
  rpt$durations <- do.call(c, as.list(durations))
  
  
  b <- as.numeric(st_distance(sgrid[1,], sgrid[2,]) / 2)
  xx <- st_as_sf(st_make_grid(x = st_buffer(st_as_sfc(st_bbox(sgrid)), b), 
                              n = c(res, res)), sf_column_name = "x")
  xx$id <- 1:nrow(xx)
  inter <- st_join(xx, rpt)
  inter <- suppressMessages({suppressWarnings({left_join(xx,
            summarize(
              group_by(
                st_drop_geometry(inter), 
                id), 
              durations = mean(durations, na.rm = TRUE)))})})
  # There are some duplicated geometries.  Need to combine and average them
  # This could be sped up
  # inter <- inter %>% group_by(id) %>%
  #   summarize(durations = mean(durations, na.rm = TRUE)) 
  
  
  # sgrid$durations <- unlist(lapply(inter, function(x)mean(rpt[["durations"]][x], na.rm=TRUE)))
  sgrid$durations <- inter$durations
  # browser()
  sgrid[is.nan(sgrid$durations), "durations"] <- tmax + 1
  sgrid[is.na(sgrid$durations), "durations"] <- tmax + 1
  sgrid[sgrid$durations > tmax, "durations"] <- tmax + 1
  if(min(sgrid$durations) > tmax){
    e <- "Use lower values for 'breaks' or increase 'res'"
    stop(e, call. = FALSE)
  }
  iso <- isopoly(x = sgrid, breaks = breaks, var = "durations")
  # proj mgmnt
  if (!is.na(oprj)){
    iso <- st_transform(x = iso, oprj)
  }else{
    iso <- st_transform(x = iso, 4326)
  }
  
  return(iso)
  
  
  }

# i <- 1
# 
# tm_shape(
#   er_osrmIsochrone(
#     loc = clinics_preload[i,],
#     breaks = c(30),
#     res = 60,
#     returnclass =  "sf")) + 
#   tm_polygons() + 
#   tm_shape(
#     osrmIsochrone(
#       loc = clinics_preload[i,],
#       breaks = c(30),
#       res = 60,
#       returnclass =  "sf") 
#   ) + 
#   tm_polygons()
# 
# for(i in 1:10) {
#   er_osrmIsochrone(
#     loc = clinics_preload[i,],
#     breaks = c(30),
#     res = 60,
#     returnclass =  "sf")}
# 
# # undebug(er_osrmIsochrone)
# library(microbenchmark)
# microbenchmark(
#   er = {for(i in 1:10) {
#     er_osrmIsochrone(
#       loc = clinics_preload[i,],
#       breaks = c(30),
#       res = 60,
#       returnclass =  "sf")}
#     },
#   osrm = {for(i in 1:10) {
#     osrmIsochrone(
#       loc = clinics_preload[i,],
#       breaks = c(30),
#       res = 60,
#       returnclass =  "sf")}
#    },
#   times = 1
# )
