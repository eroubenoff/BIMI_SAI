library(tmap)
library(tidyverse)
library(sf)
library(tmaptools)
library(tigris)
library(tidycensus)
library(cowplot)
theme_set(theme_cowplot(10))

tmap_mode("plot")

load("data/clinics_preload.Rdata")
load("data/SAI_index_2stepFCA_allvars.Rdata")
# load("data/tract_data_withvars.Rdata")
# st_crs(clinics_preload) <- 4326
clinics_preload <- st_transform(clinics_preload, st_crs(AFi_30min))

# Summary stats
counties_list <- st_join(clinics_preload,
  tigris::counties(state = c("CA", "NV", "AZ")) %>% st_transform(st_crs(clinics_preload))) %>%
  st_drop_geometry() %>% 
  select(NAME, STATEFP)

counties_list %>%  
  group_by(STATEFP) %>% 
  select(NAME) %>% 
  distinct(NAME) %>% 
  summarize(n = n())

tigris::counties(state = c("CA", "NV", "AZ")) %>% st_transform(st_crs(clinics_preload)) %>% st_drop_geometry() %>%  group_by(STATEFP) %>% summarize(n = n())

clinics_preload %>% st_drop_geometry() %>% group_by(type) %>% summarize(n = n())


# Clinic Locations 
states_ <- tigris::states() %>% filter(NAME %in% c("California", "Arizona", "Nevada"))

tmap_arrange(
  tm_shape(states_) + 
    tm_borders() + 
    tm_shape(clinics_preload %>% filter(type == 'health')) + 
    tm_dots(size=0.05, alpha = 0.6),
  tm_shape(states_) + 
    tm_borders() + 
    tm_shape(clinics_preload %>% filter(type == 'legal')) + 
    tm_dots(size=0.05, alpha = 0.6),
  ncol = 2
) %>%
  tmap_save('figs/clinics.png', height = 4, width=7)




tigris::counties(state = c("CA", "NV", "AZ")) %>% st_transform(st_crs(clinics_preload)) %>% st_drop_geometry() %>% pull(NAME) %>% unique() %>% length

bay_area_counties <- c("Alameda", "Contra Costa", "Solano", "Napa", "Sonoma", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Monterey", "Santa Cruz", "San Benito" )

southern_california <- c("Ventura", "Los Angeles", "Orange", "Riverside", "San Bernardino", "San Diego", "Imperial", "San Luis Obispo", "Santa Barbara")

central_valley_counties <- c("Sacramento", "San Joaquin", "Stanislaus", "Merced", "Madera", "Fresno", "Tulare", "Kings", "Kern", "Yolo", "Sutter", "Yuba", "Butte")

mountain_california <- c("Del Norte", "Siskiyou", "Modoc", "Humboldt", "Trinity", "Shasta", "Lassen", "Tehama", "Plumas", "Mendocino", "Lake", "Glenn","Sierra", "El Dorado", "Amador", "Alpine", "Calaveras", "Tuolumne", "Mono", "Mariposa", "Inyo", "Colusa", "Nevada", "El Dorado", "Placer")

AFi_30min <- AFi_30min %>% mutate(
  STATE = str_extract(NAME, "(?<=County, )(.*)"),
  COUNTY = str_extract(NAME, "(?<=, )(.*)(?= County)"),
  region = case_when(
    STATE == "Arizona" ~ "Arizona",
    STATE == "Nevada" ~ "Nevada",
    COUNTY %in% bay_area_counties ~ "bay_area",
    COUNTY %in% central_valley_counties ~ "central_valley",
    COUNTY %in% southern_california ~ "southern_california",
    COUNTY %in% mountain_california ~ "mountain"
  )) 

AFi_12mi <- AFi_12mi %>% mutate(
  STATE = str_extract(NAME, "(?<=County, )(.*)"),
  COUNTY = str_extract(NAME, "(?<=, )(.*)(?= County)"),
  region = case_when(
    STATE == "Arizona" ~ "Arizona",
    STATE == "Nevada" ~ "Nevada",
    COUNTY %in% bay_area_counties ~ "bay_area",
    COUNTY %in% central_valley_counties ~ "central_valley",
    COUNTY %in% southern_california ~ "southern_california",
    COUNTY %in% mountain_california ~ "mountain"
  )) 

AFi_15min <- AFi_15min %>% mutate(
  STATE = str_extract(NAME, "(?<=County, )(.*)"),
  COUNTY = str_extract(NAME, "(?<=, )(.*)(?= County)"),
  region = case_when(
    STATE == "Arizona" ~ "Arizona",
    STATE == "Nevada" ~ "Nevada",
    COUNTY %in% bay_area_counties ~ "bay_area",
    COUNTY %in% central_valley_counties ~ "central_valley",
    COUNTY %in% southern_california ~ "southern_california",
    COUNTY %in% mountain_california ~ "mountain"
  )) 


AFi_60min <- AFi_60min %>% mutate(
  STATE = str_extract(NAME, "(?<=County, )(.*)"),
  COUNTY = str_extract(NAME, "(?<=, )(.*)(?= County)"),
  region = case_when(
    STATE == "Arizona" ~ "Arizona",
    STATE == "Nevada" ~ "Nevada",
    COUNTY %in% bay_area_counties ~ "bay_area",
    COUNTY %in% central_valley_counties ~ "central_valley",
    COUNTY %in% southern_california ~ "southern_california",
    COUNTY %in% mountain_california ~ "mountain"
  )) 


# Export data for GIS mapping 
AFi_30min <- AFi_30min %>% 
  select(GEOID, SAIhealth = health_AFi_p_fb_nohi_count, SAIlegal = legal_AFi_p_noncit_count, COUNTY, STATE, region, tract_totpop, area = p_land_sqmiles) %>% 
  # Right censor data to 95% quantile
  mutate_at(vars(SAIhealth, SAIlegal), ~if_else(. > quantile(., probs = .99, na.rm = TRUE),quantile(., probs = .99, na.rm = TRUE), .)) 

AFi_15min <- AFi_15min %>% 
  select(GEOID,SAIhealth = health_AFi_p_fb_nohi_count, SAIlegal = legal_AFi_p_noncit_count, COUNTY, STATE, region, tract_totpop, area = p_land_sqmiles) %>% 
  # Right censor data to 95% quantile
  mutate_at(vars(SAIhealth, SAIlegal), ~if_else(. > quantile(., probs = .99, na.rm = TRUE),quantile(., probs = .99, na.rm = TRUE), .)) 

AFi_60min <- AFi_60min %>% 
  select(GEOID, SAIhealth = health_AFi_p_fb_nohi_count, SAIlegal = legal_AFi_p_noncit_count, COUNTY, STATE, region, tract_totpop, area = p_land_sqmiles) %>% 
  # Right censor data to 95% quantile
  mutate_at(vars(SAIhealth, SAIlegal), ~if_else(. > quantile(., probs = .99, na.rm = TRUE),quantile(., probs = .99, na.rm = TRUE), .)) 

AFi_30min %>%
  st_write(dsn = "data/SAI_shp/SAI.shp", layer = "SAI", delete_dsn = TRUE)

AFi_15min %>%
  st_write(dsn = "data/SAI_shp/SAI_15min.shp", layer = "SAI", delete_dsn = TRUE)

AFi_60min %>%
  st_write(dsn = "data/SAI_shp/SAI_60min.shp", layer = "SAI", delete_dsn = TRUE)

clinics_preload %>%
  rename_with(~str_replace_all(.,"_", "")) %>%
  st_join(AFi_30min %>% select(region)) %>%
  st_write(dsn = "data/clinics_shp/clinics_shp.shp", layer = "clinics", delete_dsn = TRUE)





# Generate summary tables 
AFi_30min %>% 
  st_drop_geometry() %>% 
  select(SAIhealth, SAIlegal, COUNTY, STATE, region) %>%
  filter( region == "bay_area") %>% 
  group_by(COUNTY) %>%
  summarize(`SAI Legal Mean` = mean(SAIlegal, na.rm = TRUE),
            `SAI Legal sd` = sd(SAIlegal, na.rm = TRUE),
            `SAI Health Mean` = mean(SAIhealth, na.rm = TRUE),
            `SAI Health sd` = sd(SAIhealth, na.rm = TRUE)
            )
  
gen_county_summaries <- function(region) {
  SAI <- AFi_30min %>% 
    st_drop_geometry() %>% 
    select(SAIhealth, SAIlegal, COUNTY, STATE, region) %>%
    mutate_at(vars(SAIhealth, SAIlegal), ~if_else(. > quantile(., probs = .99, na.rm = TRUE),quantile(., probs = .99, na.rm = TRUE), .)) %>%
    filter(region == {{region}})
  
 SAI_tbl <- SAI %>%  
    group_by(COUNTY) %>%
    summarize(`SAI Legal Mean` = mean(SAIlegal, na.rm = TRUE),
              `SAI Legal sd` = sd(SAIlegal, na.rm = TRUE),
              `SAI Health Mean` = mean(SAIhealth, na.rm = TRUE),
              `SAI Health sd` = sd(SAIhealth, na.rm = TRUE)
    )
  
  
 SAI_plot <- plot_grid(
    SAI %>% pivot_longer(cols = SAIhealth) %>%
      ggplot() +
      geom_boxplot(aes(COUNTY, value)) + 
      coord_flip() + 
      ylab("SAI Health") +
      xlab(""),
    SAI %>% pivot_longer(cols = SAIlegal) %>% 
      ggplot() +
      geom_boxplot(aes(COUNTY, value)) +
      coord_flip() + 
      ylab("SAI Legal") +
      xlab("") 
      ,
    nrow= 1
  )
 
 ggsave(paste0('figs/', region, "_summary.png"), width = 7, height = 5)
  
 return(list(SAI_tbl, SAI_plot))
}

invisible(gen_county_summaries("bay_area"))
invisible(gen_county_summaries("central_valley"))
invisible(gen_county_summaries("southern_california"))
invisible(gen_county_summaries("mountain"))
invisible(gen_county_summaries("Arizona"))
invisible(gen_county_summaries("Nevada"))


most_frequent <- function(codes){
  codes <- codes[!is.na(codes)]
  r <- names(which.max(table(codes)))
  
  if (is.null(r)) return(NA_character_) else return(r)
}

# Municipality level summaries and analysis by population size
places <- tidycensus::get_acs(table = "B01003", geography = "place", state = c("California", "Nevada", "Arizona"), year = 2018, geometry = TRUE) %>% st_transform(st_crs(AFi_30min))

places <- st_join(places, AFi_30min)  %>% st_drop_geometry() %>%
  select(place = NAME,
         total_pop = estimate, SAIhealth, 
         SAIlegal, COUNTY, STATE, region) %>%
  group_by(place) %>%
  summarize(total_pop = mean(total_pop),
            med_SAIhealth = mean(SAIhealth, na.rm = TRUE),
            med_SAIlegal = mean(SAIlegal, na.rm = TRUE),
            region = most_frequent(region)
            )

places <- places %>% mutate(region = case_when(
  region == "bay_area" ~ "Bay Area",
  region == "central_valley" ~ "Central Valley",
  region == "southern_california" ~ "Southern California",
  region == "mountain" ~ "Mountain",
  TRUE ~ region
))

# Plot SAI against total pop
places_plot <- plot_grid(
  ggplot(na.omit(places)) +
    geom_point(aes(total_pop, med_SAIhealth), size = 1) +
    facet_wrap(~region, ncol = 1) + 
    xlab("Total Population (Log Scale)") +
    ylab("Median Health SAI (Log Scale)") +
    # scale_y_log10() +
    scale_x_log10()
    ,
  ggplot(na.omit(places)) +
    geom_point(aes(total_pop, med_SAIlegal), size = 1) +
    facet_wrap(~region, ncol = 1) +
    xlab("Total Population (Log Scale)") +
    ylab("Median Legal SAI (Log Scale)") + 
    # scale_y_log10() +
    scale_x_log10()
    ,
  ncol = 2
)






ua <- tidycensus::get_acs(table = "B01003", 
                           geography = "urban area", 
                           year = 2018, geometry = TRUE) %>% st_transform(st_crs(AFi_30min))



most_frequent(c("a", "a", "b", "c", NULL))

ua <- st_join(ua, AFi_30min)  %>%  st_drop_geometry() %>%
  select(place = NAME, 
         total_pop = estimate, SAIhealth, 
         SAIlegal, COUNTY, STATE, region) %>%
  group_by(place) %>%
  summarize(total_pop = median(total_pop, na.rm = TRUE),
            med_SAIhealth = median(SAIhealth, na.rm = TRUE),
            med_SAIlegal = median(SAIlegal, na.rm = TRUE) ,
            region = most_frequent(region)
  ) %>% filter(!is.na(med_SAIhealth)) 

# ua %>% qtm(fill = "med_SAIhealth")

ua <- ua %>% mutate(region = case_when(
  region == "bay_area" ~ "Bay Area",
  region == "central_valley" ~ "Central Valley",
  region == "southern_california" ~ "Southern California",
  region == "mountain" ~ "Mountain",
  TRUE ~ region
)) %>%
  filter(!is.na(region))

# Plot SAI against total pop
ua_plot <- plot_grid(
  ggplot(na.omit(ua)) +
    geom_point(aes(total_pop, med_SAIhealth), size = 1) +
    facet_wrap(~region, ncol = 1) + 
    xlab("Total Population (Log Scale)") +
    ylab("Median Health SAI") +
    # ylim(0,1) + 
    # scale_y_log10() +
    scale_x_log10()
  ,
  ggplot(na.omit(ua)) +
    geom_point(aes(total_pop, med_SAIlegal), size = 1) +
    facet_wrap(~region, ncol = 1) +
    xlab("Total Population (Log Scale)") +
    ylab("Median Legal SAI") + 
    # ylim(0,.1) + 
    # scale_y_log10() +
    scale_x_log10()
  ,
  ncol = 2
)








places_plot
ggsave("figs/places_plot.png", places_plot, width = 7, height = 10)
places %>% group_by(region) %>%
  arrange(-total_pop) %>%
  mutate(total_pop = formatC(total_pop, big.mark=",", digits = 0, format="f"),
         med_SAIhealth = signif(med_SAIhealth, 3),
         med_SAIlegal = signif(med_SAIlegal, 3)) %>% 
  slice(1:5) %>% View


ua_plot
ggsave("figs/ua_plot.png", ua_plot, width = 7, height = 10)
ua %>% group_by(region) %>%
  arrange(-total_pop) %>%
  mutate(total_pop = formatC(total_pop, big.mark=",", digits = 0, format="f"),
         med_SAIhealth = signif(med_SAIhealth, 3),
         med_SAIlegal = signif(med_SAIlegal, 3)) %>% 
  slice(1:5) %>% View





# Sensitivity analysis: travel time
# Analyzing how the distance buffers change access
sensitivity_analysis <- left_join(
  AFi_15min %>% 
    # st_drop_geometry() %>% 
    rename(SAIhealth_15min = SAIhealth, SAIlegal_15min = SAIlegal),
  AFi_30min %>% 
    st_drop_geometry() %>% 
    select(SAIhealth_30min = SAIhealth, SAIlegal_30min = SAIlegal, GEOID),
  by = "GEOID"
) %>% left_join(
  AFi_60min %>% 
    st_drop_geometry() %>% 
    select(SAIhealth_60min = SAIhealth, SAIlegal_60min = SAIlegal, GEOID),
  by = "GEOID"
)

# Median tract increase 
sensitivity_analysis %>% 
  st_drop_geometry() %>%
  mutate(health_diff_60_30 = SAIhealth_60min - SAIhealth_30min,
         health_diff_60_15 = SAIhealth_60min - SAIhealth_15min,
         health_diff_30_15 = SAIhealth_30min - SAIhealth_15min,
         legal_diff_60_30 = SAIlegal_60min - SAIlegal_30min,
         legal_diff_60_15 = SAIlegal_60min - SAIlegal_15min,
         legal_diff_30_15 = SAIlegal_30min - SAIlegal_15min
         ) %>% 
  summarize(across(contains("_diff_"), 
                   list(median = ~median(., na.rm=TRUE),
                        mean = ~mean(., na.rm=TRUE))))

# Number who were at 0 who gained access
sensitivity_analysis %>% 
  st_drop_geometry() %>%
  # group_by(region) %>% 
  mutate(health_diff_60_30 = SAIhealth_60min > 0 & SAIhealth_30min == 0,
         health_diff_60_15 = SAIhealth_60min > 0 & SAIhealth_15min == 0,
         health_diff_30_15 = SAIhealth_30min > 0 & SAIhealth_15min == 0,
         legal_diff_60_30 = SAIlegal_60min > 0 & SAIlegal_30min == 0,
         legal_diff_60_15 = SAIlegal_60min > 0 &  SAIlegal_15min == 0,
         legal_diff_30_15 = SAIlegal_30min > 0 & SAIlegal_15min == 0
         ) %>% 
  summarize(across(contains("_diff_"), ~sum(. * tract_totpop, na.rm=TRUE)))

sensitivity_analysis %>% 
  st_drop_geometry() %>%
  # group_by(region) %>% 
  mutate(health_diff_30 = SAIhealth_30min == 0,
         health_diff_15 = SAIhealth_15min == 0,
         health_diff_60 = SAIhealth_60min == 0,
         legal_diff_30 = SAIlegal_30min == 0,
         legal_diff_15 = SAIlegal_15min == 0,
         legal_diff_60 = SAIlegal_60min == 0
         ) %>% 
  summarize(across(contains("_diff_"), ~sum(. * tract_totpop, na.rm=TRUE)))

sensitivity_analysis %>% 
  st_drop_geometry() %>%
  group_by(region) %>% 
  mutate(health_diff_60_30 = SAIhealth_60min > 0 & SAIhealth_30min == 0,
         health_diff_60_15 = SAIhealth_60min > 0 & SAIhealth_15min == 0,
         health_diff_30_15 = SAIhealth_30min > 0 & SAIhealth_15min == 0,
         legal_diff_60_30 = SAIlegal_60min > 0 & SAIlegal_30min == 0,
         legal_diff_60_15 = SAIlegal_60min > 0 &  SAIlegal_15min == 0,
         legal_diff_30_15 = SAIlegal_30min > 0 & SAIlegal_15min == 0
         ) %>% 
  summarize(across(contains("_diff_"), ~sum(. * tract_totpop, na.rm=TRUE)))

ggplot(st_drop_geometry(sensitivity_analysis)) + 
  geom_point(aes(x = log(tract_totpop)/area , y = SAIhealth_60min - SAIhealth_30min))


sensitivity_analysis %>% 
  mutate(health_diff_60_30 = SAIhealth_60min - SAIhealth_30min,
         health_diff_60_15 = SAIhealth_60min - SAIhealth_15min,
         health_diff_30_15 = SAIhealth_30min - SAIhealth_15min,
         legal_diff_60_30 = SAIlegal_60min - SAIlegal_30min,
         legal_diff_60_15 = SAIlegal_60min - SAIlegal_15min,
         legal_diff_30_15 = SAIlegal_30min - SAIlegal_15min
  ) %>% 
  filter(health_diff_60_30 != 0) %>%
  tm_shape() + tm_fill(col = "health_diff_60_30")








drop_and_normalize <- function(vec, n, pct, drop =FALSE) {
  # vec[vec == 0] <- NA
  # vec[vec == 0] <- min(vec[vec != 0])
  topn <- sort(vec, decreasing = TRUE)[1:n]
  
  if(isTRUE(drop)) {
    vec[vec%in% topn] <- 0
  }
  if(isFALSE(drop)) {
    vec[vec %in% topn] <- sort(vec, decreasing = TRUE)[n+1]
  }
  
  # take the log
  # vec <- log10(vec)
  # vec[is.na(vec)] <- 0
  
  # min-max standardization
  vec <- (vec - min(vec, na.rm = TRUE))/(max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE))
  return(vec)
}

cities <- maps::us.cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(st_crs(AFi_30min))
 
# cities %>% filter(country.etc == "CA") %>% View()

gen_maps <- function(shp, region, cities, n_health_drop = 50, n_legal_drop = 50, drop = FALSE, ncol = 2, xmod = NA, ymod = NA) {
  
  if (is.na(xmod)) {
    auto.placement <- TRUE
  } else {
    auto.placement <- FALSE
  }
  
  if (is.na(ymod)) {ymod <- -.1}
  if (is.na(xmod)) {xmod <- 0}
  
  cities <- maps::us.cities %>% 
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    st_transform(st_crs(shp)) %>% 
    slice(match(cities, name))
    # filter(name %in% cities )
  
  tmp <- tmap_arrange(
    shp %>%
      filter(region == {{region}}) %>%
      mutate_at(vars(health_AFi_p_fb_nohi_count), ~drop_and_normalize(., {{n_health_drop}}, drop = {{drop}})) %>%
      tm_shape(.) +
        tm_fill(col = "health_AFi_p_fb_nohi_count", style = "cont", palette = "RdYlGn", title = "Health SAI (Normalized)", alpha = 0.9)# + 
        # tm_shape(cities)  +
        # tm_dots(size = .25) + 
        # tm_text(text = "name", size = .5, auto.placement = {{auto.placement}}, xmod = {{xmod}},ymod = {{ymod}}, fontface = "bold"),
        # tm_layout(asp = 0.5)
    ,
    shp %>%
      filter(region == {{region}}) %>%
      mutate_at(vars(legal_AFi_p_noncit_count), ~drop_and_normalize(., {{n_legal_drop}}, drop = {{drop}})) %>%
      tm_shape(.) +
        tm_fill(col = "legal_AFi_p_noncit_count", style = "cont", palette = "RdYlGn", title = "Legal SAI (Normalized)", alpha = 0.9) #+ 
        # tm_shape(cities) +
        # tm_dots(size = .25) + 
        # tm_text(text = "name", size = .5, auto.placement = {{auto.placement}}, xmod = {{xmod}}, ymod = {{ymod}}, fontface = "bold" ),
        # tm_layout(asp = 0.5)
    ,
    ncol = ncol,
    asp = NA) 
  
  tmap_save(tmp, filename = paste0("figs/",region, ".png"), asp =NA, width = 7, height = 4, dpi = 300)
  
  return(tmp)

}

gen_maps(AFi_30min, "bay_area", 
         cities = c("San Francisco CA", "Oakland CA", "San Jose CA"),
         n_health_drop = 50,
         n_legal_drop = 50)

gen_maps(AFi_30min, "central_valley", 
         cities = c("Stockton CA", "Fresno CA", "Bakersfield CA", "Sacramento CA"),
         n_health_drop = 50,
         n_legal_drop = 200)

gen_maps(AFi_30min, "southern_california", 
         cities = c("Los Angeles CA", "San Diego CA", "Palm Springs CA", "San Bernadino CA", "Anaheim CA", "Santa Barbara CA"),
         n_health_drop = 100,
         n_legal_drop = 200)

gen_maps(AFi_30min, "mountain", cities = c("Redding CA"), n_health_drop =  100, n_legal_drop = 100)
gen_maps(AFi_30min, "Nevada", cities = c("Carson City NV", "Reno NV", "Las Vegas NV"),
         n_health_drop = 100, n_legal_drop = 100)
gen_maps(AFi_30min, "Arizona", cities = c("Phoenix AZ", "Mesa AZ", "Chandler AZ", "Tempe AZ", "Flagstaff AZ", "Prescott AZ", "Yuma AZ", "Tucson AZ", "Lake Havasu AZ"),
         n_health_drop = 150, n_legal_drop = 100)











# Maps for comparison paper

bay_area_counties <- c("Alameda", "Contra Costa", "Solano", "Napa", "Sonoma", "Marin", "San Francisco", "San Mateo", "Santa Clara")
phoenix_counties <- c("Maricopa", "Pinal")

AFi_30min_comparison <- AFi_30min %>% mutate(
  region = NA,
  region = case_when(
    STATE == "California" & COUNTY %in% bay_area_counties ~ "bay_area_comparison",
    STATE == "Arizona" & COUNTY %in% phoenix_counties ~ "phoenix_comparison"
  )) %>%
  filter(!is.na(region))

gen_maps(AFi_30min_comparison, "bay_area_comparison", 
         cities = c("San Francisco CA", "Oakland CA", "San Jose CA"), 
         n_health_drop = 50,
         n_legal_drop = 50,
         drop = FALSE,
         xmod = c(-3.5, 2.5, 2.5))

gen_maps(AFi_30min_comparison, "phoenix_comparison", 
         cities = c("Phoenix AZ", "Mesa AZ", "Chandler AZ", "Tempe AZ", "Scottsdale AZ"),
         n_health_drop = 60, n_legal_drop = 1,
         drop=TRUE,
         ncol = 2,
         xmod = c(-2.5, 2, -2.5, 2.5, 3),
         ymod = c(0, .25, 0, -.25, 0))



clinics_preload  %>% 
  # filter(state == "CA") %>%
  mutate(county = str_replace_all(county, " County", "")) %>%
  filter(county %in% bay_area_counties)  %>%
  group_by(type) %>%
  summarize(n = n())



clinics_preload  %>% 
  mutate(county = str_replace_all(county, " County", "")) %>%
  filter(county %in% phoenix_counties)  %>%
  group_by(type) %>%
  summarize(n = n())









