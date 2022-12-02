setwd("~/BIMI_SAI")
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot(10))

message(
  "
  \n \n \n \n 
  1_calc_sj.R
  Author: Ethan Roubenoff, eroubenoff@berkeley.edu
  \n \n \n \n
  This script loads the raw consolidated data 
  ('clinic_data/xxx_clinics_noscreenshots.csv') 
  and saves two objects ('./xxx_clinics.Rdata') that are
  csv files with the sj column attached.  This script also
  prints out basic descriptives about sj. \n \n \n \n "
)

# legal_clinics <- read_csv("clinic_data/legal_clinics_noscreenshots.csv",
#                           col_types = cols())
# health_clinics <- read_csv("clinic_data/health_clinics_noscreenshots.csv",
#                            col_types = cols())

health_clinics <- read_csv("data/health_final_2022.csv") 
legal_clinics <- read_csv("data/legal_2020_combined.csv")

# Legal clinics need to combine with geocoded data
legal_geocoded <- read_csv("data/CA_NV_AZ_legal_geocoded.csv")



#### Tally the number of services for each clinic type ####
health_services = c(
  "service_behavioral",
  "service_cancer",
  "service_dental",
  "service_chronic",
  "service_emergency",
  "service_holistic",
  "service_immunizations",
  "service_prevention",
  "service_primary",
  "service_physicals",
  "service_pharmacy",
  "service_sexual",
  "service_social",
  "service_women",
  "service_specialty",
  "service_domestic"
)


legal_services = c(
  "service_adjustment",
  "service_asylum",
  "service_citizenship",
  "service_consular",
  "service_criminal",
  "service_daca",
  "service_domestic",
  "service_employment_auth",
  "service_employment_imm",
  "service_family_petition",
  "service_family_law",
  "service_green_card",
  "service_habeus_corpus",
  "service_housing",
  "service_rights",
  "service_labor",
  "service_naraca",
  "service_naturalization",
  "service_removal",
  "service_juvenile",
  "service_t_visas",
  "service_tps",
  "service_u_visas",
  "service_vawa"
)



message(" \n \n \n \n 
        Health services offered frequency 
          \n \n \n \n")

health_services <- health_clinics %>%
  select(all_of(health_services)) 

health_services %>%
  colSums(na.rm = TRUE) %>%
  data.frame(service = names(.), count = .) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE)


message(" \n \n \n \n 
        Legal services offered frequency 
          \n \n \n \n")

legal_services <- legal_clinics %>%
  select(all_of(legal_services))

legal_services %>%
  colSums(na.rm = TRUE) %>%
  data.frame(service = names(.), count = .) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE)



#### Combining services to form sj ####
# Health
sj_health <- health_services %>% mutate(
  sj_primary = service_primary | service_physicals,
  sj_behavioral = service_behavioral,
  sj_dental = service_dental,
  sj_women = service_women | service_sexual,
  sj_immunizations = service_immunizations
)  %>%
  select(contains("sj")) %>%
  rowSums(., na.rm = TRUE) + 1

# 45 missing operating hours and are imputed with the median
table(health_clinics$operating_hours == 0 | is.na(health_clinics$operating_hours))
health_clinics[health_clinics$operating_hours == 0 | is.na(health_clinics$operating_hours), "operating_hours"] <- median(health_clinics$operating_hours, na.rm = TRUE)

# Add sj to table
health_clinics$sj <- sj_health * health_clinics$operating_hours

# Plot
p <- qplot(health_clinics$sj, geom="histogram", binwidth = 50) +
  ggtitle("Health Clinics sj") +
  xlab("sj") +
  ylab("Frequency")
p
ggsave("figs/health_clinics_sj_histogram.png", p, width = 5, height= 3)




# Legal
sj_legal <- legal_services %>% mutate(
  sj_status = service_adjustment | service_consular | service_family_petition,
  sj_citizenship = service_citizenship | service_naturalization,
  sj_visas = service_daca | service_juvenile | service_t_visas | service_u_visas | service_tps |service_green_card,
  sj_asylum = service_asylum | service_naraca,
  sj_rights = service_rights | service_removal,
  sj_criminal = service_criminal | service_habeus_corpus,
  sj_family = service_domestic | service_family_law | service_vawa,
  sj_employment = service_employment_auth | service_employment_imm | service_labor,
  sj_housing = service_housing
) %>%
  select(contains("sj")) %>%
  rowSums(., na.rm = TRUE) + 1


# Legal clinics hours are strings  
legal_hrs_f = function(.) case_when(
  . == "43713" ~ as.numeric(NA),
  . == "10-4 M-W,F" ~ 24,
  . == "9-1 M-TH" ~ 16,
  . == "9-5 M-F" ~ 40,
  . == "M-F 9-5" ~ 40,
  . == "M-F: 9:00am - 5:00pm" ~ 40,
  . == "M-T 9-5, F 9-12" ~ 36,
  . == "M-Th 10-12:30, 1:30-6" ~ 40,
  . == "T 1 - 4" ~ 3,
  . == "Third Thursday Every Month 9-11" ~ 3,
  . == "Thursday Appointments, 9am -12pm , 1pm-5pm" ~ 7,
  . == "W 1:30-4:30" ~ 3,
  . == "Th 9-12" ~ 3,
  TRUE ~ as.numeric(.)
)

legal_clinics <- legal_clinics %>%
  mutate(capacity_hours = legal_hrs_f(capacity_hours)) 

# about half missing and are imputed with the median
table(is.na(legal_clinics$capacity_hours))
legal_clinics[is.na(legal_clinics$capacity_hours), "capacity_hours"] <- median(legal_clinics$capacity_hours, na.rm = TRUE)

legal_clinics <- legal_clinics %>%
  mutate(sj = sj_legal * capacity_hours)

p <- qplot(legal_clinics$sj, geom="histogram", binwidth = 50) +
  ggtitle("Legal Clinics sj") +
  xlab("sj") + 
  ylab("Frequency")
p
ggsave("figs/legal_clinics_sj_histogram.png", p, width = 5, height = 3)




message("\n \n \n \n
        Saving Objects 
        \n \n \n \n")

save(legal_clinics, file = "data/legal_clinics.Rdata")
save(health_clinics, file = "data/health_clinics.Rdata")


# File for analysis
clinics_preload <- bind_rows(
  health_clinics %>% 
    select(site_name, site_address = `site address`, city, state, zipcode = postal, county, sj, geometry) %>%
    mutate(type = "health") %>%
    mutate(lon = as.numeric(str_extract(geometry, "(?<=c\\().*(?=,)")),
           lat = as.numeric(str_extract(geometry, "(?<=, ).*(?=\\))"))
           ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326),
  legal_clinics %>% 
    select(site_name, site_address = address_final, city, state, zipcode, county, sj, x, y) %>%
    mutate(type = "legal") %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
) %>%
  mutate(index = 1:n())

save(clinics_preload, file = "data/clinics_preload.Rdata")


# Tallies of services:
health_services %>% select(contains("service")) %>% colSums(na.rm = TRUE) %>% sort(decreasing = TRUE)
legal_services %>% select(contains("service")) %>% colSums(na.rm = TRUE) %>% sort(decreasing = TRUE)

message("\n \n \n \n
        Exiting 
        \n \n \n \n")

