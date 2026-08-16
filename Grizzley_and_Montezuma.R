#############Packages#########

library(tidyverse)
library(mgcv)
library(lubridate)
library(deltamapr)
library(sf)
library(dplyr)
library(readr)
library(data.table)
library(geosphere)
library(wql)
library(car)
library(corrplot)  
library(energy)    
library(GGally)


###########Bring in the Zoop dataset ##########
Zoop_Community <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/zooplankton_community (3).csv")


##################################################################
## PART 2 — Inspect actual subregion names before filtering
##################################################################

# List every subregion name that actually exists in this shapefile -
# don't guess at the exact string, confirm it here first
sort(unique(R_EDSM_Subregions_Mahardja$SubRegion))

# Quick visual of everything, for orientation
ggplot(R_EDSM_Subregions_Mahardja) +
  geom_sf(aes(fill = SubRegion)) +
  theme_bw() +
  theme(legend.position = "none")


##################################################################
## PART 3 — Filter to Grizzly Bay only
##################################################################

target_regions <- c("Grizzly Bay")

study_area <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion %in% target_regions)

cat("\nRegions matched:\n")
print(unique(study_area$SubRegion))
cat("\nRow count:\n")
print(nrow(study_area))

ggplot(study_area) +
  geom_sf(fill = "steelblue") +
  theme_bw() +
  labs(title = "Grizzly Bay")

##################################################################
## PART 4 — Convert Zoop stations to spatial points, clip to Grizzly Bay
##################################################################

st_crs(R_EDSM_Subregions_Mahardja)   # confirm CRS before spatial ops

zoop_stations_sf <- Zoop_Community_avg %>%
  distinct(Channel_Station, Latitude, Longitude) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(R_EDSM_Subregions_Mahardja))

zoop_stations_in_area <- zoop_stations_sf %>%
  st_filter(study_area, .predicate = st_within)

cat("\nZoop stations found within Grizzly Bay:\n")
print(nrow(zoop_stations_in_area))
print(zoop_stations_in_area$Channel_Station)

##################################################################
## PART 5 — Map it
##################################################################

ggplot() +
  geom_sf(data = study_area, fill = "steelblue", alpha = 0.4) +
  geom_sf(data = zoop_stations_in_area, color = "red", size = 2) +
  theme_bw() +
  labs(title = "Zooplankton Stations in Grizzly Bay")


##################################################################
## PART 6 — table of stations only in Grizzley
##################################################################
zoop_grizzly_bay <- Zoop_Community %>%
  filter(Station %in% zoop_stations_in_area$Channel_Station)

cat("\nRows:\n"); print(nrow(zoop_grizzly_bay))
cat("\nDistinct stations included:\n"); print(unique(zoop_grizzly_bay$Station))

View(zoop_grizzly_bay)
write_csv(zoop_grizzly_bay, "zoop_grizzly_bay.csv")













Zoop_Community %>%
  filter(Year %in% c(2020, 2021)) %>%
  count(Year, Source) %>%
  group_by(Year) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(Year, desc(pct))
