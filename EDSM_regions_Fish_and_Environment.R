
library(tidyverse)
library(mgcv)



##############Fish###############
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1075/2/5429d3e82b1671e7454c7b5d7a15c6ef") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt2 
dt1 <-read_delim(infile1  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "SampleID",   
                   "Taxa",   
                   "Length",   
                   "Count",   
                   "Notes_catch"   ), 
                 col_types=list( 
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() ,  
                   col_character()), 
                 na=c(" ",".","NA","")  )



#####################Environmental Data############################
inUrl2  <- trimws("https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec") 
infile2 <- sub("^https","http",inUrl2)

dt2 <-read_delim(infile2
                 ,delim=","  
                 ,skip=1
                 ,quote = "" 
                 ,col_names=c(
                   "Source",     
                   "Station",     
                   "Latitude",     
                   "Longitude",     
                   "Year",     
                   "Date",     
                   "Datetime",     
                   "SampleID",     
                   "TowType",     
                   "AmphipodCode",     
                   "Tide",     
                   "BottomDepth",     
                   "Chl",     
                   "Secchi",     
                   "Temperature",     
                   "Turbidity",     
                   "Microcystis",     
                   "pH",     
                   "DO",     
                   "SalSurf",     
                   "SalBott",     
                   "SizeClass",     
                   "Volume",     
                   "Phylum",     
                   "Class",     
                   "Order",     
                   "Family",     
                   "Genus",     
                   "Species",     
                   "Taxname",     
                   "Lifestage",     
                   "Taxlifestage",     
                   "CPUE",     
                   "Undersampled"    ), col_types=list(
                     col_character(),
                     col_character(),
                     col_number(),
                     col_number(),
                     col_character(),
                     col_date("%Y-%m-%d"),
                     col_datetime("%Y-%m-%d %H:%M:%S"),
                     
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_character(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_character(),
                     col_number(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_character(),
                     col_number(),
                     col_character()),
                 na=c("",".","NA",""))

unlink(infile1)


library("deltamapr")
library("ggplot2")
library("sf")
library("tidyverse")

head(dt1)
head(dt2)

dt1 %>% filter(grepl("smelt", Taxname, ignore.case = TRUE) |grepl("smelt", Species, ignore.case = TRUE)) %>% distinct(Species, Taxname)



my_data <- dt1 %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326,remove = FALSE) %>% st_transform(st_crs(R_EDSM_Strata_1718P1))

san_pablo <- R_EDSM_Subregions_Mahardja %>% filter(SubRegion == "San Pablo Bay") %>% mutate(Stratum = "San Pablo Bay")  # adds it into the Stratum fill aesthetic

ggplot() +
  geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = san_pablo, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = my_data, size = 1, color = "black", alpha = 0.5) +
  theme_bw() +
  labs(title = "EDSM Strata with Sample Locations")

###########Only looking at trawls and points with delta smelt############

# Combine strata + san pablo into one boundary polygon
strata_boundary <- R_EDSM_Strata_1718P1 %>%
  bind_rows(san_pablo %>% st_transform(st_crs(R_EDSM_Strata_1718P1)))

# Filter for Delta Smelt only, remove missing coords, clip to strata boundary
my_data <- dt1 %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Species == "Hypomesus transpacificus") %>%  # Delta Smelt scientific name
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1)) %>%
  st_intersection(st_union(strata_boundary))  # clips points to boundary

san_pablo <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion == "San Pablo Bay") %>%
  mutate(Stratum = "San Pablo Bay")

ggplot() +
  geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = san_pablo, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = my_data, size = 1, color = "black", alpha = 0.5) +
  theme_bw() +
  labs(title = "EDSM Strata - Delta Smelt Catch Locations")


