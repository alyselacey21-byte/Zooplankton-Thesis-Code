
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

#####################Fish lat and long###########
options(HTTPUserAgent="EDI_CodeGen") 
infile3 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1075/2/79240c490fe74543da6b86a1c7c751b9") 
infile3 <-sub("^https","http",infile3)
# This creates a tibble named: dt1 
dt3 <-read_delim(infile3  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "Source",   
                   "Station",   
                   "Latitude",   
                   "Longitude",   
                   "Date",   
                   "Datetime",   
                   "Survey",   
                   "Depth",   
                   "SampleID",   
                   "Method",   
                   "Tide",   
                   "Sal_surf",   
                   "Sal_bot",   
                   "Temp_surf",   
                   "TurbidityNTU",   
                   "TurbidityFNU",   
                   "Secchi",   
                   "Secchi_estimated",   
                   "Tow_duration",   
                   "Tow_area",   
                   "Tow_volume",   
                   "Cable_length",   
                   "Tow_direction",   
                   "Notes_tow",   
                   "Notes_flowmeter"   ), 
                 col_types=list( 
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() ,  
                   col_date("%Y-%m-%d"),   
                   col_datetime("%Y-%m-%d %H:%M:%S"), 
                   
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_character(),  
                   col_character()), 
                 na=c(" ",".","NA","")  )


library("deltamapr")
library("ggplot2")
library("sf")
library("tidyverse")

head(dt1)
head(dt2)
head(dt3)

dt1 %>% filter(grepl("smelt", Taxa, ignore.case = TRUE) |
           grepl("Hypomesus", Taxa, ignore.case = TRUE)) %>% distinct(Taxa)



my_data <- dt1 %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326,remove = FALSE) %>% st_transform(st_crs(R_EDSM_Strata_1718P1))

san_pablo <- R_EDSM_Subregions_Mahardja %>% filter(SubRegion == "San Pablo Bay") %>% mutate(Stratum = "San Pablo Bay")  # adds it into the Stratum fill aesthetic

ggplot() +
  geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = san_pablo, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = my_data, size = 1, color = "black", alpha = 0.5) +
  theme_bw() +
  labs(title = "EDSM Strata with Sample Locations")


###################join lat and long dataset with fish dataset################

# Check dt3 loaded correctly
head(dt3)

# Check SampleID formats match between tables
head(dt1$SampleID)
head(dt3$SampleID)


# Check if any SampleIDs match between the two tables
intersect(dt1$SampleID, dt3$SampleID)

# Also check what surveys are in each
unique(dt1$SampleID) %>% head(20)
unique(dt3$SampleID) %>% head(20)

unique(dt3$Source)

unique(substr(dt1$SampleID, 1, 10))


# Filter dt3 to only 20mm survey entries to keep it lean
tow_locations <- dt3 %>%
  filter(Source == "20mm") %>%
  select(SampleID, Latitude, Longitude, Date, Station)

# Join catch data with tow locations, filter for Delta Smelt only
delta_smelt <- dt1 %>%
  filter(Taxa == "Hypomesus transpacificus") %>%
  left_join(tow_locations, by = "SampleID") %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Check the join worked
nrow(delta_smelt)
head(delta_smelt)




###########Only looking at trawls and points with delta smelt############

library(deltamapr)
library(ggplot2)
library(sf)
library(dplyr)

# Step 1: Tow locations from ALL surveys
tow_locations <- dt3 %>%
  select(SampleID, Latitude, Longitude, Date, Station, Source)

# Step 2: Join all catch data with tow locations
all_trawls <- dt1 %>%
  left_join(tow_locations, by = "SampleID") %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Step 3: Build strata boundary including San Pablo Bay
san_pablo <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion == "San Pablo Bay") %>%
  mutate(Stratum = "San Pablo Bay") %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1))

strata_boundary <- R_EDSM_Strata_1718P1 %>%
  bind_rows(san_pablo)

# Step 4: All unique trawl locations clipped to strata boundary
all_trawl_locations <- all_trawls %>%
  distinct(SampleID, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1)) %>%
  st_intersection(st_union(strata_boundary))

# Step 5: Delta Smelt positive catches only
delta_smelt_positive <- all_trawls %>%
  filter(Taxa == "Hypomesus transpacificus", Count > 0) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1)) %>%
  st_intersection(st_union(strata_boundary))

# Step 6: Plot
ggplot() +
  geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = san_pablo, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = delta_smelt_positive, size = 1.5, color = "blue", alpha = 0.7) +
  theme_bw() +
  labs(title = "EDSM Strata - Delta Smelt Positive Catches vs All Trawls",
       caption = "Grey = all trawl locations | Blue = Delta Smelt positive catches")




#########Adding salmonids to the map##########
dt1 %>%
  filter(grepl("salmon|oncorhynchus", Taxa, ignore.case = TRUE)) %>%
  distinct(Taxa)



# Build combined strata boundary including San Pablo Bay
san_pablo <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion == "San Pablo Bay") %>%
  mutate(Stratum = "San Pablo Bay") %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1))

strata_boundary <- R_EDSM_Strata_1718P1 %>%
  bind_rows(san_pablo)

# Use ALL surveys from dt3, not just 20mm
tow_locations <- dt3 %>%
  select(SampleID, Latitude, Longitude, Date, Station, Source)

# Delta Smelt positive catches across all surveys
delta_smelt_positive <- dt1 %>%
  filter(Taxa == "Hypomesus transpacificus") %>%
  left_join(tow_locations, by = "SampleID") %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Count > 0) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1)) %>%
  st_intersection(st_union(strata_boundary)) %>%
  mutate(Species = "Delta Smelt")

# Salmon positive catches across all surveys
salmon <- dt1 %>%
  filter(Taxa %in% c("Oncorhynchus tshawytscha", "Oncorhynchus kisutch")) %>%
  left_join(tow_locations, by = "SampleID") %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Count > 0) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1)) %>%
  st_intersection(st_union(strata_boundary)) %>%
  mutate(Species = "Salmon")

# Combine and plot
both_species <- bind_rows(delta_smelt_positive, salmon)

ggplot() +
  geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = san_pablo, aes(fill = Stratum), alpha = 0.6) +
  geom_sf(data = both_species, aes(color = Species), size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("Delta Smelt" = "blue", "Salmon" = "red")) +
  theme_bw() +
  labs(title = "EDSM Strata - Delta Smelt and Salmon Catches (All Surveys)",
       color = "Species")






