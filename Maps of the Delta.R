#Maps of the Delta

install.packages("sf")
# Always use binary (faster, no prompt)
install.packages("terra", type = "binary")

# Always use source (latest version)
install.packages("terra", type = "source")
install.packages("tmap", repos = c("https://r-tmap.r-universe.dev","https://cloud.r-project.org"))
install.packages("spData")
install.packages("spDataLarge", repos = c("https://geocompx.r-universe.dev"))
install.packages("leaflet")
install.packages("USAboundaries")
install.packages("USAboundariesData", repos = c("https://ropensci.r-universe.dev","https://cloud.r-project.org"))
install.packages("elevatr")
install.packages("rnaturalearth")
install.packages("rnaturalearthhires", repos = c("https://ropensci.r-universe.dev"))
install.packages("rnaturalearthdata")
install.packages("tigris")


library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tmap)    # for static and interactive maps
library(USAboundaries)
library(USAboundariesData)
library(elevatr)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(tigris)


# Get U.S. states geometry
states <- us_states()

# Filter for California
california <- subset(us_counties(), state_name == "California")


tm_shape(california) +   tm_fill() +
  tm_borders() 

# Create a simple map of California
tm_shape(california) +
  tm_polygons(col = "lightblue", border.col = "black", lwd = 2) +
  tm_layout(main.title = "California Map",
            main.title.size = 1.2,
            main.title.position = "center",
            frame = FALSE)


#Store California as an object
map_Cal = tm_shape(california) +
  tm_polygons(col = "lightblue", border.col = "black", lwd = 2) +
  tm_layout(main.title = "California Map",
            main.title.size = 1.2,
            main.title.position = "center",
            frame = FALSE)

map_Cal


#Elevations in Cali
ca_elev <- get_elev_raster(california, z = 7)  # z = zoom level

#Crop to California boundaries
ca_elev <- crop(ca_elev, california)
ca_elev <- mask(ca_elev, california)


map_Cal_Ele = map_Cal +
  tm_shape(ca_elev) + tm_raster(col_alpha = 0.7)

map_Cal_Ele




#Adds in ocean

bbox <- st_bbox(california_proj)
xmin <- as.numeric(bbox["xmin"])
xmax <- as.numeric(bbox["xmax"])
ymax <- as.numeric(bbox["ymax"])

# Use ocean_buffer's ymin instead to not cut off southern CA
ocean_bbox <- st_bbox(ocean_buffer)
ymin <- as.numeric(ocean_bbox["ymin"]+123550)

trim_box <- st_polygon(list(matrix(c(
  xmin - 2500000, ymin,
  xmax + 2500000, ymin,
  xmax + 2500000, ymax - 600,
  xmin - 2500000, ymax - 600,
  xmin - 2500000, ymin
), ncol = 2, byrow = TRUE))) |>
  st_sfc(crs = 3310)

ocean_buffer_trimmed <- st_intersection(ocean_buffer, trim_box)

ca_elev_agg <- terra::aggregate(ca_elev, fact = 3)

map_cali_water <- tm_shape(ocean_buffer_trimmed) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(midpoint = NA)) +
  tm_layout(main.title = "California Map",
            main.title.position = "center",
            frame = FALSE)

map_cali_water




##########counties and rivers

# Make sure all layers match CRS
ca_rivers <- st_transform(ca_rivers, 3310)
ca_counties <- st_transform(ca_counties, 3310)

# Crop rivers to California boundary
ca_rivers <- st_intersection(ca_rivers, california_proj)


crop_box <- st_bbox(c(
  xmin = xmin - 250000,
  xmax = xmax + 250000,
  ymin = ymin + 555000,   # trim south - increase to trim more
  ymax = ymax - 350000    # trim north - increase to trim more
), crs = 3310) |>
  st_as_sfc()

map_cali_riv <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(midpoint = NA)) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_layout(main.title = "California Map",
            main.title.position = "center",
            frame = FALSE)

map_cali_riv

#Old map

crop_box <- st_bbox(c(
  xmin = -370000,    # further west
  xmax = -50000,
  ymin = -100000,    # lower south to show full bay
  ymax = 100000
), crs = 3310) |>
  st_as_sfc()

map_cali_delta <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(midpoint = NA)) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_layout(main.title = "SF Bay-Delta Region",
            main.title.position = "center",
            frame = FALSE)

map_cali_delta




#More precise fill and such. 

library(tigris)
sf_bay <- area_water(state = "CA", county = c("San Francisco", "Marin", "Contra Costa", "Alameda", "Santa Clara", "San Mateo")) |>
  st_transform(3310)
# Sacramento city coordinates in 3310
sacramento <- st_sfc(st_point(c(-121.4944, 38.5816)), crs = 4326) |>
  st_as_sf() |>
  st_transform(3310)

crop_box <- st_bbox(c(
  xmin = -225000, # LEFT edge — decrease to move left, increase to move right
  xmax = -110000, # RIGHT edge — decrease to move left, increase to move right
  ymin = -30000,  # BOTTOM edge — decrease to move down, increase to move up
  ymax = 70000 # TOP edge — decrease to move down, increase to move up
), crs = 3310) |>
  st_as_sfc()

map_cali_delta <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(sf_bay) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(
              midpoint = NA,
              values = "terrain",
              labels = c("Below Sea", "Low", "Medium", "High", "Very High", "Mountain", "Peak")
            ),
            col.legend = tm_legend(title = "Elevation (m)")) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_shape(sacramento) +
  tm_symbols(shape = 21, size = 0.75, col = "orange", border.col = "black") +
  tm_add_legend(type = "symbol", shape = 21, col = "orange", 
                border.col = "black", size = 0.75, 
                labels = "Sacramento", title = "City") +
  tm_layout(main.title = "SF Bay-Delta Region",
            main.title.position = "center",
            frame = FALSE)

print(map_cali_delta)









###############Now adding the trawls and unique coordinate points to the map################

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
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
                 "Undersampled"    ), check.names=TRUE)

unlink(infile1)


# Get unique coordinate points only
trawl_unique <- dt1 |>
  filter(!is.na(Latitude), !is.na(Longitude)) |>
  group_by(Latitude, Longitude, Source) |>
  summarise(count = n(), .groups = "drop")

trawl_sf <- st_as_sf(trawl_unique,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326) |>
  st_transform(3310)

# Add to map
map_cali_delta <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(sf_bay) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(
              midpoint = NA,
              values = "terrain",
              labels = c("Below Sea", "Low", "Medium", "High", "Very High", "Mountain", "Peak")
            ),
            col.legend = tm_legend(title = "Elevation (m)")) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_shape(sacramento) +
  tm_symbols(shape = 21, size = 0.75, col = "orange", border.col = "black") +
  tm_add_legend(type = "symbol", shape = 21, col = "orange",
                border.col = "black", size = 0.75,
                labels = "Sacramento", title = "City") +
  tm_shape(trawl_sf) +
  tm_symbols(col = "Source",
             size = 0.3,
             shape = 21,
             border.col = "black",
             border.lwd = 0.001,
             col.legend = tm_legend(title = "Tow Type")) +
  tm_layout(main.title = "SF Bay-Delta Region",
            main.title.position = "center",
            frame = FALSE)

map_cali_delta







###############size of the dot depends on how many times a point is trawled#########
trawl_unique <- dt1 |>
  filter(!is.na(Latitude), !is.na(Longitude)) |>
  group_by(Latitude, Longitude, Source) |>
  summarise(count = n(), .groups = "drop")

trawl_sf <- st_as_sf(trawl_unique,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326) |>
  st_transform(3310)


map_cali_delta <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(sf_bay) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(
              midpoint = NA,
              values = "terrain",
              labels = c("Below Sea", "Low", "Medium", "High", "Very High", "Mountain", "Peak")
            ),
            col.legend = tm_legend(title = "Elevation (m)")) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_shape(sacramento) +
  tm_symbols(shape = 21, size = 0.75, col = "orange", border.col = "black") +
  tm_add_legend(type = "symbol", shape = 21, col = "orange",
                border.col = "black", size = 0.75,
                labels = "Sacramento", title = "City") +
  tm_shape(trawl_sf) +
  tm_symbols(col = "Source",
             size = "count",
             size.scale = tm_scale(values.scale = 1.5),
             shape = 21,
             border.col = "black",
             border.lwd = 0.1,
             col.legend = tm_legend(title = "Source"),
             size.legend = tm_legend(title = "Sample Count")) +
  tm_layout(main.title = "SF Bay-Delta Region",
            main.title.position = "center",
            frame = FALSE)

map_cali_delta



#########Now doing this with the clams 

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1036/6/7475c46f9c8bebf01ad24d50ee7a0ff9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Date",     
                 "Station",     
                 "LabSampleNumber",     
                 "Grab",     
                 "Year",     
                 "Month",     
                 "OrganismCode",     
                 "Count",     
                 "Phylum",     
                 "Class_level",     
                 "Order_level",     
                 "Family_level",     
                 "Genus",     
                 "Species",     
                 "Common_name",     
                 "Location",     
                 "Latitude",     
                 "Longitude"    ), check.names=TRUE)

unlink(infile1)

library(tidyverse)
library(mgcv)
library(lubridate)


######## actual map

# Split data
# Filter before converting to sf

#######These maps are not really correct########
clam_unique <- dt2 |>
  filter(!is.na(Latitude), !is.na(Longitude)) |>
  filter(Common_name %in% c("Asian Clam", "Brackish-water Corbula")) |>
  group_by(Latitude, Longitude, Common_name) |>
  summarise(count = n(), .groups = "drop")

corbula_unique <- clam_unique |> filter(Common_name == "Brackish-water Corbula")
asian_clam_unique <- clam_unique |> filter(Common_name == "Asian Clam")

nrow(corbula_unique)
nrow(asian_clam_unique)
# Convert separately
corbula_sf <- st_as_sf(corbula_unique,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326) |>
  st_transform(3310)

asian_clam_sf <- st_as_sf(asian_clam_unique,
                          coords = c("Longitude", "Latitude"),
                          crs = 4326) |>
  st_transform(3310)

# Corbula map
map_corbula <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(sf_bay) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(midpoint = NA, values = "terrain",
                                           labels = c("Below Sea", "Low", "Medium", "High", "Very High", "Mountain", "Peak")),
            col.legend = tm_legend(title = "Elevation (m)")) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_shape(sacramento) +
  tm_symbols(shape = 21, size = 0.75, col = "orange", border.col = "black") +
  tm_add_legend(type = "symbol", shape = 21, col = "orange",
                border.col = "black", size = 0.75,
                labels = "Sacramento", title = "City") +
  tm_shape(corbula_sf) +
  tm_symbols(col = "red", size = "count",
             size.scale = tm_scale(values.scale = 1.5),
             shape = 21, border.col = "black", border.lwd = 0.1,
             size.legend = tm_legend(title = "Sample Count")) +
  tm_layout(main.title = "Brackish-water Corbula",
            main.title.position = "center", frame = FALSE)

# Asian clam map
map_asian_clam <- tm_shape(ocean_buffer_trimmed, bbox = crop_box) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(sf_bay) +
  tm_fill(col = "steelblue", alpha = 0.8) +
  tm_shape(california_proj) +
  tm_borders(lwd = 2, col = "black") +
  tm_shape(ca_elev_agg) +
  tm_raster(col_alpha = 0.7,
            col.scale = tm_scale_intervals(midpoint = NA, values = "terrain",
                                           labels = c("Below Sea", "Low", "Medium", "High", "Very High", "Mountain", "Peak")),
            col.legend = tm_legend(title = "Elevation (m)")) +
  tm_shape(ca_counties) +
  tm_borders(lwd = 0.5, col = "gray40") +
  tm_shape(ca_rivers) +
  tm_lines(col = "steelblue", lwd = 0.8) +
  tm_shape(sacramento) +
  tm_symbols(shape = 21, size = 0.75, col = "orange", border.col = "black") +
  tm_add_legend(type = "symbol", shape = 21, col = "orange",
                border.col = "black", size = 0.75,
                labels = "Sacramento", title = "City") +
  tm_shape(asian_clam_sf) +
  tm_symbols(col = "purple", size = "count",
             size.scale = tm_scale(values.scale = 1.5),
             shape = 21, border.col = "black", border.lwd = 0.1,
             size.legend = tm_legend(title = "Sample Count")) +
  tm_layout(main.title = "Asian Clam",
            main.title.position = "center", frame = FALSE)

map_corbula
map_asian_clam