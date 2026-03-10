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


