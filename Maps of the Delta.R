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

