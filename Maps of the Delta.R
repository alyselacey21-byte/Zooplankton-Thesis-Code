#Maps of the Delta

install.packages("sf")
install.packages("terra")
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
  tm_borders(lwd = 2, col = "black") +   # State border
  tm_fill(col = "steelblue") +           # Fill color
  tm_layout(title = "California Map",
            title.size = 1.2,
            frame = FALSE)