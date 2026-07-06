options(repos = c(
  sbashevkin = 'https://sbashevkin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))


library("deltamapr")
library("ggplot2")
library("sf")
library("tidyverse")

ggplot(R_EDSM_Subregions_Mahardja)+
  geom_sf(aes(fill=SubRegion))+
  theme_bw()+
  theme(legend.position="none")



library("ggrepel")

ggplot(R_EDSM_Subregions_Mahardja) +
  geom_sf(aes(fill = SubRegion)) +
  geom_text_repel(
    data = R_EDSM_Subregions_Mahardja,
    aes(label = SubRegion, geometry = geometry),
    stat = "sf_coordinates",
    size = 2,
    min.segment.length = 0
  ) +
  theme_bw() +
  theme(legend.position = "none")


options(repos = c(
  sbashevkin = 'https://sbashevkin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
library("deltamapr")
library("ggplot2")
library("sf")
library("dplyr")
library("gridExtra")  # for combining map + table
library("mgcv")


#Just split the key table into two side-by-side halves:
options(repos = c(
    sbashevkin = 'https://sbashevkin.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library("deltamapr")
library("ggplot2")
library("sf")
library("dplyr")
library("gridExtra")

# Assign a number to every subregion
map_data <- R_EDSM_Subregions_Mahardja %>%
  arrange(SubRegion) %>%
  mutate(key = row_number())

# Build the map with numbers only
map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = SubRegion)) +
  geom_sf_text(
    aes(label = key),
    size = 2.5,
    fontface = "bold",
    color = "black"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "EDSM Subregions")

# Split key table into two halves
key_table <- map_data %>%
  st_drop_geometry() %>%
  select(`#` = key, Subregion = SubRegion) %>%
  arrange(`#`)

half <- ceiling(nrow(key_table) / 2)
col1 <- key_table[1:half, ]
col2 <- key_table[(half + 1):nrow(key_table), ]

# Give col2 consistent row count (pad with blanks if odd number of regions)
if (nrow(col2) < nrow(col1)) {
  col2 <- col2 %>% add_row(`#` = NA, Subregion = "")
}

grob1 <- tableGrob(col1, rows = NULL,
                   theme = ttheme_minimal(
                     base_size = 7,
                     core = list(padding = unit(c(2, 4), "mm")),
                     colhead = list(fg_params = list(fontface = "bold"))
                   ))

grob2 <- tableGrob(col2, rows = NULL,
                   theme = ttheme_minimal(
                     base_size = 7,
                     core = list(padding = unit(c(2, 4), "mm")),
                     colhead = list(fg_params = list(fontface = "bold"))
                   ))

# Combine everything: map | col1 | col2
grid.arrange(map_plot, grob1, grob2, ncol = 3, widths = c(2.5, 1, 1))



###########Taking points from Zoop data and plotting them on the edsm subregion mapr###############
library(tidyverse)
library(mgcv)

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- trimws("https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec") 
infile1 <- sub("^https","http",inUrl1)

dt1 <-read_delim(infile1
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




my_points <- dt1 %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326,remove = FALSE) %>% st_transform(st_crs(R_EDSM_Subregions_Mahardja))

ggplot() + geom_sf(data = R_EDSM_Subregions_Mahardja, aes(fill = SubRegion), alpha = 0.6) + geom_sf(data = my_points, size = 1, color = "black", alpha = 0.5) + theme_bw() + theme(legend.position = "none") + labs(title = "EDSM Subregions with Sample Locations")


unique(R_EDSM_Subregions_Mahardja$SubRegion)



subregions_combined <- R_EDSM_Subregions_Mahardja %>% filter(SubRegion != "San Francisco Bay", SubRegion !="South Bay", SubRegion !="Grant Line Canal and Old River", SubRegion != "Upper San Joaquin River") %>% mutate(Region = case_when(SubRegion %in% c("Lower Sacramento River Ship Channel", "Upper Sacramento River", "Upper Sacramento River Ship Channel") ~ "Sacramento River Ship Channel", SubRegion %in% c("Upper Sacramento River", "Lower Sacramento River") ~ "Sacramento River", SubRegion %in% c("Upper Mokelumne River","Lower Mokelumne River") ~ "Mokelumne River", SubRegion %in% c("Upper Napa River", "Lower Napa River") ~ "Napa River", SubRegion %in% c("Cache Slough and Lindsey Slough", "Lower Cache Slough", "Liberty Island") ~ "Cache Slough Complex", SubRegion %in% c("Mid Suisun Bay", "West Suisun Bay", "Honker Bay") ~ "Suisun Bay", SubRegion %in% c("Franks Tract", "Holland Cut", "Mildred Island") ~ "Central Delta", SubRegion %in% c("Lower San Joaquin River") ~ "San Joaquin River", SubRegion %in% c("San Joaquin River at Twitchell Island","San Joaquin River at Prisoners Pt","Georgiana Slough") ~ "Central San Joaquin",SubRegion %in% c("Rock Slough and Discovery Bay", "Victoria Canal") ~ "Southern Delta Canals", SubRegion %in% c("Old River", "Middle River") ~ "Old and Middle River",TRUE ~SubRegion  
# keep all others as-is  
)) %>% group_by(Region) %>% summarise(geometry = st_union(geometry))

ggplot() + geom_sf(data = subregions_combined, aes(fill = Region), alpha = 0.6) + geom_sf(data = my_points, size = 1, color = "black", alpha = 0.5) + theme_bw() + theme(legend.position = "none") + labs(title = "EDSM Subregions with Sample Locations")




library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Assign a number to every combined region
map_data <- subregions_combined %>%
  arrange(Region) %>%
  mutate(key = row_number())

# Build the map with numbers only
map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = Region)) +
  geom_sf_text(
    aes(label = key),
    size = 3,
    fontface = "bold",
    color = "black"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Combined EDSM Regions")

# Create the key table
key_table <- map_data %>%
  st_drop_geometry() %>%
  select(`#` = key, Region) %>%
  arrange(`#`)

# Split the key into two columns
half <- ceiling(nrow(key_table) / 2)

col1 <- key_table[1:half, ]
col2 <- key_table[(half + 1):nrow(key_table), ]

# Pad second column if needed
if (nrow(col2) < nrow(col1)) {
  col2 <- col2 %>%
    add_row(`#` = NA, Region = "")
}

# Create table grobs
grob1 <- tableGrob(
  col1,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    core = list(padding = unit(c(2, 4), "mm")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
)

grob2 <- tableGrob(
  col2,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    core = list(padding = unit(c(2, 4), "mm")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
)

# Display the map and key
grid.arrange(
  map_plot,
  grob1,
  grob2,
  ncol = 3,
  widths = c(2.5, 1, 1)
)



