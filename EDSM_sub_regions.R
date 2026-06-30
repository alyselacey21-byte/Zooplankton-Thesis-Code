options(repos = c(
  sbashevkin = 'https://sbashevkin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages("deltamapr")

library("deltamapr")
library("ggplot2")
library("sf")

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


install.packages("gridExtra")


options(repos = c(
  sbashevkin = 'https://sbashevkin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
library("deltamapr")
library("ggplot2")
library("sf")
library("dplyr")
library("gridExtra")  # for combining map + table

Just split the key table into two side-by-side halves:
  roptions(repos = c(
    sbashevkin = 'https://sbashevkin.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
install.packages("deltamapr")
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