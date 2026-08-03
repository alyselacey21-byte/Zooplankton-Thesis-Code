
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

head(dt1)



Zoop1 <- subset(dt1, Year>2004)
head(Zoop1)
tail(Zoop1)
summary(Zoop1)

Zoop100 <- subset(dt1, Year<1994)
head(Zoop100)
tail(Zoop100)
summary(Zoop100)


Zoop2 <- Zoop1 %>% select(-BottomDepth, -Taxlifestage, -Lifestage, -AmphipodCode, -Species, -Phylum, -Class, -pH, -Family, -Taxname, -Datetime, -TowType, -SizeClass, -Volume, -Order)

Zoop101 <- Zoop100 %>% select(-BottomDepth, -Taxlifestage, -Lifestage, -AmphipodCode, -Species, -Phylum, -Class, -pH, -Family, -Taxname, -Datetime, -TowType, -SizeClass, -Volume, -Order)

library(dplyr)

Zoop3 <- Zoop2 %>%
  filter(Undersampled == FALSE)
summary(Zoop3)
head(Zoop3)
tail(Zoop3)

Zoop102 <- Zoop101 %>%
  filter(Undersampled == FALSE)



library(dplyr)
library(ggplot2)
library(lubridate)

#Gammarus Genus

#line and dot plot

Zoop4 <- Zoop3 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop4,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Acartiella

Zoop5 <- Zoop3 %>%
  filter(Genus == "Acartiella") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop5,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Acartiella CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()




#Eurytemora

Zoop6 <- Zoop3 %>%
  filter(Genus == "Eurytemora") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop6,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Eurytemora CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Pseudodiaptomus

Zoop7 <- Zoop3 %>%
  filter(Genus == "Pseudodiaptomus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop7,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Pseudodiaptomus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()



#Mysis
Zoop8 <- Zoop3 %>%
  filter(Genus %in% c(
    "Alienacanthomysis",
    "Deltamysis",
    "Hyperacanthomysis",
    "Neomysis",
    "Orientomysis"
  )) %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop8,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Mysis CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()

#######################first half of the data#######################


#Gammarus Genus

#line and dot plot

Zoop103 <- Zoop102 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop103,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Acartiella

Zoop104 <- Zoop102 %>%
  filter(Genus == "Acartiella") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop104,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Acartiella CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()




#Eurytemora

Zoop105 <- Zoop102 %>%
  filter(Genus == "Eurytemora") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop105,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Eurytemora CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Pseudodiaptomus

Zoop106 <- Zoop102 %>%
  filter(Genus == "Pseudodiaptomus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop106,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Pseudodiaptomus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()



#Mysis
Zoop107 <- Zoop102 %>%
  filter(Genus %in% c(
    "Alienacanthomysis",
    "Deltamysis",
    "Hyperacanthomysis",
    "Neomysis",
    "Orientomysis"
  )) %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop107,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Mysis CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()


###################Gammarus from bay-delta benthic data######################

library("lubridate")

infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1036/7/1696d100de056ab584579f8df4a5ecd1") 
infile2 <- sub("^https","http",infile2)

dt2 <- read_delim(infile2,
                  delim=",",
                  skip=1,
                  quote='"',
                  col_names=c("Date","Station","Year","Month","OrganismCode",
                              "MeanCPUE","TotalGrabs","Phylum","Class_level",
                              "Order_level","Family_level","Genus","Species",
                              "Common_name","Location","Latitude","Longitude"),
                  col_types=list(
                    col_datetime("%Y-%m-%dT%H:%M:%SZ"),  # <-- Z added
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(), 
                    col_number(),
                    col_number(),
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(),  
                    col_character(), 
                    col_number(),
                    col_number()),
                  na=c(" ",".","NA",""))



Gam1 <- subset(dt2, Year>2004)
head(Gam1)
tail(Gam1)
summary(Gam1)

Gam100 <- subset(dt2, Year<1994)
head(Gam100)
tail(Gam100)
summary(Gam100)


library(dplyr)

#####################Gam post 2004######################3
Gam2 <- Gam1 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(MeanCPUE = mean(MeanCPUE, na.rm = TRUE),
            .groups = "drop")

ggplot(Gam2,
       aes(x = Month,
           y = MeanCPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()




######################Gams pre 1994####################
Gam101 <- Gam100 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(MeanCPUE = mean(MeanCPUE, na.rm = TRUE),
            .groups = "drop")

ggplot(Gam101,
       aes(x = Month,
           y = MeanCPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





########################zoop genus monthly spatial distribution average old and new #####################

target_genera <- c(
  "Gammarus",
  "Acartiella",
  "Eurytemora",
  "Pseudodiaptomus",
  "Alienacanthomysis",
  "Deltamysis",
  "Hyperacanthomysis",
  "Neomysis",
  "Orientomysis"
)

zoop_map <- dt1 %>%
  filter(Genus %in% target_genera,
         CPUE > 0,
         !is.na(Latitude),
         !is.na(Longitude)) %>%
  mutate(
    Year = lubridate::year(Date),
    Group = case_when(
      Genus %in% c("Alienacanthomysis",
                   "Deltamysis",
                   "Hyperacanthomysis",
                   "Neomysis",
                   "Orientomysis") ~ "Mysis",
      TRUE ~ Genus
    )
  )




library(deltamapr)
library(sf)
library(dplyr)
library(ggplot2)

san_pablo <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion == "San Pablo Bay") %>%
  mutate(Stratum = "San Pablo Bay") %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1))

strata_boundary <- R_EDSM_Strata_1718P1 %>%
  bind_rows(san_pablo)

# 1. Convert zoop_map to an sf points object (lat/long assumed WGS84)
zoop_sf <- zoop_map %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Use strata_boundary (includes San Pablo Bay), matched to zoop_sf's CRS
strata <- strata_boundary %>% st_transform(st_crs(zoop_sf))

zoop_strata <- st_join(zoop_sf, strata, join = st_within)

# 3. Summarize mean CPUE by stratum and taxon group
strata_summary <- zoop_strata %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum)) %>%
  group_by(Stratum, Group) %>%
  summarise(MeanCPUE = mean(CPUE, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# 4a. Bar chart: mean CPUE by stratum, faceted by taxon group
ggplot(strata_summary, aes(x = reorder(Stratum, MeanCPUE), y = MeanCPUE, fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Group, scales = "free_x") +
  coord_flip() +
  labs(x = "EDSM Stratum", y = "Mean CPUE", title = "Zooplankton CPUE by EDSM Stratum") +
  theme_bw()




library(deltamapr)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

mysis_genera <- c("Alienacanthomysis", "Deltamysis", "Hyperacanthomysis",
                  "Neomysis", "Orientomysis")
target_genera <- c("Acartiella", "Eurytemora", "Pseudodiaptomus", mysis_genera)  # Gammarus removed, sourced from dt2 instead

# --- Strata boundary (reuse from before) ---
san_pablo <- R_EDSM_Subregions_Mahardja %>%
  filter(SubRegion == "San Pablo Bay") %>%
  mutate(Stratum = "San Pablo Bay") %>%
  st_transform(st_crs(R_EDSM_Strata_1718P1))

strata_boundary <- R_EDSM_Strata_1718P1 %>%
  bind_rows(san_pablo)

# --- Zooplankton genera (dt1-based), same as before ---
prep_station_month_summary <- function(data) {
  data %>%
    filter(Genus %in% target_genera, CPUE > 0,
           !is.na(Latitude), !is.na(Longitude), !is.na(Station)) %>%
    mutate(
      Group = if_else(Genus %in% mysis_genera, "Mysis", Genus),
      Month = month(Date, label = TRUE, abbr = TRUE)
    ) %>%
    group_by(Station, Latitude, Longitude, Month, Group) %>%
    summarise(MeanCPUE = mean(CPUE, na.rm = TRUE), .groups = "drop")
}

post_summary <- prep_station_month_summary(Zoop3)
pre_summary  <- prep_station_month_summary(Zoop102)

# --- Gammarus from dt2 (benthic dataset), matched to same schema ---
prep_gammarus_benthic <- function(data, year_min = NULL, year_max = NULL) {
  out <- data %>%
    filter(Genus == "Gammarus",
           !is.na(Latitude), !is.na(Longitude), !is.na(Station)) %>%
    mutate(
      Year_num = as.numeric(Year),
      # Month may come in as a number ("1"-"12") or name; handle both
      Month_num = suppressWarnings(as.integer(Month)),
      Month = if_else(
        !is.na(Month_num),
        month(Month_num, label = TRUE, abbr = TRUE),
        month(match(Month, month.name), label = TRUE, abbr = TRUE)
      ),
      Group = "Gammarus"
    )
  
  if (!is.null(year_min)) out <- out %>% filter(Year_num > year_min)
  if (!is.null(year_max)) out <- out %>% filter(Year_num < year_max)
  
  out %>%
    group_by(Station, Latitude, Longitude, Month, Group) %>%
    summarise(MeanCPUE = mean(MeanCPUE, na.rm = TRUE), .groups = "drop")
}

gammarus_post <- prep_gammarus_benthic(dt2, year_min = 2004)
gammarus_pre  <- prep_gammarus_benthic(dt2, year_max = 1994)

# --- Combine dt1-based genera with dt2-based Gammarus ---
post_summary_full <- bind_rows(post_summary, gammarus_post)
pre_summary_full  <- bind_rows(pre_summary, gammarus_pre)

# --- Convert to sf, matched to strata CRS ---
post_sf <- post_summary_full %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(strata_boundary))

pre_sf <- pre_summary_full %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(strata_boundary))

# --- Same grid map function as before ---
make_grid_map <- function(sf_data, period_label) {
  ggplot() +
    geom_sf(data = strata_boundary, fill = "grey95", color = "grey70", linewidth = 0.1) +
    geom_sf(data = sf_data, aes(color = MeanCPUE), size = 1.1, alpha = 0.85) +
    scale_color_viridis_c(trans = "log10", option = "plasma") +
    facet_grid(Group ~ Month) +
    labs(
      title = paste0("Zooplankton Abundance by Genus and Month — ", period_label),
      color = "Mean CPUE\n(log scale)"
    ) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(size = 8),
      panel.spacing = unit(0.1, "lines")
    )
}

post_grid <- make_grid_map(post_sf, "Post-2004")
pre_grid  <- make_grid_map(pre_sf, "Pre-1994")

post_grid
pre_grid

ggsave("zoop_grid_post2004.png", post_grid, width = 20, height = 9, dpi = 300)
ggsave("zoop_grid_pre1994.png", pre_grid, width = 20, height = 9, dpi = 300)