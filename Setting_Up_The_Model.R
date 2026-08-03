
#############Packages#########

library(tidyverse)
library(mgcv)
library(lubridate)
library(deltamapr)
library(sf)
library(dplyr)
library(readr)
library(data.table)


###########Bring in the Zoop, benthic invert, and Tides, datasets ##########
Zoop_Communiy <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/zooplankton_community (3).csv")


DWR_Benthic <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/DWR Benthic CPUE data 1975-2025.csv")

####X2, Jersey Point Flow, and total outflows########
dayflow_70_83 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1970-1983.csv")

dayflow_84_96 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1984-1996.csv")


dayflow_97_23 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1997-2023.csv")




###########Calculation of X2 1975-1997ish#######

# --- 6. Prep outflow for the log calculation ---
# Floor outflow at 1 cfs whenever it's zero or negative (avoids log of non-positive number)
dayflow <- dayflow %>%
  mutate(Q_for_log = ifelse(OUT <= 0, 50, OUT))

# --- 7. Compute X2 recursively ---
n <- nrow(dayflow)
X2 <- numeric(n)
X2[1] <- 75   # chosen because I am not using the first 4ish years of data so the echo from this is minimal

for (t in 2:n) {
  X2[t] <- 10.16 + 0.945 * X2[t - 1] - 1.487 * log10(dayflow$Q_for_log[t])
}

dayflow$X2 <- X2

# --- 8. Discard burn-in period ---
# With 0.945 decay, ~60 days is a safe margin for convergence
dayflow_clean <- dayflow[-(1:60), ]

# --- 9. Save result ---
write_csv(dayflow_clean, "dayflow-combined-1970-2023-with-X2.csv")

# Quick check
head(dayflow_clean %>% select(Date, OUT, X2))
tail(dayflow_clean %>% select(Date, OUT, X2))
View(dayflow_clean)



# --- 1. Harmonize column names ---
# The column names drift slightly across eras. Rename the 1997-2023 file's
# columns to match the earlier two so bind_rows lines them up correctly.
dayflow_97_23 <- dayflow_97_23 %>%
  rename(
    EXPORT = EXPORTS,
    DIVE   = DIVER,
    EFFECT = EFFEC,
    EFFD   = EFFDIV
  )

# --- 2. Stack all three into one dataframe ---
dayflow <- bind_rows(dayflow_70_83, dayflow_84_96, dayflow_97_23) %>%
  arrange(Date)

# --- 3. Quick check ---
glimpse(dayflow)
range(dayflow$Date)
sum(duplicated(dayflow$Date))  # check for overlapping dates between files

# --- 4. Date Mutation ---
dayflow <- dayflow %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date)

range(dayflow$Date)

# --- 5. Save combined result ---
write_csv(dayflow, "dayflow-combined-1970-2023.csv")



#######################Save the datasets#################
saveRDS(Zoop_Communiy, "Zoop_Communiy")
saveRDS(DWR_Benthic, "DWR_Benthic")
saveRDS(dayflow_clean, "dayflow_clean") ###X2, Jersey Point Flow, and total outflows###
 #######test######
view(Zoop_Communiy)
view(DWR_Benthic)
view(dayflow_clean)

###############Determination of earliest and latest cutoff dates###########
unique(Zoop_Communiy$Year)
Zoop_Communiy #1972-2021
DWR_Benthic #1975-2025

#1975-2021 is the time period. Remove 1994-2004.



###############Combine and average the dates with the same latitude and longitude##############

# Genera of interest (raw genus names to recode as "mysid_genera")
mysid_genera <- c("Alienacanthomysis", "Deltamysis", "Hyperacanthomysis",
                  "Neomysis", "Orientomysis")

target_genera <- c("Eurytemora", "Pseudodiaptomus", "mysid_genera", "Acartiella", "Gammarus")

# --- Zoop Community ---
Zoop_Community_avg <- Zoop_Communiy %>%
  mutate(Genus_grouped = if_else(Genus %in% mysid_genera, "mysid_genera", Genus)) %>%
  filter(Genus_grouped %in% target_genera) %>%
  group_by(Latitude, Longitude, Date, Genus_grouped) %>%
  summarise(
    CPUE = sum(CPUE, na.rm = TRUE),
    across(where(is.numeric) & !all_of("CPUE"), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character) & !all_of("Genus"), ~ first(na.omit(.x))),
    .groups = "drop"
  ) %>%
  rename(Genus = Genus_grouped)

# --- DWR Benthic ---
DWR_Benthic_avg <- DWR_Benthic %>%
  mutate(Genus_grouped = if_else(Genus %in% mysid_genera, "mysid_genera", Genus)) %>%
  filter(Genus_grouped %in% target_genera) %>%
  group_by(Latitude, Longitude, Date, Genus_grouped) %>%
  summarise(
    CPUE = sum(MeanCPUE, na.rm = TRUE),
    across(where(is.numeric) & !all_of(c("MeanCPUE")), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character) & !all_of("Genus"), ~ first(na.omit(.x))),
    .groups = "drop"
  ) %>%
  rename(Genus = Genus_grouped)

# --- Fix DWR_Benthic date ---
DWR_Benthic_avg <- DWR_Benthic_avg %>%
  mutate(Date = as.Date(substr(Date, 1, 10)))

str(Zoop_Community_avg)
glimpse(Zoop_Community_avg)

unique(Zoop_Community_avg$Genus)
unique(DWR_Benthic_avg$Genus)

# Look at raw rows for a specific date/site with multiple mysid genera
Zoop_Communiy %>%
  filter(Genus %in% mysid_genera, Date == "2019-04-01", Latitude == 38.16073)

# Compare to the collapsed output for that same site/date
Zoop_Community_avg %>%
  filter(Genus == "mysid_genera", Date == "2019-04-01", Latitude == 38.16073)

nrow(Zoop_Communiy %>% filter(Genus %in% target_genera | Genus %in% mysid_genera))  # rows going in
nrow(Zoop_Community_avg)  # rows coming out — should be smaller (collapsed) or equal, never larger

summary(Zoop_Community_avg$CPUE)
sum(is.na(Zoop_Community_avg$CPUE))

View(Zoop_Community_avg)
View(DWR_Benthic_avg)




##########checks#########

# 1. Confirm Date columns
class(Zoop_Community_avg$Date)
class(DWR_Benthic_avg$Date)
head(DWR_Benthic_avg$Date)   # should now show clean yyyy-mm-dd, not the T00:00:00 version

# 2. Get FULL precision lat/long, not tibble-truncated display
options(pillar.sigfig = 10)   # forces tibble to show more digits
Zoop_Community_avg %>% select(Latitude, Longitude) %>% slice(1:5)
DWR_Benthic_avg %>% select(Latitude, Longitude) %>% slice(1:5)

# 3. Actual numeric ranges (no truncation)
range(Zoop_Community_avg$Longitude, na.rm = TRUE)
range(DWR_Benthic_avg$Longitude, na.rm = TRUE)





##################joining the datasets###########
joined <- Zoop_Community_avg %>%
  left_join(DWR_Benthic_avg, by = c("Latitude", "Longitude", "Date"))


nrow(joined)
mean(!is.na(joined$BottomDepth))

# How many location matches exist, ignoring Date entirely?
loc_only <- Zoop_Community_avg %>%
  semi_join(DWR_Benthic_avg, by = c("Latitude", "Longitude"))
nrow(loc_only)
nrow(Zoop_Community_avg)

# How many exact lat/long values are shared between the two datasets?
length(intersect(
  paste(Zoop_Community_avg$Latitude, Zoop_Community_avg$Longitude),
  paste(DWR_Benthic_avg$Latitude, DWR_Benthic_avg$Longitude)
))




#################Label the time periods and separate the genera#########

Zoop1 <- subset(dt1, Year>2004)
head(Zoop1)
tail(Zoop1)
summary(Zoop1)

Zoop100 <- subset(dt1, Year<1994)
head(Zoop100)
tail(Zoop100)
summary(Zoop100)

Zoop2 <- Zoop1 %>%
  filter(Undersampled == FALSE)
summary(Zoop2)
head(Zoop2)
tail(Zoop2)

Zoop101 <- Zoop100 %>%
  filter(Undersampled == FALSE)
summary(Zoop101)
head(Zoop101)
tail(Zoop101)


##########Check co-linearity before modeling##########

zoop_model_data %>%
  select(SalSurf, SalBott, Turbidity, Secchi, Temperature, Chl, OUT, X2, JerseyPointFlow, pH, month) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)


############Fit one GAM per taxon group, per period###########


