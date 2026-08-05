
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

##################################################################
## Section 1 — Biology datasets joining
##################################################################
##################################################################
## PART 1 — Build the combined Dayflow dataset and calculate X2
##################################################################
#---------------------------------------------------------------
# 1. Standardize column names across Dayflow datasets
#---------------------------------------------------------------

dayflow_97_23 <- dayflow_97_23 %>% rename(EXPORT = EXPORTS, DIVE    = DIVER, EFFECT  = EFFEC, EFFD    = EFFDIV)

#---------------------------------------------------------------
# 2. Combine all Dayflow datasets
#---------------------------------------------------------------

dayflow <- bind_rows(dayflow_70_83, dayflow_84_96, dayflow_97_23)

#---------------------------------------------------------------
# 3. Convert Date column BEFORE sorting
#---------------------------------------------------------------

dayflow <- dayflow %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% arrange(Date)

#---------------------------------------------------------------
# 4. Verify combined dataset
#---------------------------------------------------------------

glimpse(dayflow)

cat("\nDate range:\n")
print(range(dayflow$Date, na.rm = TRUE))

cat("\nDuplicate dates:\n")
print(sum(duplicated(dayflow$Date)))

#---------------------------------------------------------------
# 5. Save combined Dayflow dataset
#---------------------------------------------------------------

write_csv(dayflow, "dayflow-combined-1970-2023.csv")

#---------------------------------------------------------------
# 6. Prepare outflow for recursive X2 calculation
#
# Replace zero or negative outflow with 50 cfs so log10()
# remains defined.
#---------------------------------------------------------------

dayflow <- dayflow %>% mutate(Q_for_log = pmax(OUT, 50))

#---------------------------------------------------------------
# 7. Calculate X2
#
# Equation:
# X2(t) = 10.16 + 0.945 * X2(t-1) -
#         1.487 * log10(Q)
#
# Initial value has negligible influence after the burn-in period.
#---------------------------------------------------------------

n <- nrow(dayflow)

X2_calc <- numeric(n)
X2_calc[1] <- 75

for (i in 2:n) {X2_calc[i] <- 10.16 + 0.945 * X2_calc[i - 1] - 1.487 * log10(dayflow$Q_for_log[i])}

dayflow <- dayflow %>% mutate(X2 = X2_calc)

#---------------------------------------------------------------
# 8. Remove burn-in period
#
# Sixty days is sufficient for the recursive equation to
# converge and minimizes dependence on the initial X2 value.
#---------------------------------------------------------------

burn_in_days <- 60

dayflow_clean <- dayflow %>% slice(-(1:burn_in_days))

#---------------------------------------------------------------
# 9. Save final Dayflow dataset
#---------------------------------------------------------------

write_csv(dayflow_clean, "dayflow-combined-1970-2023-with-X2.csv")

#---------------------------------------------------------------
# 10. Diagnostics
#---------------------------------------------------------------

head(dayflow_clean %>% select(Date, OUT, X2))

tail(dayflow_clean %>% select(Date, OUT, X2))

View(dayflow_clean)



##################################################################
## PART 2 — Save original datasets
##################################################################

saveRDS(Zoop_Communiy, "Zoop_Communiy.rds")

saveRDS(DWR_Benthic, "DWR_Benthic.rds")

saveRDS(dayflow_clean, "dayflow_clean.rds")

# Quick inspection

View(Zoop_Communiy)
View(DWR_Benthic)
View(dayflow_clean)



##################################################################
## PART 3 — Determine common study period
##################################################################

#---------------------------------------------------------------
# Determine temporal coverage
#---------------------------------------------------------------

cat("\nZooplankton years:\n")
print(range(Zoop_Communiy$Year, na.rm = TRUE))

cat("\nBenthic years:\n")
print(range(DWR_Benthic$Year, na.rm = TRUE))

#---------------------------------------------------------------
# Restrict both datasets to the common analysis period
#
# Common years:
#   1975–2021
#
# Exclude:
#   1994–2004
#---------------------------------------------------------------

Zoop_Communiy <- Zoop_Communiy %>% filter(Year >= 1975, Year <= 2021, !(Year >= 1994 & Year <= 2004))

DWR_Benthic <- DWR_Benthic %>% filter(Year >= 1975, Year <= 2021, !(Year >= 1994 & Year <= 2004))

#---------------------------------------------------------------
# Verify filtering
#---------------------------------------------------------------

cat("\nZooplankton years after filtering:\n")
print(range(Zoop_Communiy$Year, na.rm = TRUE))

cat("\nBenthic years after filtering:\n")
print(range(DWR_Benthic$Year, na.rm = TRUE))

##################################################################
## PART 4 — Clean and summarize each biological dataset
##################################################################

#---------------------------------------------------------------
# Genera of interest
#---------------------------------------------------------------

mysid_genera <- c("Alienacanthomysis", "Deltamysis", "Hyperacanthomysis", "Neomysis", "Orientomysis")

target_genera <- c("Eurytemora", "Pseudodiaptomus", "mysid_genera",  "Acartiella","Gammarus")

ROUND_DIGITS <- 1


##################################################################
## Zooplankton
##################################################################

Zoop_Community_avg <- Zoop_Communiy %>% mutate(Date = as.Date(Date), Genus = if_else(Genus %in% mysid_genera, "mysid_genera", Genus), Lat_region = round(Latitude, ROUND_DIGITS), Long_region = round(Longitude, ROUND_DIGITS), Source = "Zooplankton") %>% filter(Genus %in% target_genera) %>% group_by(Lat_region, Long_region, Date,Genus) %>% summarise(CPUE = sum(CPUE, na.rm = TRUE), across(c(
  Latitude, Longitude, BottomDepth, Chl, Secchi, Temperature, Turbidity, Microcystis, pH, DO, SalSurf, SalBott, Volume), ~ mean(.x, na.rm = TRUE)), across(c(Source, Station, Year, Datetime, SampleID, TowType, AmphipodCode, Tide, SizeClass, Phylum, Class, Order, Family, Species, Taxname, Lifestage, Taxlifestage, Undersampled), first), .groups = "drop")

##################################################################
## Benthic
##################################################################

DWR_Benthic_avg <- DWR_Benthic %>% mutate(Date = as.Date(substr(Date,1,10)), Genus = if_else(Genus %in% mysid_genera, "mysid_genera", Genus), Lat_region = round(Latitude, ROUND_DIGITS), Long_region = round(Longitude, ROUND_DIGITS), Source = "Benthic") %>% filter(Genus %in% target_genera) %>% group_by(Lat_region, Long_region, Date, Genus) %>% summarise(CPUE = sum(MeanCPUE, na.rm = TRUE), across(c(Latitude, Longitude, TotalGrabs), ~ mean(.x, na.rm = TRUE)), across(c( Source, Station, Year, Month, OrganismCode, Phylum, Class_level, Order_level, Family_level, Species, Common_name, Location), first), .groups = "drop") %>% rename(Class  = Class_level, Order  = Order_level, Family = Family_level)



##################################################################
## PART 5 — Diagnostics
##################################################################

cat("\nZooplankton observations:\n")
print(nrow(Zoop_Community_avg))

cat("\nBenthic observations:\n")
print(nrow(DWR_Benthic_avg))


cat("\nZooplankton years:\n")
print(range(Zoop_Community_avg$Date))

cat("\nBenthic years:\n")
print(range(DWR_Benthic_avg$Date))


##################################################################
## Coordinate overlap
##################################################################

for (digits in 1:5) {
  z <- Zoop_Community_avg %>%
    mutate(Lat = round(Latitude, digits), Lon = round(Longitude, digits)) %>%
    distinct(Lat, Lon)
  
  b <- DWR_Benthic_avg %>%
    mutate(Lat = round(Latitude, digits), Lon = round(Longitude, digits)) %>%
    distinct(Lat, Lon)
  
  cat(
    digits, "decimal places:",
    length(intersect(paste(z$Lat, z$Lon), paste(b$Lat, b$Lon))),
    "shared locations\n"
  )
}



##################################################################
## PART 6 — Combine biological datasets
##################################################################

organisms_combined <- bind_rows(Zoop_Community_avg, DWR_Benthic_avg) 

organisms_combined <- organisms_combined %>% relocate(Source, Date, Year, Lat_region, Long_region, Latitude, Longitude, Genus, Species, CPUE)


##################################################################
## Add Dayflow variables
##################################################################

organisms_combined <- organisms_combined %>% left_join(dayflow_clean %>% select(Date, OUT, X2, SAC, SJR, TOT, EXPORT), by="Date")


##################################################################
## Final diagnostics
##################################################################

cat("\nCombined rows:\n")
print(nrow(organisms_combined))

cat("\nRows by source:\n")
print(table(organisms_combined$Source))

cat("\nDayflow coverage:\n")
print(mean(!is.na(organisms_combined$X2)))

cat("\nMissing CPUE:\n")
print(sum(is.na(organisms_combined$CPUE)))

View(organisms_combined)

Zoop_Communiy %>%
  mutate(
    Genus = if_else(Genus %in% mysid_genera, "mysid_genera", Genus),
    Lat_region = round(Latitude, ROUND_DIGITS),
    Long_region = round(Longitude, ROUND_DIGITS)
  ) %>%
  filter(Genus %in% target_genera) %>%
  distinct(Lat_region, Long_region, Date, Genus) %>%
  nrow()
######86459 check#####

colSums(is.na(organisms_combined))

organisms_combined %>%
  filter(is.na(Species)) %>%
  count(Source)

Zoop_Communiy %>%
  filter(Genus %in% target_genera | Genus %in% mysid_genera) %>%
  summarise(pct_na_species = mean(is.na(Species)))


##################################################################
## Section 2 - checking for tides
##################################################################

organisms_combined %>% filter(Source == "Zooplankton") %>% summarise(pct_missing_tide = mean(is.na(Tide)))
#8.42% missing

organisms_combined %>%
  filter(Source == "Benthic") %>%
  summarise(pct_missing_tide = mean(is.na(Tide)))
#100% missing

view(organisms_combined)

##################################################################
## Section 3 - checking for collinearity
##################################################################






