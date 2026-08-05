#############Packages#########

library(tidyverse)
library(mgcv)
library(lubridate)
library(deltamapr)
library(sf)
library(dplyr)
library(readr)
library(data.table)
library(geosphere)
library(wql)

###########Bring in the Zoop, benthic invert, and Tides, datasets ##########
Zoop_Community <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/zooplankton_community (3).csv")


DWR_Benthic <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/DWR Benthic CPUE data 1975-2025.csv")

EMP_DWQ <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/EMP_DWQ_Data_2024.csv")

Station_Metadata_DWR <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/Station_metadata.csv")

####X2, Jersey Point Flow, and total outflows########
dayflow_70_83 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1970-1983.csv")

dayflow_84_96 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1984-1996.csv")


dayflow_97_23 <- read.csv("C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs/dayflow-results-1997-2023.csv")


# What are the actual analyte names, and what units come with them?
EMP_DWQ %>%
  count(Analyte, Result_Unit) %>%
  arrange(desc(n))

# How does Sampling_Depth vary within a single visit? (pick one station/date to inspect)
EMP_DWQ %>%
  filter(Station == "D4", Date == "1975-01-07") %>%
  select(Analyte, Sampling_Depth, Result_Value, Result_Unit)

# Overall shape of Sampling_Depth per analyte, to see if surface/bottom is a clean 2-value split
EMP_DWQ %>%
  filter(Analyte %in% c("Water Temperature", "Specific Conductance", "Dissolved Oxygen")) %>%  # adjust names once you see the real list
  count(Analyte, Sampling_Depth) %>%
  arrange(Analyte, Sampling_Depth)

###############################################################
## PART 1 — DAYFLOW AND X2
###############################################################

dayflow_97_23 <- dayflow_97_23 %>%
  rename(
    EXPORT = EXPORTS,
    DIVE   = DIVER,
    EFFECT = EFFEC,
    EFFD   = EFFDIV
  )

dayflow <- bind_rows(
  dayflow_70_83,
  dayflow_84_96,
  dayflow_97_23
) %>%
  mutate(
    Date = mdy(Date),
    OUT  = readr::parse_number(as.character(OUT))
  ) %>%
  arrange(Date)

cat("\nDayflow date range:\n")
print(range(dayflow$Date, na.rm = TRUE))
cat("\nDuplicate dates:\n")
print(sum(duplicated(dayflow$Date)))

dayflow <- dayflow %>%
  mutate(Q_for_log = pmax(OUT, 50))

X2_calc <- numeric(nrow(dayflow))
X2_calc[1] <- 75

for (i in 2:nrow(dayflow)) {
  X2_calc[i] <-
    10.16 +
    0.945 * X2_calc[i - 1] -
    1.487 * log10(dayflow$Q_for_log[i])
}

dayflow_clean <- dayflow %>%
  mutate(X2 = X2_calc) %>%
  filter(between(year(Date), 1975, 2021))

cat("\nX2 head/tail check:\n")
print(head(dayflow_clean %>% select(Date, OUT, X2)))
print(tail(dayflow_clean %>% select(Date, OUT, X2)))

###############################################################
## PART 2 — HELPER FUNCTIONS & DATA CLEANING
###############################################################

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

#-----------------------------
# Standardize Dates
#-----------------------------

Zoop_Community <- Zoop_Community %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date)
  )

DWR_Benthic <- DWR_Benthic %>%
  mutate(
    Date = ymd(substr(Date, 1, 10)),
    Year = year(Date)
  )

EMP_DWQ <- EMP_DWQ %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date)
  )

#-----------------------------
# Clean station metadata
#-----------------------------

Station_Metadata_DWR <- Station_Metadata_DWR %>%
  mutate(
    Latitude  = suppressWarnings(readr::parse_number(Latitude)),
    Longitude = suppressWarnings(readr::parse_number(Longitude))
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  distinct(StationID, .keep_all = TRUE)

#-----------------------------
# Join station coordinates into EMP_DWQ
#-----------------------------

EMP_DWQ <- EMP_DWQ %>%
  left_join(
    Station_Metadata_DWR %>% select(StationID, LocationID, Latitude, Longitude),
    by = c("Station" = "StationID")
  )

###############################################################
## PART 3 — COMMON ANALYSIS PERIOD
###############################################################

analysis_filter <- function(df) {
  df %>%
    filter(
      !is.na(Date),
      between(Year, 1975, 2021),
      !(Year >= 1994 & Year <= 2004)
    )
}

Zoop_Community <- analysis_filter(Zoop_Community)
DWR_Benthic    <- analysis_filter(DWR_Benthic)
EMP_DWQ        <- analysis_filter(EMP_DWQ)

cat("\nZooplankton years:\n"); print(range(Zoop_Community$Year))
cat("\nBenthic years:\n"); print(range(DWR_Benthic$Year))
cat("\nEMP years:\n"); print(range(EMP_DWQ$Year))
cat("\nMissing EMP coordinates:\n"); print(sum(is.na(EMP_DWQ$Latitude)))

###############################################################
## PART 4 — MONTHLY EMP ENVIRONMENT DATA
###############################################################
# FIX: "Turbidity" was reported under two incompatible instrument scales
# (NTU pre-transition, FNU post-transition) but shared one Analyte label.
# Without splitting these apart, the group-level mean below would blend
# two different measurement scales as if they were the same variable.
# Renaming here keeps them as separate columns after pivoting, so the
# reconciliation (which to use, or how to combine) is an explicit choice
# later, not something that happens silently inside a mean().

EMP_DWQ <- EMP_DWQ %>%
  mutate(
    Analyte = if_else(Analyte == "Turbidity", paste0("Turbidity_", Result_Unit), Analyte)
  )

EMP_monthly <- EMP_DWQ %>%
  mutate(
    Month = floor_date(Date, "month"),
    Result_Value = readr::parse_number(Result_Value)
  ) %>%
  filter(
    !Analyte %in% c("Latitude", "Longitude", "Weather Observations",
                    "Sky Conditions", "Wave Scale")
  ) %>%
  group_by(Station, Month, Analyte) %>%
  summarise(Value = safe_mean(Result_Value), .groups = "drop") %>%
  pivot_wider(names_from = Analyte, values_from = Value)

cat("\nMonthly EMP rows:", nrow(EMP_monthly), "\n")
cat("\nEMP monthly columns:\n"); print(names(EMP_monthly))

###############################################################
## PART 5 — BIOLOGICAL DATA
###############################################################

mysid_genera <- c("Alienacanthomysis", "Deltamysis", "Hyperacanthomysis",
                  "Neomysis", "Orientomysis")

target_genera <- c("Eurytemora", "Pseudodiaptomus", "mysid_genera",
                   "Acartiella", "Gammarus")

###############################################################
## Zooplankton
###############################################################
# FIX: sum(CPUE) -> sum(CPUE, na.rm = TRUE), so one NA reading in a group
# doesn't silently null out the whole group's summed CPUE.

Zoop_Community_avg <- Zoop_Community %>%
  mutate(
    Genus = case_when(
      Genus %in% mysid_genera ~ "mysid_genera",
      TRUE ~ Genus
    ),
    Lat_region = round(Latitude, 3),
    Long_region = round(Longitude, 3),
    Channel_Station = Station
  ) %>%
  filter(Genus %in% target_genera) %>%
  group_by(Channel_Station, Date, Genus) %>%
  summarise(
    CPUE = sum(CPUE, na.rm = TRUE),
    Latitude = safe_mean(Latitude),
    Longitude = safe_mean(Longitude),
    BottomDepth = safe_mean(BottomDepth),
    Chl = safe_mean(Chl),
    Secchi = safe_mean(Secchi),
    Temperature = safe_mean(Temperature),
    Turbidity = safe_mean(Turbidity),
    Microcystis = safe_mean(Microcystis),
    pH = safe_mean(pH),
    DO = safe_mean(DO),
    SalSurf = safe_mean(SalSurf),
    SalBott = safe_mean(SalBott),
    Volume = safe_mean(Volume),
    Source = "Zooplankton",
    .groups = "drop"
  )

###############################################################
## Benthic
###############################################################

DWR_Benthic_avg <- DWR_Benthic %>%
  mutate(
    Genus = case_when(
      Genus %in% mysid_genera ~ "mysid_genera",
      TRUE ~ Genus
    ),
    # FIX: was str_extract("^[A-Za-z]+[0-9]+"), which truncates real station
    # codes like D14A/D28A/D41A down to D14/D28/D41 - confirmed against
    # Station_metadata.csv and EMP_DWQ that these are genuinely distinct
    # stations, not sub-site variants. Stripping only the trailing
    # Center/Left/Right suffix preserves the true station code.
    Channel_Station = sub("-[CLR]$", "", Station)
  ) %>%
  filter(Genus %in% target_genera) %>%
  group_by(Channel_Station, Date, Genus) %>%
  summarise(
    CPUE = sum(MeanCPUE, na.rm = TRUE),
    Latitude = safe_mean(Latitude),
    Longitude = safe_mean(Longitude),
    TotalGrabs = safe_mean(TotalGrabs),
    Phylum = first(Phylum),
    Class = first(Class_level),
    Order = first(Order_level),
    Family = first(Family_level),
    Species = first(Species),
    Source = "Benthic",
    .groups = "drop"
  )

###############################################################
## PART 6 — COMBINE ORGANISMS
###############################################################

organisms_combined <- bind_rows(Zoop_Community_avg, DWR_Benthic_avg) %>%
  mutate(
    Month = floor_date(Date, "month"),
    Year = year(Date)
  )

###############################################################
## PART 7 — DAYFLOW JOIN
###############################################################

organisms_combined <- organisms_combined %>%
  left_join(
    dayflow_clean %>% select(Date, OUT, X2, SAC, SJR, TOT, EXPORT),
    by = "Date"
  )

cat("\nX2 coverage after Dayflow join:\n")
print(mean(!is.na(organisms_combined$X2)))

###############################################################
## PART 8 — MONTHLY EMP JOIN
###############################################################
# FIX: explicit suffix argument, so BOTH sides of any name collision
# (e.g. Turbidity_NTU/pH existing natively in Zoop data AND as EMP
# analyte-derived columns) get clear, intentional names - not just the
# EMP side, with the Zoop side left stranded under a default ".x" name.

organisms_combined <- organisms_combined %>%
  left_join(
    EMP_monthly,
    by = c("Channel_Station" = "Station", "Month"),
    suffix = c("_zoop", "_EMP")
  )

cat("\nColumns after EMP join:\n")
print(names(organisms_combined))

###############################################################
## PART 9 — CLEAN VARIABLE NAMES
###############################################################
# NOTE: column names below assume the EMP analyte strings confirmed
# earlier ("Water Temperature", "Dissolved Oxygen mg/L", "Chlorophyll a",
# "Specific Conductance", "Turbidity_NTU"/"Turbidity_FNU"). If PART 4's
# printed column list differs, adjust the left-hand names here to match.

organisms_combined <- organisms_combined %>%
  rename(
    EMP_Temperature = `Water Temperature`,
    EMP_DO           = `Dissolved Oxygen mg/L`,
    EMP_Chlorophyll  = `Chlorophyll a`,
    EMP_Conductance  = `Specific Conductance`,
    EMP_pH           = pH_EMP
  ) %>%
  rename_with(~ "EMP_Turbidity_NTU", .cols = matches("^Turbidity_NTU$")) %>%
  rename_with(~ "EMP_Turbidity_FNU", .cols = matches("^Turbidity_FNU$"))


###############################################################
## PART 10 — SALINITY CONVERSION + UNIFIED ENVIRONMENTAL VARIABLES
###############################################################
# FIX: EMP reports specific conductance (uS/cm), not salinity directly.
# Converting here so it's comparable to Zoop's SalSurf/SalBott (which
# were presumably already derived the same way upstream).
# FIX: coalescing Zoop-native and EMP-derived readings into single
# unified columns, so one GAM can reference one consistent variable
# name regardless of which program a given row came from.

organisms_combined <- organisms_combined %>%
  mutate(
    EMP_Salinity = ec2pss(EMP_Conductance / 1000, t = EMP_Temperature),
    
    Final_Temperature = coalesce(Temperature, EMP_Temperature),
    Final_Chl         = coalesce(Chl, EMP_Chlorophyll),
    Final_DO          = coalesce(DO, EMP_DO),
    Final_pH          = coalesce(pH_zoop, EMP_pH),
    # FIX: was Turbidity_zoop, which doesn't exist - the NTU/FNU rename in
    # Part 4 already prevented a name collision on "Turbidity", so Zoop's
    # own column kept its plain name instead of getting a "_zoop" suffix.
    Final_Turbidity   = coalesce(Turbidity, EMP_Turbidity_NTU, EMP_Turbidity_FNU),
    Final_SalSurf     = coalesce(SalSurf, EMP_Salinity)
  )

###############################################################
## PART 11 — DATA COVERAGE
###############################################################

cat("\nEMP-side coverage (raw EMP columns):\n")
print(colMeans(!is.na(organisms_combined %>%
                        select(EMP_Temperature, EMP_DO, EMP_Chlorophyll, EMP_Conductance))))

cat("\nCoverage of unified Final_* variables, by Source:\n")
print(
  organisms_combined %>%
    group_by(Source) %>%
    summarise(across(starts_with("Final_"), ~ mean(!is.na(.x))))
)

###############################################################
## PART 12 — FINAL CHECKS
###############################################################

cat("\nRows:\n"); print(nrow(organisms_combined))
cat("\nSources:\n"); print(table(organisms_combined$Source))
cat("\nMissing CPUE:\n"); print(sum(is.na(organisms_combined$CPUE)))
cat("\nX2 Coverage:\n"); print(mean(!is.na(organisms_combined$X2)))
cat("\nDate Range:\n"); print(range(organisms_combined$Date))

###############################################################
## PART 13 — SAVE
###############################################################

saveRDS(organisms_combined, "organisms_combined_final.rds")
write_csv(organisms_combined, "organisms_combined_final.csv")

organisms_model <- organisms_combined %>%
  mutate(
    Source = factor(Source),
    Genus = factor(Genus),
    Year = factor(Year),
    Month_num = lubridate::month(Date)
  )

saveRDS(organisms_model, "organisms_model_ready.rds")
write_csv(organisms_model, "organisms_model_ready.csv")