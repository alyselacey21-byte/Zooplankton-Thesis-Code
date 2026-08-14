############Making the GAM For Eurytemora###########

#####Packages######

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
library(car)
library(corrplot)  
library(energy)    
library(GGally) 

#########Using organisms_model_reduced from the script  Setting_Up_The_Model##############
###############################################################
## PART 1 — Confirm source
###############################################################
eurytemora_data <- organisms_model_reduced %>%
  filter(Genus == "Eurytemora") %>%
  droplevels()

cat("\nRows:\n"); print(nrow(eurytemora_data))
cat("\nSource breakdown:\n"); print(table(eurytemora_data$Source))

###############################################################
## PART 2 - check completeness
###############################################################

eurytemora_data %>%
  summarise(across(c(X2, Final_Temperature, Final_Chl, Final_DO, Final_pH, Final_Turbidity, Final_SalSurf), 
                   ~ mean(!is.na(.x))))

eurytemora_complete <- eurytemora_data %>%
  drop_na(X2, Final_Temperature, Final_Chl, Final_DO, Final_pH, Final_Turbidity, Final_SalSurf)

cat("\nComplete-case rows for GAM fitting:\n")
print(nrow(eurytemora_complete))

eurytemora_reduced_complete <- eurytemora_data %>%
  drop_na(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_SalSurf)

cat("\nComplete-case rows without DO/pH:\n")
print(nrow(eurytemora_reduced_complete))

# Individual coverage, for reference (X2/Temperature already known to be ~100%/99%)
eurytemora_data %>%
  summarise(across(c(Final_Chl, Final_Turbidity, Final_SalSurf), ~ mean(!is.na(.x))))

# Test dropping Turbidity too, keeping Chl
eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Chl, Final_SalSurf) %>% nrow()

# Test dropping Chl too, keeping Turbidity
eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Turbidity, Final_SalSurf) %>% nrow()

# Test dropping both Chl and Turbidity - down to the near-universal variables only
eurytemora_data %>% drop_na(X2, Final_Temperature, Final_SalSurf) %>% nrow()


# Full data year distribution (baseline to compare against)
full_years <- eurytemora_data %>% count(Year) %>% rename(n_full = n)

# Chl-only-kept subset (18,702 rows)
chl_only_complete <- eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Chl, Final_SalSurf)
chl_years <- chl_only_complete %>% count(Year) %>% rename(n_chl_subset = n)

# Compare proportionally, not just raw counts
year_compare <- full_years %>%
  left_join(chl_years, by = "Year") %>%
  mutate(
    n_chl_subset = replace_na(n_chl_subset, 0),
    pct_of_year_retained = n_chl_subset / n_full
  )

print(year_compare, n = Inf)

# Same check for Turbidity-only-kept subset (10,472 rows)
turb_only_complete <- eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Turbidity, Final_SalSurf)
turb_years <- turb_only_complete %>% count(Year) %>% rename(n_turb_subset = n)

year_compare2 <- full_years %>%
  left_join(turb_years, by = "Year") %>%
  mutate(
    n_turb_subset = replace_na(n_turb_subset, 0),
    pct_of_year_retained = n_turb_subset / n_full
  )

print(year_compare2, n = Inf)


##################################################################
## Part 3 - Split Eurytemora data into pre-1994 and post-2004 eras
##################################################################

eurytemora_pre1994 <- eurytemora_data %>%
  filter(as.numeric(as.character(Year)) < 1994) %>%
  droplevels()

eurytemora_post2004 <- eurytemora_data %>%
  filter(as.numeric(as.character(Year)) > 2004) %>%
  droplevels()

cat("\nPre-1994 rows:\n"); print(nrow(eurytemora_pre1994))
cat("Pre-1994 years:\n"); print(range(as.numeric(as.character(eurytemora_pre1994$Year))))

cat("\nPost-2004 rows:\n"); print(nrow(eurytemora_post2004))
cat("Post-2004 years:\n"); print(range(as.numeric(as.character(eurytemora_post2004$Year))))

##################################################################
## Part 4 - Check coverage WITHIN each era before building formulas
##################################################################
# (coverage rates can differ from the full-dataset numbers once
# split - don't assume the earlier Chl/Turbidity picture still holds)

cat("\nCoverage, pre-1994:\n")
print(eurytemora_pre1994 %>%
        summarise(across(c(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_SalSurf), ~ mean(!is.na(.x)))))

cat("\nCoverage, post-2004:\n")
print(eurytemora_post2004 %>%
        summarise(across(c(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_SalSurf), ~ mean(!is.na(.x)))))

##################################################################
## Part 5 - Check post 2004
##################################################################
cat("\nPost-2004 complete cases with both Chl and Turbidity:\n")
print(eurytemora_post2004 %>% drop_na(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases, Turbidity only (drop Chl):\n")
print(eurytemora_post2004 %>% drop_na(X2, Final_Temperature, Final_Turbidity, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases, Chl only (drop Turbidity):\n")
print(eurytemora_post2004 %>% drop_na(X2, Final_Temperature, Final_Chl, Final_SalSurf) %>% nrow())

print(eurytemora_pre1994 %>%
        summarise(across(c(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_DO, Final_pH, Final_SalSurf),
                         ~ mean(!is.na(.x)))), width = Inf)
##################################################################
## Part 6 - run the GAMs
##################################################################

gam_eurytemora_pre1994 <- gam(
  CPUE ~ s(X2, k = 5) +
    s(Final_Temperature, k = 5) +
    s(Final_Chl, k = 5) +
    s(Final_SalSurf, k = 5) +
    s(Month_num, bs = "cc", k = 8) +
    s(Year, bs = "re") +
    s(Channel_Station, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_pre1994
)

summary(gam_eurytemora_pre1994)
gam.check(gam_eurytemora_pre1994)
concurvity(gam_eurytemora_pre1994, full = TRUE)




# Post-2004: all four included
gam_eurytemora_post2004_full <- gam(
  CPUE ~ s(X2, k = 5) +
    s(Final_Temperature, k = 5) +
    s(Final_Chl, k = 5) +
    s(Final_Turbidity, k = 5) +
    s(Final_DO, k = 5) +
    s(Final_pH, k = 5) +
    s(Final_SalSurf, k = 5) +
    s(Month_num, bs = "cc", k = 8) +
    s(Year, bs = "re") +
    s(Channel_Station, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_post2004
)

summary(gam_eurytemora_post2004_full)
gam.check(gam_eurytemora_post2004_full)
concurvity(gam_eurytemora_post2004_full, full = TRUE)