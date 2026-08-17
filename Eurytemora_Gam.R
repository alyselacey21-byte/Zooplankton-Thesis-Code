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

##################################################################
## EURYTEMORA GAM SETUP
## Uses organisms_model_reduced from Setting_Up_The_Model
##################################################################

##################################################################
## PART 1 — Subset to Eurytemora, confirm source
##################################################################

eurytemora_data <- organisms_model_reduced %>%
  filter(Genus == "Eurytemora") %>%
  droplevels()

cat("\nRows:\n"); print(nrow(eurytemora_data))
cat("\nSource breakdown:\n"); print(table(eurytemora_data$Source))
# Expect: all rows are "Zooplankton" - Eurytemora doesn't appear in Benthic data.

##################################################################
## PART 2 — Predictor completeness (full dataset, pre-split)
##################################################################

env_vars <- c("X2", "Final_Temperature", "Final_Chl", "Final_DO",
              "Final_pH", "Final_Turbidity", "Final_SalSurf")

cat("\nCoverage, full Eurytemora dataset:\n")
print(eurytemora_data %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

# Complete-case counts under different variable combinations, to see
# which predictors are the real bottleneck before committing to a formula
cat("\nComplete cases - all 7 variables:\n")
print(eurytemora_data %>% drop_na(all_of(env_vars)) %>% nrow())

cat("\nComplete cases - drop DO/pH only:\n")
print(eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Chl, Final_Turbidity, Final_SalSurf) %>% nrow())

cat("\nComplete cases - drop Turbidity, keep Chl:\n")
print(eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Chl, Final_SalSurf) %>% nrow())

cat("\nComplete cases - drop Chl, keep Turbidity:\n")
print(eurytemora_data %>% drop_na(X2, Final_Temperature, Final_Turbidity, Final_SalSurf) %>% nrow())

cat("\nComplete cases - X2/Temperature/SalSurf only (near-universal vars):\n")
print(eurytemora_data %>% drop_na(X2, Final_Temperature, Final_SalSurf) %>% nrow())

##################################################################
## PART 3 — Year-by-year retention check
##################################################################
# Confirms WHY coverage is low for some variables: era-dependent
# instrumentation changes, not random missingness. Compares the full
# per-year row count against the per-year count retained under each
# variable-inclusion scenario.

full_years <- eurytemora_data %>% count(Year) %>% rename(n_full = n)

chl_years <- eurytemora_data %>%
  drop_na(X2, Final_Temperature, Final_Chl, Final_SalSurf) %>%
  count(Year) %>% rename(n_chl_subset = n)

turb_years <- eurytemora_data %>%
  drop_na(X2, Final_Temperature, Final_Turbidity, Final_SalSurf) %>%
  count(Year) %>% rename(n_turb_subset = n)

year_compare <- full_years %>%
  left_join(chl_years, by = "Year") %>%
  left_join(turb_years, by = "Year") %>%
  mutate(
    n_chl_subset  = replace_na(n_chl_subset, 0),
    n_turb_subset = replace_na(n_turb_subset, 0),
    pct_chl_retained  = n_chl_subset / n_full,
    pct_turb_retained = n_turb_subset / n_full
  )

cat("\nPer-year retention, Chl-inclusive vs Turbidity-inclusive subsets:\n")
print(year_compare, n = Inf)
# Confirmed pattern: Chl is well-covered 1975-1993, drops sharply 2005-2018,
# partially recovers 2019-2021. Turbidity is entirely absent pre-1994,
# then becomes well-covered from ~2010 onward. This is why the data is
# split into pre-1994 / post-2004 eras below, using different predictor
# sets appropriate to each era's actual instrumentation.

##################################################################
## PART 4 — Split into pre-1994 and post-2004 eras
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
## PART 5 — Within-era coverage (confirms era-specific predictor sets)
##################################################################

cat("\nCoverage, pre-1994:\n")
print(eurytemora_pre1994 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)
# Confirmed: Final_Turbidity, Final_DO, Final_pH are all 0% pre-1994.
# Not a sample-size tradeoff - these instruments did not exist yet in
# this era. Pre-1994 model therefore uses X2, Temperature, Chl, SalSurf only.

cat("\nCoverage, post-2004:\n")
print(eurytemora_post2004 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

cat("\nPost-2004 complete cases, all four (Chl+Turbidity+DO+pH):\n")
print(eurytemora_post2004 %>% drop_na(all_of(env_vars)) %>% nrow())
# DECISION: include all four despite reduced N (documented limitation),
# per full predictor-set priority over sample size for this analysis.

##################################################################
## PART 6 — LM sanity check
##################################################################
# Not the final model - a quick linear pass to confirm predictor
# directions make sense and to visually demonstrate why a Tweedie GAM
# (not a Gaussian LM) is appropriate for this right-skewed CPUE response.

lm_pre1994 <- lm(
  CPUE ~ X2 + Final_Temperature + Final_Chl + Final_SalSurf + Month_num + Year,
  data = eurytemora_pre1994
)
summary(lm_pre1994)
par(mfrow = c(2, 2)); plot(lm_pre1994); par(mfrow = c(1, 1))

lm_post2004 <- lm(
  CPUE ~ X2 + Final_Temperature + Final_Turbidity + Final_SalSurf + Month_num + Year,
  data = eurytemora_post2004
)
summary(lm_post2004)
par(mfrow = c(2, 2)); plot(lm_post2004); par(mfrow = c(1, 1))

##################################################################
## PART 7 — Outlier / extreme-value check (pre-1994)
##################################################################

cat("\nTop 10 largest CPUE values, pre-1994:\n")
print(
  eurytemora_pre1994 %>%
    arrange(desc(CPUE)) %>%
    select(Date, Channel_Station, CPUE, X2, Final_Chl, Final_Temperature, Final_SalSurf) %>%
    slice(1:10)
)
# Checked: extreme values cluster in Apr-Jun and Nov (real seasonal pulse
# windows for Eurytemora), not random - consistent with genuine bloom
# events rather than data-entry errors. No exclusions warranted.

cat("\nCPUE by Chl-missingness (checking whether missing Chl skews toward high catches):\n")
print(
  eurytemora_pre1994 %>%
    mutate(chl_missing = is.na(Final_Chl)) %>%
    group_by(chl_missing) %>%
    summarise(mean_cpue = mean(CPUE, na.rm = TRUE), median_cpue = median(CPUE, na.rm = TRUE), n = n())
)
# Checked: means/medians are close between groups (571 vs 616; 21.6 vs
# 26.9) - Chl missingness is not meaningfully tied to catch size.

##################################################################
## PART 8 — GAMs
##################################################################

# Pre-1994: Turbidity/DO/pH excluded - genuinely unavailable (0% coverage),
# not a sample-size tradeoff.
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

# Post-2004: all four environmental variables included by choice, despite
# reduced complete-case N (see Part 5) - documented as a limitation rather
# than silently dropping variables for sample size.
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