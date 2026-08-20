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
library(nlme)

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
##################################################################
## Pre-1994 GAM — FINAL
## X2 at moderate k=15 (extensively tested up to k=150; edf never
## stabilized below ceiling due to structural concurvity between X2
## and calendar time - not a basis-dimension problem, documented as
## a limitation rather than chased further). Year excluded (eta^2
## with X2 = 0.59, confirmed redundant). Turbidity/DO/pH excluded -
## genuinely unavailable pre-1994 (0% coverage), not a tradeoff.
##################################################################
eurytemora_pre1994 <- eurytemora_pre1994 %>%
  mutate(Channel_Station = factor(Channel_Station))

class(eurytemora_pre1994$Channel_Station)   # confirm it's now "factor"

gam_eurytemora_pre1994_final <- gam(
  CPUE ~ s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl, k = 12) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(Channel_Station, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_pre1994,
  knots = list(Month_num = c(0.5, 12.5))
)

summary(gam_eurytemora_pre1994_final)
gam.check(gam_eurytemora_pre1994_final)
concurvity(gam_eurytemora_pre1994_final, full = TRUE)

# Residual autocorrelation check
# (confirmed lag-1 ~0.47-0.48 in prior runs; AR(1) correction via gamm()
# attempted but rejected - mgcv does not fully support Tweedie + gamm(),
# confirmed by package warnings and a ~7x unexplained shift in scale
# estimate. Documented as a limitation instead: standard errors and
# significance tests likely understate true uncertainty.)
acf_result_pre1994 <- acf(residuals(gam_eurytemora_pre1994_final), plot = FALSE)
print(acf_result_pre1994$acf[1:10])

plot(gam_eurytemora_pre1994_final, select = 1, shade = TRUE)

##################################################################
## Post-2004 GAM — moderate k from the start, Year excluded (same
## structural reasoning as pre-1994), all four environmental
## variables included per earlier decision (documented limitation
## re: reduced complete-case N)
##################################################################

gam_eurytemora_post2004_final <- gam(
  CPUE ~ s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl, k = 10) +
    s(Final_Turbidity, k = 10) +
    s(Final_DO, k = 8) +
    s(Final_pH, k = 8) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(Channel_Station, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_post2004,
  knots = list(Month_num = c(0.5, 12.5))
)

summary(gam_eurytemora_post2004_final)
gam.check(gam_eurytemora_post2004_final)
concurvity(gam_eurytemora_post2004_final, full = TRUE)

# Residual autocorrelation check, same as pre-1994
acf_result_post2004 <- acf(residuals(gam_eurytemora_post2004_final), plot = FALSE)
print(acf_result_post2004$acf[1:10])







##################################################################
## PART 9- PRESENTATION-READY X2 GAM PLOT
##################################################################

library(ggplot2)

# Create prediction grid
x2_grid <- data.frame(
  X2 = seq(
    min(eurytemora_pre1994$X2, na.rm = TRUE),
    max(eurytemora_pre1994$X2, na.rm = TRUE),
    length.out = 300
  )
)

# Add values for the other predictors
x2_grid$Final_Temperature <- median(
  eurytemora_pre1994$Final_Temperature,
  na.rm = TRUE
)

x2_grid$Final_Chl <- median(
  eurytemora_pre1994$Final_Chl,
  na.rm = TRUE
)

x2_grid$Final_SalSurf <- median(
  eurytemora_pre1994$Final_SalSurf,
  na.rm = TRUE
)

x2_grid$Month_num <- 6.5

x2_grid$Longitude <- median(
  eurytemora_pre1994$Longitude,
  na.rm = TRUE
)

x2_grid$Latitude <- median(
  eurytemora_pre1994$Latitude,
  na.rm = TRUE
)

x2_grid$Year <- eurytemora_pre1994$Year[1]
x2_grid$Channel_Station <- eurytemora_pre1994$Channel_Station[1]


# Predictions from the GAM
pred <- predict(
  gam_eurytemora_pre1994,
  newdata = x2_grid,
  type = "terms",
  se.fit = TRUE
)

# Extract X2 smooth
x2_term <- grep(
  "^s\\(X2\\)",
  colnames(pred$fit),
  value = TRUE
)

x2_grid$effect <- pred$fit[, x2_term]

x2_grid$se <- pred$se.fit[, x2_term]

# 95% confidence interval
x2_grid <- x2_grid %>%
  mutate(
    lower = effect - 1.96 * se,
    upper = effect + 1.96 * se
  )


##################################################################
## PLOT
##################################################################

x2_figure <- ggplot(
  x2_grid,
  aes(x = X2, y = effect)
) +
  
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.20
  ) +
  
  geom_line(
    linewidth = 1.5
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  labs(
    title = "Eurytemora abundance shows a nonlinear relationship with X2",
    subtitle = "Preliminary generalized additive model | Pre-1994 observations",
    x = "X2",
    y = "Estimated partial effect on log(CPUE)"
  ) +
  
  theme_classic(base_size = 18) +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 16
    ),
    
    axis.title = element_text(
      size = 18,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 15
    ),
    
    plot.margin = margin(
      20, 25, 20, 20
    )
  )

x2_figure