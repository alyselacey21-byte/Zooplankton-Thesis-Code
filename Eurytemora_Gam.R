

############ Making the GAM For Eurytemora ###########
## Consolidated version - all parts reordered so every object is
## defined/created before it's referenced. Run top-to-bottom.

##### Packages ######

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
library(gratia)
library(patchwork)


##################################################################
## EURYTEMORA GAM SETUP
## Uses organisms_model_reduced from Setting_Up_The_Model
##################################################################
view(organisms_model_reduced)

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

##################################################################
## PART 4 — Lag-Chl helper function (defined once, used later)
##################################################################

add_lagged_chl <- function(data, lag_days) {
  
  dt <- as.data.table(data)
  dt[, row_id := .I]
  
  chl_lookup <- dt[!is.na(Final_Chl), .(Channel_Station, Date, Final_Chl)]
  setkey(chl_lookup, Channel_Station, Date)
  
  dt[, target_date := Date - lag_days]
  setkey(dt, Channel_Station, target_date)
  
  joined <- chl_lookup[dt, on = .(Channel_Station, Date = target_date),
                       roll = TRUE,
                       .(row_id, i.Date, Channel_Station,
                         Final_Chl_lag = x.Final_Chl)]
  
  setnames(joined, "i.Date", "Date")
  setorder(joined, row_id)
  
  out <- data
  out$Final_Chl_lag <- joined$Final_Chl_lag[match(seq_len(nrow(out)), joined$row_id)]
  out
}

##################################################################
## PART 5 — Split into pre-1994 and post-2004 eras
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
## PART 6 — Apply chosen 4-week (28-day) Chl lag, both eras
##################################################################
# NOTE: a common-N model-selection comparison (AIC/REML/dev_expl on
# lags 0-140 days, all fit to an identical row set) found lag=0
# outperformed every tested lag, with no dip near 28 days. This
# 28-day lag is applied per an explicit deliberate decision, not
# because model selection favored it. See PART 14/15 below for a
# full 0-lag comparison model + figure, fit and plotted the same way.

eurytemora_pre1994 <- add_lagged_chl(eurytemora_pre1994, lag_days = 28) %>%
  rename(Final_Chl_lag28 = Final_Chl_lag)

cat("\nRows with valid 28-day lagged Chl, pre-1994:\n")
print(sum(!is.na(eurytemora_pre1994$Final_Chl_lag28)))
cat("(vs. contemporaneous Chl non-NA count:\n")
print(sum(!is.na(eurytemora_pre1994$Final_Chl)))
cat(")\n")

eurytemora_post2004 <- add_lagged_chl(eurytemora_post2004, lag_days = 28) %>%
  rename(Final_Chl_lag28 = Final_Chl_lag)

cat("\nRows with valid 28-day lagged Chl, post-2004:\n")
print(sum(!is.na(eurytemora_post2004$Final_Chl_lag28)))
cat("(vs. contemporaneous Chl non-NA count:\n")
print(sum(!is.na(eurytemora_post2004$Final_Chl)))
cat(")\n")

##################################################################
## PART 7 — Within-era coverage
##################################################################

cat("\nCoverage, pre-1994:\n")
print(eurytemora_pre1994 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

cat("\nCoverage, post-2004:\n")
print(eurytemora_post2004 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

cat("\nPost-2004 complete cases, all four (Chl+Turbidity+DO+pH):\n")
print(eurytemora_post2004 %>% drop_na(all_of(env_vars)) %>% nrow())

##################################################################
## PART 7B — Re-check complete-case tradeoffs for post-2004,
## using the LAGGED Chl variable
##################################################################

cat("\nPost-2004 complete cases - all four (Chl_lag28+Turbidity+DO+pH):\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag28, Final_Turbidity,
                Final_DO, Final_pH, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop DO/pH only:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag28, Final_Turbidity,
                Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop DO/pH, drop Turbidity too:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag28, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop Chl_lag28, keep Turbidity+DO+pH:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Turbidity, Final_DO,
                Final_pH, Final_SalSurf) %>% nrow())

###################################################################
## PART 8 — ASSIGN EDSM STRATA
##################################################################

data("R_EDSM_Strata_1718P1")

# ================================================================
# PRE-1994
# ================================================================

eurytemora_sf <- eurytemora_pre1994 %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

eurytemora_sf <- st_transform(eurytemora_sf, st_crs(R_EDSM_Strata_1718P1))

eurytemora_sf <- st_join(
  eurytemora_sf,
  R_EDSM_Strata_1718P1 %>% select(Stratum),
  join = st_within,
  left = TRUE
)

eurytemora_pre1994 <- eurytemora_sf %>%
  st_drop_geometry() %>%
  mutate(R_EDSM_Strata_1718P1 = factor(Stratum)) %>%
  select(-Stratum)

cat("\nPre-1994 EDSM strata coverage:\n")
print(mean(!is.na(eurytemora_pre1994$R_EDSM_Strata_1718P1)))
print(table(eurytemora_pre1994$R_EDSM_Strata_1718P1, useNA = "ifany"))

# ================================================================
# POST-2004
# ================================================================

eurytemora_post2004_sf <- eurytemora_post2004 %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

eurytemora_post2004_sf <- st_transform(eurytemora_post2004_sf, st_crs(R_EDSM_Strata_1718P1))

eurytemora_post2004_sf <- st_join(
  eurytemora_post2004_sf,
  R_EDSM_Strata_1718P1 %>% select(Stratum),
  join = st_within,
  left = TRUE
)

eurytemora_post2004 <- eurytemora_post2004_sf %>%
  st_drop_geometry() %>%
  mutate(R_EDSM_Strata_1718P1 = factor(Stratum)) %>%
  select(-Stratum)

cat("\nPost-2004 EDSM strata coverage:\n")
print(mean(!is.na(eurytemora_post2004$R_EDSM_Strata_1718P1)))
print(table(eurytemora_post2004$R_EDSM_Strata_1718P1, useNA = "ifany"))

##################################################################
## PART 9 — LM sanity check
##################################################################

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
## PART 10 — Outlier / extreme-value check (pre-1994)
##################################################################

cat("\nTop 10 largest CPUE values, pre-1994:\n")
print(
  eurytemora_pre1994 %>%
    arrange(desc(CPUE)) %>%
    select(Date, Channel_Station, CPUE, X2, Final_Chl, Final_Temperature, Final_SalSurf) %>%
    slice(1:10)
)

cat("\nCPUE by Chl-missingness:\n")
print(
  eurytemora_pre1994 %>%
    mutate(chl_missing = is.na(Final_Chl)) %>%
    group_by(chl_missing) %>%
    summarise(mean_cpue = mean(CPUE, na.rm = TRUE), median_cpue = median(CPUE, na.rm = TRUE), n = n())
)

##################################################################
## PART 11 — GAMs (28-day lagged Chl + EDSM strata)
##################################################################

eurytemora_pre1994 <- eurytemora_pre1994 %>%
  mutate(Channel_Station = factor(Channel_Station))

class(eurytemora_pre1994$Channel_Station)

gam_eurytemora_pre1994_final <- gam(
  CPUE ~ s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl_lag28, k = 12) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(Channel_Station, bs = "re") +
    s(R_EDSM_Strata_1718P1, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_pre1994,
  knots = list(Month_num = c(0.5, 12.5))
)

summary(gam_eurytemora_pre1994_final)
gam.check(gam_eurytemora_pre1994_final)
concurvity(gam_eurytemora_pre1994_final, full = TRUE)

acf_result_pre1994 <- acf(residuals(gam_eurytemora_pre1994_final), plot = FALSE)
print(acf_result_pre1994$acf[1:10])

plot(gam_eurytemora_pre1994_final, select = 1, shade = TRUE)


##################################################################
## PART 11B — POST-2004 GAM
## Lagged chlorophyll + environmental variables + EDSM strata
##################################################################

eurytemora_post2004 <- eurytemora_post2004 %>%
  mutate(
    Channel_Station = factor(Channel_Station),
    R_EDSM_Strata_1718P1 = factor(R_EDSM_Strata_1718P1)
  )

cat("\nEDSM strata coverage, post-2004:\n")
print(mean(!is.na(eurytemora_post2004$R_EDSM_Strata_1718P1)))
print(table(eurytemora_post2004$R_EDSM_Strata_1718P1, useNA = "ifany"))

cat("\nPredictor coverage, post-2004:\n")
print(
  colMeans(!is.na(
    eurytemora_post2004 %>%
      select(CPUE, X2, Final_Temperature, Final_Chl_lag28, Final_Turbidity,
             Final_DO, Final_pH, Final_SalSurf, Month_num,
             Channel_Station, R_EDSM_Strata_1718P1)
  ))
)

gam_eurytemora_post2004_final <- gam(
  CPUE ~
    s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl_lag28, k = 10) +
    s(Final_Turbidity, k = 10) +
    s(Final_DO, k = 8) +
    s(Final_pH, k = 8) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(Channel_Station, bs = "re") +
    s(R_EDSM_Strata_1718P1, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_post2004,
  knots = list(Month_num = c(0.5, 12.5))
)

summary(gam_eurytemora_post2004_final)
gam.check(gam_eurytemora_post2004_final)
concurvity(gam_eurytemora_post2004_final, full = TRUE)

acf_result_post2004 <- acf(residuals(gam_eurytemora_post2004_final), plot = FALSE)
print(acf_result_post2004$acf[1:10])


##################################################################
## PART 12 — PRESENTATION-READY GAM SMOOTH PLOTS (28-day lag, pre-1994)
#################################################################

library(mgcv)
library(ggplot2)
library(dplyr)
library(patchwork)

get_smooth_data <- function(model, variable, label, n = 200) {
  
  model_data <- model$model
  
  x_values <- seq(
    min(model_data[[variable]], na.rm = TRUE),
    max(model_data[[variable]], na.rm = TRUE),
    length.out = n
  )
  
  newdata <- data.frame(
    X2 = median(model_data$X2, na.rm = TRUE),
    Final_Temperature = median(model_data$Final_Temperature, na.rm = TRUE),
    Final_Chl_lag28 = median(model_data$Final_Chl_lag28, na.rm = TRUE),
    Final_SalSurf = median(model_data$Final_SalSurf, na.rm = TRUE),
    Month_num = 6,
    Channel_Station = model_data$Channel_Station[1],
    R_EDSM_Strata_1718P1 = model_data$R_EDSM_Strata_1718P1[1]
  )
  
  newdata <- newdata[rep(1, length(x_values)), ]
  newdata[[variable]] <- x_values
  
  pred <- predict(model, newdata = newdata, type = "terms", se.fit = TRUE)
  
  term_name <- paste0("s(", variable, ")")
  term_index <- which(colnames(pred$fit) == term_name)
  
  fit <- pred$fit[, term_index]
  se <- pred$se.fit[, term_index]
  
  data.frame(
    x = x_values, fit = fit, se = se,
    lower = fit - 1.96 * se, upper = fit + 1.96 * se,
    variable = label
  )
}

d_x2 <- get_smooth_data(gam_eurytemora_pre1994_final, "X2", "Location")
d_temp <- get_smooth_data(gam_eurytemora_pre1994_final, "Final_Temperature", "Temperature")
d_chl <- get_smooth_data(gam_eurytemora_pre1994_final, "Final_Chl_lag28", "Chlorophyll-a")
d_sal <- get_smooth_data(gam_eurytemora_pre1994_final, "Final_SalSurf", "Surface salinity")
d_month <- get_smooth_data(gam_eurytemora_pre1994_final, "Month_num", "Season")

make_gam_plot <- function(dat, title, xlab) {
  
  ggplot(dat, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1.2) +
    labs(title = title, x = xlab, y = "Effect on log(CPUE)") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

g_x2 <- make_gam_plot(d_x2, "Location", "X2 position")
g_temp <- make_gam_plot(d_temp, "Temperature", "Temperature (°C)")
g_chl <- make_gam_plot(d_chl, "Chlorophyll-a", "28-day lagged chlorophyll-a")
g_sal <- make_gam_plot(d_sal, "Surface salinity", "Surface salinity")
g_month <- make_gam_plot(d_month, "Season", "Month")

gam_figure <- (
  g_x2 | g_temp | g_chl
) / (
  g_sal | g_month
)

# FIX: this figure previously printed with no overall title/subtitle,
# unlike the post-2004 figure in Part 13 - added plot_annotation here
# for consistency across both era figures.
gam_figure +
  plot_annotation(
    title = "Eurytemora GAM relationships — Pre-1994 period",
    subtitle = "1975–1993 (28-day lagged chlorophyll-a)"
  )


##################################################################
## PART 13 — PRESENTATION-READY POST-2004 GAM SMOOTH PLOTS (28-day lag)
##################################################################

library(mgcv)
library(ggplot2)
library(dplyr)
library(patchwork)

get_post2004_smooth_data <- function(model, variable, label, n = 200) {
  
  model_data <- model$model
  
  x_values <- seq(
    min(model_data[[variable]], na.rm = TRUE),
    max(model_data[[variable]], na.rm = TRUE),
    length.out = n
  )
  
  newdata <- data.frame(
    X2 = median(model_data$X2, na.rm = TRUE),
    Final_Temperature = median(model_data$Final_Temperature, na.rm = TRUE),
    Final_Chl_lag28 = median(model_data$Final_Chl_lag28, na.rm = TRUE),
    Final_Turbidity = median(model_data$Final_Turbidity, na.rm = TRUE),
    Final_DO = median(model_data$Final_DO, na.rm = TRUE),
    Final_pH = median(model_data$Final_pH, na.rm = TRUE),
    Final_SalSurf = median(model_data$Final_SalSurf, na.rm = TRUE),
    Month_num = 6,
    Channel_Station = model_data$Channel_Station[1],
    R_EDSM_Strata_1718P1 = model_data$R_EDSM_Strata_1718P1[1]
  )
  
  newdata <- newdata[rep(1, length(x_values)), ]
  newdata[[variable]] <- x_values
  
  pred <- predict(model, newdata = newdata, type = "terms", se.fit = TRUE)
  
  term_name <- paste0("s(", variable, ")")
  term_index <- which(colnames(pred$fit) == term_name)
  
  fit <- pred$fit[, term_index]
  se <- pred$se.fit[, term_index]
  
  data.frame(
    x = x_values, fit = fit, se = se,
    lower = fit - 1.96 * se, upper = fit + 1.96 * se,
    variable = label
  )
}

d_x2_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "X2", "Location")
d_temp_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_Temperature", "Temperature")
d_chl_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_Chl_lag28", "Chlorophyll-a")
d_turb_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_Turbidity", "Turbidity")
d_do_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_DO", "Dissolved oxygen")
d_ph_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_pH", "pH")
d_sal_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_SalSurf", "Surface salinity")
d_month_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Month_num", "Season")

make_post2004_gam_plot <- function(dat, title, xlab) {
  
  ggplot(dat, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1.2) +
    labs(title = title, x = xlab, y = "Effect on log(CPUE)") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

g_x2_post <- make_post2004_gam_plot(d_x2_post, "Location", "X2 position")
g_temp_post <- make_post2004_gam_plot(d_temp_post, "Temperature", "Temperature (°C)")
g_chl_post <- make_post2004_gam_plot(d_chl_post, "Chlorophyll-a", "28-day lagged chlorophyll-a")
g_turb_post <- make_post2004_gam_plot(d_turb_post, "Turbidity", "Turbidity")
g_do_post <- make_post2004_gam_plot(d_do_post, "Dissolved oxygen", "Dissolved oxygen")
g_ph_post <- make_post2004_gam_plot(d_ph_post, "pH", "pH")
g_sal_post <- make_post2004_gam_plot(d_sal_post, "Surface salinity", "Surface salinity")
g_month_post <- make_post2004_gam_plot(d_month_post, "Season", "Month")

gam_figure_post2004 <- (
  g_x2_post | g_temp_post | g_chl_post | g_turb_post
) / (
  g_do_post | g_ph_post | g_sal_post | g_month_post
)

gam_figure_post2004 +
  plot_annotation(
    title = "Eurytemora GAM relationships — Post-2004 period",
    subtitle = "2005–present (28-day lagged chlorophyll-a)"
  )


##################################################################
## PART 14 — ZERO-LAG (CONTEMPORANEOUS) CHLOROPHYLL GAMs
## Same model structure as Part 11/11B, but using same-day Final_Chl
## instead of Final_Chl_lag28. This is the direct comparison case:
## the common-N model-selection test (documented in Part 6) found
## lag = 0 outperformed every tested lag on AIC/REML/deviance
## explained, so these models represent the data-preferred version
## alongside the 28-day-lag models fit above.
##################################################################

# ================================================================
# Pre-1994, zero-lag Chl
# ================================================================

gam_eurytemora_pre1994_lag0 <- gam(
  CPUE ~ s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl, k = 12) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(Channel_Station, bs = "re") +
    s(R_EDSM_Strata_1718P1, bs = "re"),
  family = tw(),
  method = "REML",
  data = eurytemora_pre1994,
  knots = list(Month_num = c(0.5, 12.5))
)

cat("\n=== Pre-1994, ZERO-LAG Chl model ===\n")
summary(gam_eurytemora_pre1994_lag0)
gam.check(gam_eurytemora_pre1994_lag0)
concurvity(gam_eurytemora_pre1994_lag0, full = TRUE)

# ================================================================
# Post-2004, zero-lag Chl
# ================================================================

cat("\nStarting reduced Post-2004 zero-lag GAM...\n")
flush.console()

gam_eurytemora_post2004_lag0_test <- mgcv::gam(
  CPUE ~
    s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl, k = 10) +
    s(Final_Turbidity, k = 10) +
    s(Final_DO, k = 8) +
    s(Final_pH, k = 8) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(R_EDSM_Strata_1718P1, bs = "re"),
  family = mgcv::tw(),
  method = "REML",
  data = post2004_complete,
  knots = list(Month_num = c(0.5, 12.5)),
  control = mgcv::gam.control(trace = TRUE)
)

cat("\nFinished reduced Post-2004 zero-lag GAM.\n")
flush.console()

print(summary(gam_eurytemora_post2004_lag0_test))


gam_eurytemora_post2004_lag0 <- mgcv::gam(
  CPUE ~
    s(X2, k = 15) +
    s(Final_Temperature, k = 8) +
    s(Final_Chl, k = 10) +
    s(Final_Turbidity, k = 10) +
    s(Final_DO, k = 8) +
    s(Final_pH, k = 8) +
    s(Final_SalSurf, k = 15) +
    s(Month_num, bs = "cc", k = 10) +
    s(R_EDSM_Strata_1718P1, bs = "re"),
  family = mgcv::tw(),
  method = "REML",
  data = post2004_complete,
  knots = list(Month_num = c(0.5, 12.5)),
  control = mgcv::gam.control(trace = TRUE)
)

summary(gam_eurytemora_post2004_lag0)
gam.check(gam_eurytemora_post2004_lag0)
concurvity(gam_eurytemora_post2004_lag0, full = TRUE)



##################################################################
## PART 15 — PRESENTATION-READY GAM SMOOTH PLOTS: ZERO-LAG MODELS
## Uses a generic smooth-extraction function (builds newdata from
## whatever columns the model actually used, rather than hardcoding
## column names) so the same function works for both eras and for
## any future model without editing the function itself.
##################################################################

get_smooth_data_generic <- function(model, variable, label, n = 200) {
  
  model_data <- model$model
  response_name <- all.vars(formula(model))[1]
  predictor_cols <- setdiff(names(model_data), response_name)
  
  x_values <- seq(
    min(model_data[[variable]], na.rm = TRUE),
    max(model_data[[variable]], na.rm = TRUE),
    length.out = n
  )
  
  # Build a representative row: median for numeric columns, first
  # observed level for factor columns - then repeat it once per x value.
  rep_row <- lapply(model_data[predictor_cols], function(col) {
    if (is.numeric(col)) median(col, na.rm = TRUE) else col[1]
  })
  newdata <- as.data.frame(rep_row, stringsAsFactors = FALSE)
  newdata <- newdata[rep(1, length(x_values)), , drop = FALSE]
  
  # Hold month at a representative mid-year value unless month is
  # itself the focal variable being swept.
  if ("Month_num" %in% names(newdata) && variable != "Month_num") {
    newdata$Month_num <- 6
  }
  
  newdata[[variable]] <- x_values
  
  pred <- predict(model, newdata = newdata, type = "terms", se.fit = TRUE)
  
  term_name <- paste0("s(", variable, ")")
  term_index <- which(colnames(pred$fit) == term_name)
  
  fit <- pred$fit[, term_index]
  se <- pred$se.fit[, term_index]
  
  data.frame(
    x = x_values, fit = fit, se = se,
    lower = fit - 1.96 * se, upper = fit + 1.96 * se,
    variable = label
  )
}

make_lag0_gam_plot <- function(dat, title, xlab) {
  
  ggplot(dat, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1.2) +
    labs(title = title, x = xlab, y = "Effect on log(CPUE)") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ================================================================
# Pre-1994, zero-lag Chl — extract + plot
# ================================================================

d_x2_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "X2", "Location")
d_temp_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_Temperature", "Temperature")
d_chl_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_Chl", "Chlorophyll-a (no lag)")
d_sal_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_SalSurf", "Surface salinity")
d_month_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Month_num", "Season")

g_x2_lag0 <- make_lag0_gam_plot(d_x2_lag0, "Location", "X2 position")
g_temp_lag0 <- make_lag0_gam_plot(d_temp_lag0, "Temperature", "Temperature (°C)")
g_chl_lag0 <- make_lag0_gam_plot(d_chl_lag0, "Chlorophyll-a (no lag)", "Chlorophyll-a (contemporaneous)")
g_sal_lag0 <- make_lag0_gam_plot(d_sal_lag0, "Surface salinity", "Surface salinity")
g_month_lag0 <- make_lag0_gam_plot(d_month_lag0, "Season", "Month")

gam_figure_pre1994_lag0 <- (
  g_x2_lag0 | g_temp_lag0 | g_chl_lag0
) / (
  g_sal_lag0 | g_month_lag0
)

gam_figure_pre1994_lag0 +
  plot_annotation(
    title = "Eurytemora GAM relationships — Pre-1994 period",
    subtitle = "1975–1993 (0-day lag, contemporaneous chlorophyll-a)"
  )

# ================================================================
# Post-2004, zero-lag Chl — extract + plot
# ================================================================

d_x2_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "X2", "Location")
d_temp_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Temperature", "Temperature")
d_chl_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Chl", "Chlorophyll-a (no lag)")
d_turb_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Turbidity", "Turbidity")
d_do_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_DO", "Dissolved oxygen")
d_ph_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_pH", "pH")
d_sal_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_SalSurf", "Surface salinity")
d_month_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Month_num", "Season")

g_x2_post_lag0 <- make_lag0_gam_plot(d_x2_post_lag0, "Location", "X2 position")
g_temp_post_lag0 <- make_lag0_gam_plot(d_temp_post_lag0, "Temperature", "Temperature (°C)")
g_chl_post_lag0 <- make_lag0_gam_plot(d_chl_post_lag0, "Chlorophyll-a (no lag)", "Chlorophyll-a (contemporaneous)")
g_turb_post_lag0 <- make_lag0_gam_plot(d_turb_post_lag0, "Turbidity", "Turbidity")
g_do_post_lag0 <- make_lag0_gam_plot(d_do_post_lag0, "Dissolved oxygen", "Dissolved oxygen")
g_ph_post_lag0 <- make_lag0_gam_plot(d_ph_post_lag0, "pH", "pH")
g_sal_post_lag0 <- make_lag0_gam_plot(d_sal_post_lag0, "Surface salinity", "Surface salinity")
g_month_post_lag0 <- make_lag0_gam_plot(d_month_post_lag0, "Season", "Month")

gam_figure_post2004_lag0 <- (
  g_x2_post_lag0 | g_temp_post_lag0 | g_chl_post_lag0 | g_turb_post_lag0
) / (
  g_do_post_lag0 | g_ph_post_lag0 | g_sal_post_lag0 | g_month_post_lag0
)

gam_figure_post2004_lag0 +
  plot_annotation(
    title = "Eurytemora GAM relationships — Post-2004 period",
    subtitle = "2005–present (0-day lag, contemporaneous chlorophyll-a)"
  )