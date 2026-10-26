

############ Making the GAM For Eurytemora ###########
## Consolidated version - all parts reordered so every object is
## defined/created before it's referenced. Run top-to-bottom.

# ---- Packages ------------------------------------

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


# ---- EURYTEMORA GAM SETUP Uses organisms_model_reduced from Setting_Up_The_Model ------------------------------------

view(organisms_model_reduced)


# ---- PART 1 — Subset to Eurytemora, confirm source ------------------------------------


eurytemora_data <- organisms_model_reduced %>%
  filter(Genus == "Eurytemora") %>%
  droplevels()

cat("\nRows:\n"); print(nrow(eurytemora_data))
cat("\nSource breakdown:\n"); print(table(eurytemora_data$Source))
# Expect: all rows are "Zooplankton" - Eurytemora doesn't appear in Benthic data.

# ---- PART 2 — Predictor completeness (full dataset, pre-split) ------------------------------------

add_water_month <- function(df) {
  df %>% mutate(Water_Month = ((as.numeric(Month_num) + 2) %% 12) + 1)
}

eurytemora_data     <- add_water_month(eurytemora_data)
eurytemora_pre1994  <- add_water_month(eurytemora_pre1994)
eurytemora_post2004 <- add_water_month(eurytemora_post2004)

# Sanity check: Oct -> 1, Dec -> 3, Jan -> 4, Sep -> 12
table(eurytemora_pre1994$Month_num, eurytemora_pre1994$Water_Month)

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

# ---- PART 3 — Year-by-year retention check  ------------------------------------

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

# ---- PART 4 — Lag-Chl helper function (defined once, used later) ------------------------------------

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

# ---- PART 5 — Split into pre-1994 and post-2004 eras ------------------------------------

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

# ----  PART 6 — Apply chosen 2-week (14-day) Chl lag, both eras ------------------------------------

eurytemora_pre1994 <- add_lagged_chl(eurytemora_pre1994, lag_days = 14) %>%
  rename(Final_Chl_lag14 = Final_Chl_lag)

cat("\nRows with valid 14-day lagged Chl, pre-1994:\n")
print(sum(!is.na(eurytemora_pre1994$Final_Chl_lag14)))
cat("(vs. contemporaneous Chl non-NA count:\n")
print(sum(!is.na(eurytemora_pre1994$Final_Chl)))
cat(")\n")

eurytemora_post2004 <- add_lagged_chl(eurytemora_post2004, lag_days = 14) %>%
  rename(Final_Chl_lag14 = Final_Chl_lag)

cat("\nRows with valid 14-day lagged Chl, post-2004:\n")
print(sum(!is.na(eurytemora_post2004$Final_Chl_lag14)))
cat("(vs. contemporaneous Chl non-NA count:\n")
print(sum(!is.na(eurytemora_post2004$Final_Chl)))
cat(")\n")

# ----  PART 7 — Within-era coverage ------------------------------------

cat("\nCoverage, pre-1994:\n")
print(eurytemora_pre1994 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

cat("\nCoverage, post-2004:\n")
print(eurytemora_post2004 %>% summarise(across(all_of(env_vars), ~ mean(!is.na(.x)))), width = Inf)

cat("\nPost-2004 complete cases, all four (Chl+Turbidity+DO+pH):\n")
print(eurytemora_post2004 %>% drop_na(all_of(env_vars)) %>% nrow())

# ----  PART 7B — Re-check complete-case tradeoffs for post-2004, using the LAGGED Chl variable ------------------------------------

cat("\nPost-2004 complete cases - all four (Chl_lag14+Turbidity+DO+pH):\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag14, Final_Turbidity,
                Final_DO, Final_pH, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop DO/pH only:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag14, Final_Turbidity,
                Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop DO/pH, drop Turbidity too:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Chl_lag14, Final_SalSurf) %>% nrow())

cat("\nPost-2004 complete cases - drop Chl_lag14, keep Turbidity+DO+pH:\n")
print(eurytemora_post2004 %>%
        drop_na(X2, Final_Temperature, Final_Turbidity, Final_DO,
                Final_pH, Final_SalSurf) %>% nrow())

# ---- PART 8 — ASSIGN EDSM STRATA ------------------------------------

data("R_EDSM_Strata_1718P1")


################# PRE-1994


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


# POST-2004


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

# ---- PART 9 — LM sanity check ------------------------------------

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

# ---- PART 10 — Outlier / extreme-value check (pre-1994) ------------------------------------

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

# ---- PART 11 — GAM, PRE-1994 (strata parametric term + all smooths by strata) ----

min_n <- 100   # drop strata with fewer rows than this

pre_vars <- c("CPUE","X2","Final_Temperature","Final_Chl_lag14",
              "Final_SalSurf","Water_Month","R_EDSM_Strata_1718P1")

pre_cc <- eurytemora_pre1994 %>% drop_na(all_of(pre_vars))

keep_strata_pre <- pre_cc %>%
  count(R_EDSM_Strata_1718P1) %>%
  filter(n >= min_n) %>%
  pull(R_EDSM_Strata_1718P1) %>% as.character()

pre1994_g <- pre_cc %>%
  filter(R_EDSM_Strata_1718P1 %in% keep_strata_pre) %>%
  mutate(R_EDSM_Strata_1718P1 = factor(R_EDSM_Strata_1718P1)) %>%
  droplevels()

stopifnot(nlevels(pre1994_g$R_EDSM_Strata_1718P1) >= 2)
cat("\nStrata kept (pre-1994):\n"); print(table(pre1994_g$R_EDSM_Strata_1718P1))

gam_pre1994_by <- bam(
  CPUE ~ R_EDSM_Strata_1718P1 +
    s(X2,                by = R_EDSM_Strata_1718P1, k = 8) +
    s(Final_Temperature, by = R_EDSM_Strata_1718P1, k = 6) +
    s(Final_Chl_lag14,   by = R_EDSM_Strata_1718P1, k = 8) +
    s(Final_SalSurf,     by = R_EDSM_Strata_1718P1, k = 10) +
    s(Water_Month,       by = R_EDSM_Strata_1718P1, bs = "cc", k = 6),
  family   = tw(),
  method   = "fREML",     # fast REML; "REML" is not used with discrete = TRUE
  discrete = TRUE,
  nthreads = parallel::detectCores() - 1,
  gamma    = 1.4,
  data     = pre1994_g,
  knots    = list(Water_Month = c(0.5, 12.5))
)

summary(gam_pre1994_by)
gam.check(gam_pre1994_by)
concurvity(gam_pre1994_by, full = TRUE)

acf_result_pre1994 <- acf(residuals(gam_pre1994_by), plot = FALSE)
print(acf_result_pre1994$acf[1:10])

# ---- PART 11B — GAM, POST-2004 (strata parametric term + all smooths by strata) ----

post_vars <- c("CPUE","X2","Final_Temperature","Final_Chl_lag14",
               "Final_SalSurf","Water_Month","R_EDSM_Strata_1718P1")

post_cc <- eurytemora_post2004 %>% drop_na(all_of(post_vars))

keep_strata_post <- post_cc %>%
  count(R_EDSM_Strata_1718P1) %>%
  filter(n >= min_n) %>%
  pull(R_EDSM_Strata_1718P1) %>% as.character()

post2004_g <- post_cc %>%
  filter(R_EDSM_Strata_1718P1 %in% keep_strata_post) %>%
  mutate(R_EDSM_Strata_1718P1 = factor(R_EDSM_Strata_1718P1)) %>%
  droplevels()

stopifnot(nlevels(post2004_g$R_EDSM_Strata_1718P1) >= 2)
cat("\nStrata kept (post-2004):\n"); print(table(post2004_g$R_EDSM_Strata_1718P1))

gam_post2004_by <- bam(
  CPUE ~ R_EDSM_Strata_1718P1 +
    s(X2,                by = R_EDSM_Strata_1718P1, k = 8) +
    s(Final_Temperature, by = R_EDSM_Strata_1718P1, k = 6) +
    s(Final_Chl_lag14,   by = R_EDSM_Strata_1718P1, k = 8) +
    s(Final_SalSurf,     by = R_EDSM_Strata_1718P1, k = 10) +
    s(Water_Month,       by = R_EDSM_Strata_1718P1, bs = "cc", k = 6),
  family   = Tweedie(p = 1.67, link = "log"),
  method   = "fREML",
  discrete = TRUE,
  nthreads = max(1, parallel::detectCores() - 1),
  gamma    = 1.4,
  data     = post2004_g,
  knots    = list(Water_Month = c(0.5, 12.5))
)

summary(gam_post2004_by)
gam.check(gam_post2004_by)
concurvity(gam_post2004_by, full = TRUE)
print(acf(residuals(gam_post2004_by), plot = FALSE)$acf[1:10])


# ---- PART 12 — ONE PAGE PER VARIABLE, ALL STRATA ----

library(mgcv); library(ggplot2); library(dplyr); library(purrr)

# Partial effect of `variable` for every stratum, each drawn only over that
# stratum's observed range (trimmed at the quantiles to avoid sparse tails).
get_by_smooth_data <- function(model, variable, by_var = "R_EDSM_Strata_1718P1",
                               n = 100, trim = c(0.01, 0.99)) {
  md   <- model$model
  cols <- setdiff(names(md), names(md)[1])
  base <- lapply(md[cols], function(col) if (is.numeric(col)) median(col, na.rm = TRUE) else col[1])
  
  map_dfr(levels(md[[by_var]]), function(lvl) {
    sub <- md[md[[by_var]] == lvl, ]
    rng <- quantile(sub[[variable]], trim, na.rm = TRUE)
    xs  <- seq(rng[1], rng[2], length.out = n)
    
    nd <- as.data.frame(base, stringsAsFactors = FALSE)[rep(1, n), , drop = FALSE]
    nd[[variable]] <- xs
    nd[[by_var]]   <- factor(lvl, levels = levels(md[[by_var]]))
    if ("Water_Month" %in% names(nd) && variable != "Water_Month") nd$Water_Month <- 6
    
    pred <- predict(model, newdata = nd, type = "terms", se.fit = TRUE)
    idx  <- which(colnames(pred$fit) == paste0("s(", variable, "):", by_var, lvl))
    fit  <- pred$fit[, idx]; se <- pred$se.fit[, idx]
    
    data.frame(x = xs, fit = fit, se = se,
               lower = fit - 1.96 * se, upper = fit + 1.96 * se, stratum = lvl)
  })
}

water_year_axis <- scale_x_continuous(
  breaks = 1:12,
  labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")
)

# One full page for one variable.
#   type = "overlay": all strata as coloured lines on a single panel
#   type = "facet"  : one panel per stratum, with 95% CI ribbons
plot_variable_page <- function(model, variable, title, xlab, era_label,
                               type = c("overlay", "facet"),
                               trim = c(0.01, 0.99), xlim = NULL, season = FALSE) {
  type <- match.arg(type)
  dat  <- get_by_smooth_data(model, variable, trim = trim)
  lv   <- levels(model$model$R_EDSM_Strata_1718P1)
  cols <- setNames(c("#E69F00","#56B4E9","#009E73","#F0E442",
                     "#0072B2","#D55E00","#CC79A7","#000000",
                     "#999999","#882255")[seq_along(lv)], lv)
  
  p <- ggplot(dat, aes(x = x, y = fit)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4)
  
  if (type == "overlay") {
    p <- p + geom_line(aes(colour = stratum), linewidth = 1.1) +
      scale_colour_manual(values = cols, name = "EDSM stratum")
  } else {
    p <- p +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20) +
      geom_line(linewidth = 1) +
      facet_wrap(~ stratum, scales = "free_x", ncol = 4)
  }
  
  p <- p +
    labs(title = paste0("Eurytemora — ", title, " by EDSM stratum"),
         subtitle = paste0(era_label, " (gamma = 1.4, ",
                           if (type == "overlay") "all strata overlaid)" else "one panel per stratum)"),
         x = xlab, y = "Effect on log(CPUE) (unitless)") +
    theme_classic(base_size = 14) +
    theme(plot.title    = element_text(face = "bold", size = 16),
          axis.title    = element_text(face = "bold"),
          axis.text     = element_text(color = "black"),
          strip.text    = element_text(face = "bold"),
          strip.background = element_blank(),
          legend.position = "right")
  
  if (!is.null(xlim)) p <- p + coord_cartesian(xlim = xlim)
  if (season)         p <- p + water_year_axis
  p
}

# Prints all five variables, one page each, to the plot window and a PDF.
make_variable_pages <- function(model, era_label, file, type = "overlay") {
  specs <- list(
    list("X2",                "Location (X2)",      "X2 position (km)",                  c(0.01, 0.99), NULL,       FALSE),
    list("Final_Temperature", "Temperature",        "Temperature (deg C)",               c(0.01, 0.99), NULL,       FALSE),
    list("Final_Chl_lag14",   "Chlorophyll-a",      "14-day lagged chlorophyll-a (ug/L)",c(0.01, 0.99), c(0, 50),   FALSE),
    list("Final_SalSurf",     "Surface salinity",   "Surface salinity (PSU)",            c(0.01, 0.99), NULL,       FALSE),
    list("Water_Month",       "Season",             "Water-year month",                  c(0, 1),       NULL,       TRUE)
  )
  pages <- map(specs, ~ plot_variable_page(model, .x[[1]], .x[[2]], .x[[3]], era_label,
                                           type = type, trim = .x[[4]],
                                           xlim = .x[[5]], season = .x[[6]]))
  walk(pages, print)                         # show in RStudio
  pdf(file, width = 12, height = 8)          # one variable per page
  walk(pages, print)
  dev.off()
  invisible(pages)
}

# ---- Run: pre-1994 and post-2004, overlay and per-stratum versions ----
make_variable_pages(gam_pre1994_by,  "Pre-1994 (1975–1993)",   "eurytemora_pre1994_overlay.pdf",  "overlay")
make_variable_pages(gam_pre1994_by,  "Pre-1994 (1975–1993)",   "eurytemora_pre1994_facet.pdf",    "facet")
make_variable_pages(gam_post2004_by, "Post-2004 (2005–2021)",  "eurytemora_post2004_overlay.pdf", "overlay")
make_variable_pages(gam_post2004_by, "Post-2004 (2005–2021)",  "eurytemora_post2004_facet.pdf",   "facet")

# ---- PART 13 — PRESENTATION-READY POST-2004 GAM SMOOTH PLOTS (14-day lag) ------------------------------------

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
    Final_Chl_lag14 = median(model_data$Final_Chl_lag14, na.rm = TRUE),
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
d_chl_post <- get_post2004_smooth_data(gam_eurytemora_post2004_final, "Final_Chl_lag14", "Chlorophyll-a")
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
    labs(title = title, x = xlab, y = "Effect on log(CPUE) (unitless)") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

g_x2_post <- make_post2004_gam_plot(d_x2_post, "Location", "X2 position (km)")
g_temp_post <- make_post2004_gam_plot(d_temp_post, "Temperature", "Temperature (deg C)")
g_chl_post <- make_post2004_gam_plot(d_chl_post, "Chlorophyll-a", "14-day lagged chlorophyll-a (ug/L)") +
  coord_cartesian(xlim = c(0, 50))
g_turb_post <- make_post2004_gam_plot(d_turb_post, "Turbidity", "Turbidity (NTU/FNU)")
g_do_post <- make_post2004_gam_plot(d_do_post, "Dissolved oxygen", "Dissolved oxygen (mg/L)")
g_ph_post <- make_post2004_gam_plot(d_ph_post, "pH", "pH (unitless)")
g_sal_post <- make_post2004_gam_plot(d_sal_post, "Surface salinity", "Surface salinity (PSU)")
g_month_post <- make_post2004_gam_plot(d_month_post, "Season", "Month (1-12)")

gam_figure_post2004 <- (
  g_x2_post | g_temp_post | g_chl_post | g_turb_post
) / (
  g_do_post | g_ph_post | g_sal_post | g_month_post
)

gam_figure_post2004 +
  plot_annotation(
    title = "Eurytemora GAM relationships — Post-2004 period",
    subtitle = "2005–present (14-day lagged chlorophyll-a)"
  )


# ---- PART 14 — ZERO-LAG (CONTEMPORANEOUS) CHLOROPHYLL GAMs ## Same model structure as Part 11/11B, but using same-day Final_Chl ## instead of Final_Chl_lag14.------------------------------------


# Pre-1994, zero-lag Chl


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


# Post-2004, zero-lag Chl

# NOTE - NOT EDITED, FLAGGING ONLY: both gam() calls below reference
# `post2004_complete`, which is never created anywhere in this script
# (the rest of the script uses `eurytemora_post2004`). Running this
# section as-is will throw "object 'post2004_complete' not found".
# I've left this section exactly as you had it since you didn't ask
# me to touch Part 14 this time - flagging so it doesn't catch you by
# surprise, not silently guessing what post2004_complete should be
# (e.g. a specific drop_na() subset, and with or without
# Channel_Station - the first call below omits Channel_Station as a
# random effect entirely, unlike every other model in this script,
# so that looks like an intentional-but-undocumented change worth
# confirming before running this section).

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



# ---- PART 15 — PRESENTATION-READY GAM SMOOTH PLOTS: ZERO-LAG MODELS ## Uses a generic smooth-extraction function (builds newdata from whatever columns the model actually used, rather than hardcoding ##column names) so the same function works for both eras and for any future model without editing the function itself------------------------------------

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
    labs(title = title, x = xlab, y = "Effect on log(CPUE) (unitless)") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
}


# Pre-1994, zero-lag Chl — extract + plot


d_x2_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "X2", "Location")
d_temp_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_Temperature", "Temperature")
d_chl_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_Chl", "Chlorophyll-a (no lag)")
d_sal_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Final_SalSurf", "Surface salinity")
d_month_lag0 <- get_smooth_data_generic(gam_eurytemora_pre1994_lag0, "Month_num", "Season")

g_x2_lag0 <- make_lag0_gam_plot(d_x2_lag0, "Location", "X2 position (km)")
g_temp_lag0 <- make_lag0_gam_plot(d_temp_lag0, "Temperature", "Temperature (deg C)")
g_chl_lag0 <- make_lag0_gam_plot(d_chl_lag0, "Chlorophyll-a (no lag)", "Chlorophyll-a, contemporaneous (ug/L)") +
  coord_cartesian(xlim = c(0, 50))
g_sal_lag0 <- make_lag0_gam_plot(d_sal_lag0, "Surface salinity", "Surface salinity (PSU)")
g_month_lag0 <- make_lag0_gam_plot(d_month_lag0, "Season", "Month (1-12)")

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


# Post-2004, zero-lag Chl — extract + plot


d_x2_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "X2", "Location")
d_temp_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Temperature", "Temperature")
d_chl_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Chl", "Chlorophyll-a (no lag)")
d_turb_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_Turbidity", "Turbidity")
d_do_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_DO", "Dissolved oxygen")
d_ph_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_pH", "pH")
d_sal_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Final_SalSurf", "Surface salinity")
d_month_post_lag0 <- get_smooth_data_generic(gam_eurytemora_post2004_lag0, "Month_num", "Season")

g_x2_post_lag0 <- make_lag0_gam_plot(d_x2_post_lag0, "Location", "X2 position (km)")
g_temp_post_lag0 <- make_lag0_gam_plot(d_temp_post_lag0, "Temperature", "Temperature (deg C)")
g_chl_post_lag0 <- make_lag0_gam_plot(d_chl_post_lag0, "Chlorophyll-a (no lag)", "Chlorophyll-a, contemporaneous (ug/L)") +
  coord_cartesian(xlim = c(0, 50))
g_turb_post_lag0 <- make_lag0_gam_plot(d_turb_post_lag0, "Turbidity", "Turbidity (NTU/FNU)")
g_do_post_lag0 <- make_lag0_gam_plot(d_do_post_lag0, "Dissolved oxygen", "Dissolved oxygen (mg/L)")
g_ph_post_lag0 <- make_lag0_gam_plot(d_ph_post_lag0, "pH", "pH (unitless)")
g_sal_post_lag0 <- make_lag0_gam_plot(d_sal_post_lag0, "Surface salinity", "Surface salinity (PSU)")
g_month_post_lag0 <- make_lag0_gam_plot(d_month_post_lag0, "Season", "Month (1-12)")

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