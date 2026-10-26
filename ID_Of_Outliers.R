# ---- EURYTEMORA GAM SETUP Uses organisms_model_reduced from Setting_Up_The_Model ------------------------------------

view(organisms_model_reduced)


outlier_vars <- c("CPUE", "TotalGrabs",
                  "OUT", "X2",
                  "Final_Temperature", "Final_Chl", "Final_DO",
                  "Final_pH", "Final_Turbidity", "Final_SalSurf")

eury <- organisms_model_reduced %>%
  filter(Genus == "Eurytemora") %>%
  mutate(row_id = row_number())

eury_outliers <- eury %>%
  select(row_id, Channel_Station, Date, Source, all_of(outlier_vars)) %>%
  pivot_longer(all_of(outlier_vars),
               names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(Variable) %>%
  mutate(
    Q1       = quantile(Value, 0.25),
    Q3       = quantile(Value, 0.75),
    IQR_val  = Q3 - Q1,
    Inner_lo = Q1 - 1.5 * IQR_val,
    Inner_hi = Q3 + 1.5 * IQR_val,
    Outer_lo = Q1 - 3.0 * IQR_val,
    Outer_hi = Q3 + 3.0 * IQR_val,
    
    Outlier_type = case_when(
      Value < Inner_lo ~ "Low",
      Value > Inner_hi ~ "High",
      TRUE ~ NA_character_
    ),
    Severity = case_when(
      Value < Outer_lo | Value > Outer_hi ~ "Extreme",
      !is.na(Outlier_type)                ~ "Mild",
      TRUE ~ NA_character_
    ),
    # How far past the nearest quartile, in IQR units (bigger = more extreme)
    IQRs_beyond = case_when(
      Value > Q3 ~ (Value - Q3) / IQR_val,
      Value < Q1 ~ (Q1 - Value) / IQR_val,
      TRUE ~ 0
    ),
    # Hard threshold flag for chlorophyll
    Chl_over_50 = Variable == "Final_Chl" & Value > 50
  ) %>%
  ungroup() %>%
  filter(!is.na(Severity) | Chl_over_50) %>%
  arrange(Variable, desc(IQRs_beyond))

# Counts per variable by severity
eury_outliers %>%
  count(Variable, Outlier_type, Severity) %>%
  arrange(Variable, Severity)

# Final_Chl above 50, with IQR severity alongside
eury_chl_over_50 <- eury_outliers %>%
  filter(Variable == "Final_Chl", Chl_over_50) %>%
  arrange(desc(Value))

# Only the really extreme ones, all variables
eury_extreme <- eury_outliers %>%
  filter(Severity == "Extreme")

view(eury_outliers)
view(eury_chl_over_50)
view(eury_extreme)