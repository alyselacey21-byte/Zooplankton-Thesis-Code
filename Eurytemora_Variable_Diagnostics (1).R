##################################################################
## EURYTEMORA — VARIABLE DISTRIBUTION & CPUE RELATIONSHIP DIAGNOSTICS
## For every variable in organisms_model_reduced, look at:
##   (a) the variable's own raw distribution (histogram/bar chart)
##   (b) how CPUE behaves across that variable (binned mean CPUE,
##       or boxplot for categorical variables)
##
## PURPOSE: GAM smooths that swing wildly, flatten to a straight
## line, or blow up in confidence width are very often explained by
## sparse data in that region - not a real biological signal. This
## script makes that visible directly, before looking at any GAM
## output, so odd smooth shapes can be checked against the raw data
## that produced them.
##################################################################

library(ggplot2)
library(patchwork)
library(dplyr)
# viridisLite/scales provides scale_fill_viridis_c() (loaded via ggplot2
# in recent versions, but installed explicitly here in case it isn't):
if (!requireNamespace("viridisLite", quietly = TRUE)) install.packages("viridisLite")

# Uses the full (unsplit, unlagged) Eurytemora subset - i.e. eda_data
# reflects the raw data before era-splitting, Chl lagging, or any
# GAM-specific complete-case filtering, so it shows the true
# underlying distribution the GAMs are drawing from.

eda_data <- organisms_model_reduced %>%
  filter(Genus == "Eurytemora") %>%
  droplevels()

cat("\nRows in eda_data:\n"); print(nrow(eda_data))

# Skip these: Date (not meaningful as a histogram/CPUE-bin variable
# on its own - Year/Month_num already cover time structure), CPUE
# itself (it's the response, not something to plot against itself),
# and Year - Year gets its OWN dedicated chronological plot below
# instead of going through the generic categorical branch. Year has
# ~36 levels (1975-1993, 2005-2021), which exceeds max_levels = 25,
# so the generic branch would cap it to the top 25 levels BY
# FREQUENCY - silently dropping whichever years have the fewest
# samples and destroying chronological order in the process. That's
# exactly wrong for a time variable, where seeing the full sequence
# (including the 1994-2004 sampling gap) is the point.
vars_to_check <- setdiff(names(eda_data), c("CPUE", "Date", "Year"))

cat("\nVariables to check (", length(vars_to_check), "):\n", sep = "")
print(vars_to_check)

# Full chronological year sequence (including the 1994-2004 gap as
# explicit positions) - reused as a fixed x-axis across the dedicated
# Year plot AND the new Year-heatmap panel on every other variable, so
# every plot in the PDF places years consistently and the gap always
# reads as a gap rather than being silently skipped on some plots and
# not others.
full_year_levels <- as.character(
  seq(min(as.numeric(as.character(eda_data$Year))),
      max(as.numeric(as.character(eda_data$Year))))
)

##################################################################
## Function: build a two-panel diagnostic plot for one variable
##################################################################
# Numeric variable  -> histogram of its distribution, and mean CPUE
#                       per bin (bin counts labeled, so sparse bins
#                       are visible directly under the bar).
# Categorical/factor -> bar chart of level counts, and boxplot of
#                       CPUE (log1p scale, since CPUE is heavily
#                       right-skewed) per level. High-cardinality
#                       factors (e.g. Channel_Station) are capped to
#                       the most frequent levels so the plot stays
#                       readable; the console prints how many were
#                       dropped.

make_var_cpue_plot <- function(data, var, max_levels = 25, n_bins = 20) {

  x <- data[[var]]

  if (is.numeric(x)) {

    p_hist <- ggplot(data, aes(x = .data[[var]])) +
      geom_histogram(bins = 40, fill = "grey40", color = "white") +
      labs(title = paste0(var, " — distribution"), x = var, y = "Count") +
      theme_classic(base_size = 12)

    breaks <- pretty(x, n = n_bins)

    binned <- data %>%
      filter(!is.na(.data[[var]]), !is.na(CPUE)) %>%
      mutate(bin = cut(.data[[var]], breaks = breaks, include.lowest = TRUE)) %>%
      filter(!is.na(bin)) %>%
      group_by(bin) %>%
      summarise(mean_cpue = mean(CPUE, na.rm = TRUE), n = n(), .groups = "drop")

    p_cpue <- ggplot(binned, aes(x = bin, y = mean_cpue)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = n), vjust = -0.3, size = 2.5) +
      labs(
        title = paste0("Mean CPUE by ", var, " bin  (labels = n per bin)"),
        x = var, y = "Mean CPUE"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

    # Year (x-axis) x variable-bin (y-axis) heatmap, fill = sample count.
    # Reveals WHEN a variable's coverage is sparse, not just that it is -
    # e.g. Final_Chl and Final_Turbidity should show clearly different
    # eras of coverage here, matching the year_compare retention table.
    year_bin_data <- data %>%
      filter(!is.na(.data[[var]])) %>%
      mutate(
        bin = cut(.data[[var]], breaks = breaks, include.lowest = TRUE),
        Year = factor(as.character(Year), levels = full_year_levels)
      ) %>%
      filter(!is.na(bin)) %>%
      count(Year, bin, .drop = FALSE)

    p_heat <- ggplot(year_bin_data, aes(x = Year, y = bin, fill = n)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_x_discrete(limits = full_year_levels, drop = FALSE) +
      scale_fill_viridis_c(name = "n", na.value = "grey95") +
      labs(
        title = paste0(var, " coverage by year (blank = no data)"),
        x = "Year", y = var
      ) +
      theme_classic(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 7)
      )

    combined <- p_hist / p_cpue / p_heat

  } else {

    n_levels <- n_distinct(x, na.rm = TRUE)

    plot_data <- data
    subtitle_note <- NULL

    if (n_levels > max_levels) {
      top_levels <- data %>%
        count(.data[[var]], sort = TRUE) %>%
        slice(1:max_levels) %>%
        pull(1)
      plot_data <- data %>% filter(.data[[var]] %in% top_levels)
      subtitle_note <- paste0(
        "Showing top ", max_levels, " of ", n_levels, " levels by frequency"
      )
      cat("  Note:", var, "has", n_levels, "levels - showing top", max_levels, "\n")
    }

    p_bar <- ggplot(plot_data, aes(x = .data[[var]])) +
      geom_bar(fill = "grey40") +
      labs(
        title = paste0(var, " — counts"),
        subtitle = subtitle_note,
        x = var, y = "Count"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

    p_box <- ggplot(
      plot_data %>% filter(!is.na(.data[[var]]), !is.na(CPUE)),
      aes(x = .data[[var]], y = CPUE)
    ) +
      geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
      scale_y_continuous(trans = "log1p") +
      labs(
        title = paste0("CPUE by ", var, " (log1p scale)"),
        x = var, y = "CPUE (log1p)"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

    # Year (x-axis) x category level (y-axis) heatmap, fill = count.
    # Skipped for Year itself (can't cross Year against Year) - handled
    # separately by make_year_plot().
    if (var != "Year") {

      year_cat_data <- plot_data %>%
        filter(!is.na(.data[[var]])) %>%
        mutate(Year = factor(as.character(Year), levels = full_year_levels)) %>%
        count(Year, .data[[var]], .drop = FALSE)

      p_heat <- ggplot(year_cat_data, aes(x = Year, y = .data[[var]], fill = n)) +
        geom_tile(color = "white", linewidth = 0.1) +
        scale_x_discrete(limits = full_year_levels, drop = FALSE) +
        scale_fill_viridis_c(name = "n", na.value = "grey95") +
        labs(
          title = paste0(var, " coverage by year (blank = no data)"),
          x = "Year", y = var
        ) +
        theme_classic(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          axis.text.y = element_text(size = 7)
        )

      combined <- p_bar / p_box / p_heat

    } else {
      combined <- p_bar / p_box
    }
  }

  combined
}

##################################################################
## Function: dedicated Year diagnostic (chronological, not
## frequency-capped)
##################################################################
# Two panels, both in true chronological order:
#   - sample count per year (shows the 1975-1993 / 2005-2021
#     sampling structure and the 1994-2004 gap directly)
#   - mean CPUE per year (shows whether apparent GAM-era differences
#     - e.g. the very different Season effect sizes you saw between
#     pre-1994 and post-2004 - track a real year-by-year shift, or
#     look more like noise/outlier-driven swings in specific years)
# Years with zero rows (the 1994-2004 gap) are shown as explicit
# empty positions on the x-axis rather than silently skipped, so the
# gap reads as a gap rather than looking like continuous data.

make_year_plot <- function(data, year_levels) {

  all_years <- data.frame(Year = factor(year_levels, levels = year_levels))

  year_summary <- data %>%
    filter(!is.na(CPUE)) %>%
    group_by(Year) %>%
    summarise(mean_cpue = mean(CPUE, na.rm = TRUE), n = n(), .groups = "drop") %>%
    mutate(Year = factor(as.character(Year), levels = year_levels))

  year_full <- all_years %>%
    left_join(year_summary, by = "Year") %>%
    mutate(n = replace_na(n, 0))

  p_count <- ggplot(year_full, aes(x = Year, y = n)) +
    geom_col(fill = "grey40") +
    labs(
      title = "Eurytemora sample count by year (chronological)",
      subtitle = "Zero-height bars = years with no data (1994-2004 gap)",
      x = "Year", y = "Count"
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

  p_cpue <- ggplot(year_full, aes(x = Year, y = mean_cpue)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = ifelse(n > 0, n, "")), vjust = -0.3, size = 2.3) +
    labs(
      title = "Mean CPUE by year (chronological, labels = n)",
      x = "Year", y = "Mean CPUE"
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

  p_count / p_cpue
}

##################################################################
## Generate one page per variable in a single PDF
##################################################################

pdf("Eurytemora_variable_CPUE_diagnostics.pdf", width = 9, height = 11)

cat("Plotting: Year (dedicated chronological view)\n")
tryCatch({
  print(make_year_plot(eda_data, full_year_levels))
}, error = function(e) {
  cat("  Skipped Year - error:", conditionMessage(e), "\n")
})

for (v in vars_to_check) {
  cat("Plotting:", v, "\n")
  tryCatch({
    print(make_var_cpue_plot(eda_data, v))
  }, error = function(e) {
    cat("  Skipped", v, "- error:", conditionMessage(e), "\n")
  })
}

dev.off()

cat("\nSaved: Eurytemora_variable_CPUE_diagnostics.pdf (", length(vars_to_check) + 1,
    "pages: 1 dedicated Year page +", length(vars_to_check), "other variables)\n")

##################################################################
## Optional: preview a single variable on screen without
## regenerating the whole PDF - useful once you've spotted a
## variable of interest from the PDF and want to look closer.
##################################################################
# Example:
# make_var_cpue_plot(eda_data, "Final_Chl")
# make_var_cpue_plot(eda_data, "X2")















