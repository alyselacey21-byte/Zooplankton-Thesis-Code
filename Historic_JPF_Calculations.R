








###################Cannot use the PDFs because they are scanned images and no text is picked up my R######################








############ Jersey Point Flow (JPF): read DWR hydrology PDFs (in .zip files) + .xlsx, back-calculate JPF ############
## Run top to bottom. Edit only the CONFIG section and the xlsx_map in section 3.

library(pdftools)
library(readxl)
library(slider)
library(tidyverse)   # dplyr, tidyr, stringr, readr, purrr, ggplot2, lubridate


# ---- 0. CONFIG (EDIT THESE) ------------------------------------

data_dir <- "C:/Users/al-la/OneDrive/Desktop/School/Zooplankton/Zooplankton-Thesis-Code/Data_CSVs"

zips     <- list.files(data_dir, pattern = "^\\d{4}\\.zip$", full.names = TRUE)   # 1999.zip ... 2007.zip
csv_path <- file.path(data_dir, "qryhydrorpt_2008_2026.csv")
unzip_dir <- file.path(tempdir(), "jpf_pdfs")
# Every zip and spreadsheet anywhere under Data_CSVs
found_zips <- list.files(data_dir, pattern = "\\.zip$",       recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
found_xlsx <- list.files(data_dir, pattern = "\\.xlsx?$",     recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
found_zips
found_xlsx
zip_dir    <- file.path(data_dir, "JPF_zips")            # folder that holds the .zip files
xlsx_path  <- file.path(data_dir, "qryhydrorpt_2008_2026.xlsx")      # the one .xlsx
xlsx_sheet <- 1
unzip_dir  <- file.path(tempdir(), "jpf_pdfs")           # PDFs are unzipped here (temporary)


walk(zips, ~ unzip(.x, exdir = file.path(unzip_dir, tools::file_path_sans_ext(basename(.x)))))
pdfs <- list.files(unzip_dir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
length(pdfs)
head(basename(pdfs), 10)

cat(pdf_text(pdfs[1])[1])    # first page of the first PDF
# ---- 1. PDF parsing helpers ------------------------------------

# Column layouts of the two tables in the DWR Delta Hydrologic Conditions report.
# Page-2 table gains a JPF column once DWR started reporting it (WY26, ~12/14/2025).
cols_t1      <- c("Date", "SR_at_Freeport_SRWTP", "Yolo_Rumsey_FRE_FWB", "E_side_streams",
                  "SJR_a_Vernalis", "Stockton_rain_in", "CCF_cfs", "Tracy_cfs",
                  "CCWD_cfs", "Barker_Slough_cfs", "Byron_Bethany_cfs")
cols_t2_nojpf <- c("Date", "Banks_PP_cfs", "Delta_GCD_cfs", "Rio_Vista_Flow_cfs",
                   "QWEST_cfs", "NDOI_cfs", "EI_3day", "EI_14day", "Delta_Status")
cols_t2_jpf   <- c("Date", "Banks_PP_cfs", "Delta_GCD_cfs", "Rio_Vista_Flow_cfs",
                   "QWEST_cfs", "JPF_cfs", "NDOI_cfs", "EI_3day", "EI_14day", "Delta_Status")

to_num <- function(x) as.numeric(str_remove_all(x, "[^0-9.-]"))

# Lines that start with a date (m/d/yy or m/d/yyyy)
get_date_lines <- function(page_text) {
  page_text %>%
    str_split("\n") %>% unlist() %>%
    str_replace_all("[^[:print:]]", "") %>%
    str_squish() %>%
    .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")]
}

lines_to_tbl <- function(lines, cols) {
  str_split_fixed(lines, " ", n = length(cols)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(cols) %>%
    as_tibble() %>%
    mutate(across(-any_of(c("Date", "Delta_Status")), to_num),
           Date = mdy(Date))
}

# Parse one PDF. Pages are classified by their header text, not by page number,
# so a table spilling onto an extra page is still picked up.
parse_pdf <- function(path) {
  pages <- pdf_text(path)
  t1 <- list(); t2 <- list()
  
  for (p in pages) {
    lines <- get_date_lines(p)
    if (length(lines) == 0) next
    
    if (str_detect(p, regex("QWEST", ignore_case = TRUE))) {            # table 2
      has_jpf <- str_detect(p, regex("JPF|Jersey", ignore_case = TRUE))
      t2[[length(t2) + 1]] <- lines_to_tbl(lines, if (has_jpf) cols_t2_jpf else cols_t2_nojpf)
    } else if (str_detect(p, regex("Vernalis|Freeport", ignore_case = TRUE))) {  # table 1
      t1[[length(t1) + 1]] <- lines_to_tbl(lines, cols_t1)
    }
  }
  
  if (length(t1) == 0 && length(t2) == 0) return(NULL)
  
  t1 <- bind_rows(t1); t2 <- bind_rows(t2)
  out <- if (nrow(t1) > 0 && nrow(t2) > 0) full_join(t1, t2, by = "Date") else bind_rows(t1, t2)
  out %>% mutate(source_file = basename(path))
}

# ---- 2. Unzip and parse every PDF ------------------------------------


dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
walk(zips, ~ unzip(.x, exdir = file.path(unzip_dir, tools::file_path_sans_ext(basename(.x)))))

pdfs <- list.files(unzip_dir, pattern = "\\.pdf$", recursive = TRUE,
                   full.names = TRUE, ignore.case = TRUE)
cat("PDFs found:", length(pdfs), "\n")

pdf_tbls <- map(pdfs, possibly(parse_pdf, otherwise = NULL))
failed   <- pdfs[map_lgl(pdf_tbls, is.null)]
if (length(failed) > 0) {
  cat("\nWARNING - no tables parsed from these PDFs (check them by hand):\n")
  print(basename(failed))
}

hydro_pdf <- bind_rows(pdf_tbls)
cat("\nPDF rows parsed:", nrow(hydro_pdf), "\n")
cat("PDF date range:\n"); print(range(hydro_pdf$Date, na.rm = TRUE))

# ---- 3. Read the .xlsx ------------------------------------

xl <- read_csv(csv_path, show_col_types = FALSE)

# MAP YOUR XLSX COLUMNS to the standard names: new_name = "name in your xlsx".
# Keep only the lines for columns that exist. Date, SJR_a_Vernalis, E_side_streams,
# SR_at_Freeport_SRWTP, Stockton_rain_in, Delta_GCD_cfs, CCF_cfs and Tracy_cfs are
# needed for the back-calculation; JPF_cfs is optional (only if the xlsx has reported JPF).
xlsx_map <- c(
  Date                 = "EDIT_ME",
  SJR_a_Vernalis       = "EDIT_ME",
  E_side_streams       = "EDIT_ME",
  SR_at_Freeport_SRWTP = "EDIT_ME",
  Stockton_rain_in     = "EDIT_ME",
  Delta_GCD_cfs        = "EDIT_ME",
  CCF_cfs              = "EDIT_ME",
  Tracy_cfs            = "EDIT_ME"
  # , JPF_cfs          = "EDIT_ME"
)

if (any(xlsx_map == "EDIT_ME")) stop("Fill in xlsx_map using the column names printed above.")
if (!all(xlsx_map %in% names(xl))) stop("These xlsx_map names are not in the xlsx: ",
                                        paste(setdiff(xlsx_map, names(xl)), collapse = ", "))

# Excel dates can arrive as Excel serial numbers, POSIXct, Date or text
to_date <- function(x) {
  if (inherits(x, "Date"))        return(x)
  if (inherits(x, "POSIXt"))      return(as.Date(x))
  if (is.numeric(x))              return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings(coalesce(mdy(x), ymd(x)))
}

hydro_xlsx <- xl %>%
  rename(all_of(xlsx_map)) %>%
  select(all_of(names(xlsx_map))) %>%
  mutate(Date = to_date(Date),
         across(-Date, ~ if (is.character(.x)) to_num(.x) else as.numeric(.x)),
         source_file = basename(xlsx_path))

cat("\nxlsx date range:\n"); print(range(hydro_xlsx$Date, na.rm = TRUE))

# ---- 4. Combine PDFs + xlsx into one daily table ------------------------------------

hydro_stack <- bind_rows(hydro_pdf, hydro_xlsx) %>% filter(!is.na(Date))
if (!"JPF_cfs" %in% names(hydro_stack)) hydro_stack$JPF_cfs <- NA_real_

cat("\nDates that appear in more than one source:\n")
print(sum(duplicated(hydro_stack$Date)))

# Where a date is in several files, take the first non-missing value per column
# (PDFs first, then xlsx). Check overlaps above if the sources might disagree.
hydro_all <- hydro_stack %>%
  group_by(Date) %>%
  summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop") %>%
  arrange(Date) %>%
  # Complete daily sequence: lag() below must mean "the previous calendar day",
  # so any missing days must appear as NA rows rather than being skipped.
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  arrange(Date)

cat("\nDaily rows:", nrow(hydro_all), "\n")
cat("Days with no Vernalis value (gaps):", sum(is.na(hydro_all$SJR_a_Vernalis)), "\n")

# ---- 5. Back-calculate JPF ------------------------------------
# Formula kept exactly as in the DWR script. Inputs are lagged one day, except
# pumping (same day) and the 5-day precipitation sum (the 5 days before day t).
# The first ~5 days of the record are NA because the precipitation window is incomplete.

hydro_calc <- hydro_all %>%
  arrange(Date) %>%
  mutate(
    QXGEO        = 0.133 * SR_at_Freeport_SRWTP + 829,
    Delta_precip = (Stockton_rain_in * 679000) / (12 * 1.9835 * 5),
    Delta_prec5d = slide_dbl(Delta_precip, sum, .before = 5, .after = -1, .complete = TRUE),
    Delta_div    = Delta_GCD_cfs,
    pumps        = CCF_cfs + Tracy_cfs,     # CCF rather than Banks, per the DWR note
    JPF_calc     = lag(SJR_a_Vernalis) + lag(E_side_streams) + lag(QXGEO) -
      0.65 * (lag(Delta_div) - Delta_prec5d) - pumps,
    # Reported JPF where it exists, calculated JPF everywhere else
    JPF_final    = coalesce(JPF_cfs, JPF_calc),
    JPF_source   = case_when(!is.na(JPF_cfs)  ~ "reported",
                             !is.na(JPF_calc) ~ "calculated",
                             TRUE             ~ NA_character_)
  )

cat("\nJPF source counts:\n"); print(table(hydro_calc$JPF_source, useNA = "ifany"))

# ---- 6. Check the calculation against reported JPF ------------------------------------
# Only possible on days where DWR reported JPF AND all the calc inputs exist.

overlap <- hydro_calc %>% filter(!is.na(JPF_cfs), !is.na(JPF_calc))

if (nrow(overlap) > 0) {
  cat("\n=== Calculated vs reported JPF (n =", nrow(overlap), "days) ===\n")
  cat("Correlation:", round(cor(overlap$JPF_cfs, overlap$JPF_calc), 3), "\n")
  cat("Mean bias (calc - reported), cfs:", round(mean(overlap$JPF_calc - overlap$JPF_cfs), 1), "\n")
  cat("RMSE, cfs:", round(sqrt(mean((overlap$JPF_calc - overlap$JPF_cfs)^2)), 1), "\n")
  
  print(
    ggplot(overlap, aes(x = JPF_cfs, y = JPF_calc)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = 0.7) +
      labs(title = "Back-calculated vs reported JPF",
           x = "Reported JPF (cfs)", y = "Calculated JPF (cfs)") +
      theme_bw(base_size = 14)
  )
} else {
  cat("\nNo days with both reported and calculated JPF, so the calculation can't be validated.\n")
}

# ---- 7. Plot and save ------------------------------------

jpf_plot <- ggplot(hydro_calc, aes(x = Date, y = JPF_final, colour = JPF_source)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#888888", linewidth = 1) +
  geom_line(aes(group = 1), colour = "grey70") +
  geom_point(size = 1.5, na.rm = TRUE) +
  scale_colour_manual(values = c(reported = "darkturquoise", calculated = "#D55E00"),
                      na.value = "grey50", name = "JPF source") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(y = "Jersey Point Flow (cfs)", x = NULL) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(jpf_plot)

write_csv(hydro_calc, file.path(data_dir, "hydro_cond_all_with_JPF.csv"))
write_csv(hydro_calc %>% select(Date, JPF_final, JPF_source),
          file.path(data_dir, "JPF_historic.csv"))

# ---- 8. (Optional) attach JPF to the organism data by Date ------------------------------------
# organisms_model_reduced <- organisms_model_reduced %>%
#   left_join(hydro_calc %>% select(Date, JPF_final, JPF_source), by = "Date")
