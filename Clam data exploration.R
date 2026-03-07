options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1036/6/7475c46f9c8bebf01ad24d50ee7a0ff9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Date",     
                 "Station",     
                 "LabSampleNumber",     
                 "Grab",     
                 "Year",     
                 "Month",     
                 "OrganismCode",     
                 "Count",     
                 "Phylum",     
                 "Class_level",     
                 "Order_level",     
                 "Family_level",     
                 "Genus",     
                 "Species",     
                 "Common_name",     
                 "Location",     
                 "Latitude",     
                 "Longitude"    ), check.names=TRUE)

unlink(infile1)

library(tidyverse)
library(mgcv)
library(lubridate)

#Clams are in the Phylum Mollusca, ID:6730 


#Specifically Asian and Overbite clams
dt2 %>%
  mutate(Year = year(as.Date(Date))) %>%
  filter(OrganismCode %in% c("6730", "6890")) %>%
  group_by(Year) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_count)) +
  geom_bar(stat = "identity", fill = "brown", color = "black") +
  labs(
    title = "Mean Asian clam and Overbite clam Density Over Time",
    x = "Year",
    y = "Average Clam Density"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





dt2 %>%
  mutate(Year = year(as.Date(Date)),
         OrganismCode = as.character(OrganismCode)) %>%
  filter(OrganismCode %in% c("6730", "6890")) %>%
  group_by(Year, OrganismCode) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_count, fill = OrganismCode)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(
    values = c("6730" = "steelblue", "6890" = "brown"),
    labels = c("6730" = "Asian Clam", "6890" = "Overbite Clam")
  ) +
  labs(
    title = "Mean Asian Clam and Overbite Clam Density Over Time",
    x = "Year",
    y = "Average Clam Density",
    fill = "Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



dt2_mollusca <- dt2 %>%
  filter(Phylum == "Mollusca") %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_mollusca, aes(x = Year, y = mean_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_mollusca$Common_name)))) +
  labs(x = "Year", y = "Mean Count", title = "Stacked Mean Count by Species - Mollusca") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



unique(dt2_mollusca$Common_name)
