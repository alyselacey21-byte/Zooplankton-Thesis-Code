library(tidyverse)
library(mgcv)

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Source",     
                 "Station",     
                 "Latitude",     
                 "Longitude",     
                 "Year",     
                 "Date",     
                 "Datetime",     
                 "SampleID",     
                 "TowType",     
                 "AmphipodCode",     
                 "Tide",     
                 "BottomDepth",     
                 "Chl",     
                 "Secchi",     
                 "Temperature",     
                 "Turbidity",     
                 "Microcystis",     
                 "pH",     
                 "DO",     
                 "SalSurf",     
                 "SalBott",     
                 "SizeClass",     
                 "Volume",     
                 "Phylum",     
                 "Class",     
                 "Order",     
                 "Family",     
                 "Genus",     
                 "Species",     
                 "Taxname",     
                 "Lifestage",     
                 "Taxlifestage",     
                 "CPUE",     
                 "Undersampled"    ), check.names=TRUE)

unlink(infile1)


library(RColorBrewer)

dt1 %>%
  mutate(Year = year(as.Date(Date))) %>%
  filter(Genus %in% c("Asplanchna", "Keratella", "Polyartha", "Synchaeta", "Trichocerca")) %>%
  group_by(Year, Genus) %>%
  summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = CPUE, fill = Genus)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(5, "Paired"))(5)) +
  labs(
    x = "Year",
    y = "CPUE",
    title = "Stacked CPUE by Genus (Selected Rotifera)"
  ) +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





