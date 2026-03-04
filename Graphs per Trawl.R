
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

#Here I am attempting to remake the below histograms and graph it based on trawls per DATE not per line.

dt1 %>%
  mutate(Date = as.Date(Date),
         Year = year(Date),
         MonthDay = format(Date, "%m-%d")) %>%
  distinct(Source, Year, MonthDay) %>%
  ggplot(aes(x = MonthDay, y = factor(Year), fill = Source)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01",
                              "06-01", "07-01", "08-01", "09-01", "10-01",
                              "11-01", "12-01"),
                   labels = month.abb) +
  labs(
    title = "Sampling Dates by Year and Trawl Source",
    x = "Date",
    y = "Year",
    fill = "Trawl Source"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )



#these histograms and objects are for total per line observations, not for per day/date observations. Each line has one type of Zoop so it inflates the number of observations depending on the amount of zoops recorded. 
Directed_Outflow_Project_Lower_Trophic_Study <- subset(dt1, Source == "DOP")

#For some reason this is not coming up/does not exist in the data I have brought in. May want to bring it in individually and merge dt1 with the new data.
Yolo_Bypass_Fish_Monitoring_Program <- subset(dt1, Source == "YBFMP")

Fish_Restoration_Program <- subset(dt1, Source == "FRP")

Environmental_Monitoring_Program <- subset(dt1, Source == "EMP")

Summer_Townet_Survey <- subset(dt1, Source == "STN")

Twenty_mm_survey <- subset(dt1, Source == "20mm")

Fall_Midwater_Trawl <- subset(dt1, Source == "FMWT")

#Combined all the lower histograms with facetwrap to make one image that is easier to read. 
combined <- bind_rows(
  Fish_Restoration_Program %>% mutate(Source = "Fish Restoration Program"),
  Directed_Outflow_Project_Lower_Trophic_Study %>% mutate(Source = "Directed Outflow Project"),
  Environmental_Monitoring_Program %>% mutate(Source = "Environmental Monitoring Program"),
  Summer_Townet_Survey %>% mutate(Source = "Summer Townet Survey"),
  Twenty_mm_survey %>% mutate(Source = "20 mm Survey"),
  Fall_Midwater_Trawl %>% mutate(Source = "Fall Midwater Trawl")
)

# Define colors per source
source_colors <- c(
  "Fish Restoration Program"          = "blue",
  "Directed Outflow Project"          = "green",
  "Environmental Monitoring Program"  = "purple",
  "Summer Townet Survey"              = "yellow",
  "20 mm Survey"                      = "steelblue",
  "Fall Midwater Trawl"               = "darkgreen"
)

# Plot
ggplot(combined, aes(x = Year, fill = Source)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = source_colors) +
  facet_wrap(~ Source, scales = "free_y") +
  labs(
    title = "Years Each Trawl Survey Collected Data",
    x = "Year",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",  # legend redundant since facet labels show source
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#histograms of source and year

ggplot(Fish_Restoration_Program, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Years that the Fish Restoration Program Collected Data", x = "Year", y = "Count")

ggplot(Directed_Outflow_Project_Lower_Trophic_Study, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "green", color = "black") +
  labs(title = "Years that the Directed Outflow Project Lower Trophic Study Collected Data", x = "Year", y = "Count")

ggplot(Environmental_Monitoring_Program, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "purple", color = "black") +
  labs(title = "Years that the Environmental Monitoring Program Collected Data", x = "Year", y = "Count")

ggplot(Summer_Townet_Survey, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "yellow", color = "black") +
  labs(title = "Years that the Summer Townet Survey Collected Data", x = "Year", y = "Count")

ggplot(Twenty_mm_survey, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Years that the 20 mm survey Collected Data", x = "Year", y = "Count")

ggplot(Fall_Midwater_Trawl, aes(x = Year)) +
  geom_bar(binwidth = 0.5, fill = "darkgreen", color = "black") +
  labs(title = "Years that the Fall Midwater Trawl Collected Data", x = "Year", y = "Count")
