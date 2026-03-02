
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



Directed_Outflow_Project_Lower_Trophic_Study <- subset(dt1, Source == "DOP")

#For some reason this is not coming up/does not exist in the data I have brought in. May want to bring it in individually and merge dt1 with the new data.
Yolo_Bypass_Fish_Monitoring_Program <- subset(dt1, Source == "YBFMP")

Fish_Restoration_Program <- subset(dt1, Source == "FRP")

Environmental_Monitoring_Program <- subset(dt1, Source == "EMP")

Summer_Townet_Survey <- subset(dt1, Source == "STN")

Twenty_mm_survey <- subset(dt1, Source == "20mm")

Fall_Midwater_Trawl <- subset(dt1, Source == "FMWT")


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
