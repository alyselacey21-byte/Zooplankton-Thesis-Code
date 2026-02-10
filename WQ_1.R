
WaterQuality <- read.csv("C:/Users/al-la/Downloads/SMSCG_wq_data_2017-2023_clean_tzone.csv")
head(WaterQuality)
tail(WaterQuality)
nchar(WaterQuality)
class(WaterQuality)
typeof(WaterQuality)
str(WaterQuality)
# Code above is out of date and unneeded, Do not run

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

head(dt1)


#Attempt to remove all years before 1986

head(dt1)

IEP1986 <- subset(dt1, Year>1985)

tail(IEP1986)
print(IEP1986)

#Attempt Successful, use IEP1986 from here on

#Attempt to remove all NA values
IEP_No_NA <- IEP1986 %>% drop_na(Genus) %>% drop_na(Year) %>% drop_na(Station) %>% drop_na(Temperature) %>% 
  drop_na(Turbidity) %>% drop_na(Chl) %>% drop_na(SalSurf) %>% drop_na(pH) %>% drop_na(Genus) %>% 
  drop_na(CPUE) %>% drop_na(Secchi)


#Attempt Successful, use IEP_No_NA from here on



#I will now be using the first letter of each column for the object name until all rows I want to remove, are removed.
# Year = "Y" now, pH = "p" now, etc.

#Now I will attempt to remove columns that I am not using

#Attempt to remove unneeded Columns
IEP1986_remove <- IEP_No_NA %>% select(-BottomDepth, -Taxlifestage, -Lifestage, -Tide, -AmphipodCode, -SalBott,-Species, -Phylum, -Class, -Datetime, -Microcystis,)
print(IEP1986_remove)

#Attempt successful, use IEP1986_remove


#Check the number of unique values in Order, Faimly, and Genus to see if it is a possible stat representation for Zoops

length(unique(IEP1986_remove$Order))
#Only 6 unique values
sort(unique(IEP1986_remove$Order))


length(unique(IEP1986_remove$Family))
#Only 19 unique values
sort(unique(IEP1986_remove$Family))


length(unique(IEP1986_remove$Genus))
#Only 26 unique values
sort(unique(IEP1986_remove$Genus))

#Checking the amount of NA values present in each column
colSums(is.na(IEP1986_remove))
#Order and Family have 0 NA values, Genus has 9998 NA values 

#################Use Genus####################


#Checking the amount of 0 values present in each column 
colSums(IEP1986_remove == 0, na.rm = TRUE)

#Chl = 338 == 0
#Turbidity = 3364 == 0
#CPUE = 80625 == 0
#Volume = 50 == 0
#Undersampled = 72472 ==0



