
WaterQuality <- read.csv("C:/Users/al-la/Downloads/SMSCG_wq_data_2017-2023_clean_tzone.csv")
head(WaterQuality)
tail(WaterQuality)
nchar(WaterQuality)
class(WaterQuality)
typeof(WaterQuality)
str(WaterQuality)


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


#Attempt to remove all years before 2000

head(dt1)

IEP2000 <- subset(dt1, Year>1999)

tail(IEP2000)
print(IEP2000)

#Attempt Successful, use IEP2000 from here on

#Attempt to remove all NA values
IEP_No_NA <- IEP2000[complete.cases(IEP2000[ , c(1,2,5,8,13,15,16,18,20,27)]),]

#Attempt Successful, use IEP_No_NA from here on


#Check the number of unique values in class to see if it is a possible stat representation for Zoops

length(unique(IEP2000_remove$Class))
#Only 4 unique values, too zoomed out, I will remove. 



#I will now be using the first letter of each column for the object name until all rows I want to remove, are removed.
# Year = "Y" now, pH = "p" now, etc.

#Now I will attempt to remove columns that I am not using

#Attempt to remove unneeded Columns
IEP2000_remove <- IEP_No_NA %>% select(-BottomDepth, -Undersampled, -Taxlifestage, -Lifestage, -Tide, -AmphipodCode, -SalBott, -Species, -Phylum, -Class)
print(IEP2000_remove)

#Attempt successful, use IEP2000_remove








