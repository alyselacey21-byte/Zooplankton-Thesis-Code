
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

#Attempt to remove all NA values in year
IEP2000_NA_Year <- IEP2000[complete.cases(IEP2000[,"Year"]),]
print(IEP2000_NA_Year)






