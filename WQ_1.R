
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

#Attempt Successful, use IEP2000_NA_Year from here on

#I will now be using the first letter of each column for the object name until all rows I want to clean, are cleaned.
# Year = "Y" now, pH = "p" now, etc

#Attempt to remove all NA values in pH
IEP2000_NA_Y_p <- IEP2000_NA_Year[complete.cases(IEP2000_NA_Year[,"pH"]),]
print(IEP2000_NA_Y_p)

#Attempt Successful, use IEP2000_NA_Y_p from here on

#Attempt to remove all NA values in SalSurf 
IEP2000_NA_Y_p_S <- IEP2000_NA_Y_p[complete.cases(IEP2000_NA_Y_p[,"SalSurf"]),]
print(IEP2000_NA_Y_p_S)

#Attempt Successful, use IEP2000_NA_Y_p_S from here on

#Attempt to remove all NA values in Temperature 
IEP2000_NA_Y_p_S_Te <- IEP2000_NA_Y_p_S[complete.cases(IEP2000_NA_Y_p_S[,"Temperature"]),]
print(IEP2000_NA_Y_p_S_Te)

#Attempt Successful, use IEP2000_NA_Y_p_S_Te from here on


#Attempt to remove all NA values in Turbidity 
IEP2000_NA_Y_p_S_Te_Tu <- IEP2000_NA_Y_p_S_Te[complete.cases(IEP2000_NA_Y_p_S_Te[,"Turbidity"]),]
print(IEP2000_NA_Y_p_S_Te_Tu)

#Attempt Successful, use IEP2000_NA_Y_p_S_Te_Tu from here on


#Attempt to remove all NA values in Chlorophyll (Chl) 
IEP2000_NA_Y_p_S_Te_Tu_C <- IEP2000_NA_Y_p_S_Te_Tu[complete.cases(IEP2000_NA_Y_p_S_Te_Tu[,"Chl"]),]
print(IEP2000_NA_Y_p_S_Te_Tu_C)

#Attempt Successful, use IEP2000_NA_Y_p_S_Te_Tu_C from here on


#Attempt to remove all NA values in Family
IEP2000_clean <- IEP2000_NA_Y_p_S_Te_Tu_C[complete.cases(IEP2000_NA_Y_p_S_Te_Tu_C[,"Family"]),]
print(IEP2000_clean)

#Attempt Successful, use IEP2000_clean from here on



#I will now be using the first letter of each column for the object name until all rows I want to remove, are removed.
# Year = "Y" now, pH = "p" now, etc.

#Now I will attempt to remove columns that I am not using

#Attempt to remove bottomdepth Column
IEP2000_remove <- IEP2000_clean %>% select(-BottomDepth, -Undersampled, -Taxlifestage, -Lifestage, -Tide, -AmphipodCode, -SalBott, -Species, -Phylum)
print(IEP2000_remove)

#Attempt successful, use IEP2000_remove









