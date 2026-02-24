
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


#Attempt to remove all years before 2000

head(dt1)

IEP2000 <- subset(dt1, Year>1999)

tail(IEP2000)
print(IEP2000)

colSums(IEP2000 ==2003 , na.rm = TRUE)

#Attempt Successful, use IEP2000 from here on


#Check the number of unique values in class to see if it is a possible stat representation for Zoops

length(unique(IEP2000$Class))
#Only 4 unique values, too zoomed out, I will remove. 



#I will now be using the first letter of each column for the object name until all rows I want to remove, are removed.
# Year = "Y" now, pH = "p" now, etc.

#Now I will attempt to remove columns that I am not using

#Attempt to remove unneeded Columns
IEP2000_remove <- IEP2000 %>% select(-BottomDepth, -Undersampled, -Taxlifestage, -Lifestage, -Tide, -AmphipodCode, -SalBott, -Species, -Phylum, -Class)
print(IEP2000_remove)

#Attempt successful, use IEP2000_remove


#Check the number of unique values in Order, Faimly, and Genus to see if it is a possible stat representation for Zoops

length(unique(IEP2000_remove$Order))
#Only 6 unique values
sort(unique(IEP2000_remove$Order))


length(unique(IEP2000_remove$Family))
#Only 20 unique values
sort(unique(IEP2000_remove$Family))


length(unique(IEP2000_remove$Genus))
#Only 27 unique values
sort(unique(IEP2000_remove$Genus))

#Checking the amount of NA values present in each column
colSums(is.na(IEP2000_remove))
#Order and Family have 0 NA values, Genus has 9998 NA values 

#I think the best to use is Family for most amount of unique value and least amount of NA colums removes



#Checking the amount of 0 values present in each column 
colSums(IEP2000_remove ==0 , na.rm = TRUE)

#Chl = 370 == 0
#Turbidity = 3908 == 0
#CPUE = 89353 == 0


#Histograms 
Z_Turbidity <- IEP2000_remove$Turbidity 
hist(log10(Z_Turbidity))

Z_Year <- IEP2000_remove$Year
hist(Z_Year)

Z_Chl <- IEP2000_remove$Chl
hist(log10(Z_Chl))

Z_Secchi <- IEP2000_remove$Secchi
hist(Z_Secchi)

Z_Temperature <- IEP2000_remove$Temperature
hist(Z_Temperature)

Z_pH <- IEP2000_remove$pH
hist(Z_pH)

Z_DO <- IEP2000_remove$DO
hist(log10(Z_DO))

Z_SalSurf <- IEP2000_remove$SalSurf
hist(Z_SalSurf)

Z_Volume <- IEP2000_remove$Volume
hist(log10(Z_Volume))

Z_CPUE <- IEP2000_remove$CPUE
hist(log10(Z_CPUE))

# break the cpue down by taxon and make a graph

library("tidyverse")

#Loop one at a ime by pressing enter

for (g in unique(IEP2000_remove$Genus)) {
  df_sub <- IEP2000_remove[IEP2000_remove$Genus == g, ]
  
  p <- ggplot(df_sub, aes(x = Genus, y = CPUE, fill = Genus)) +
    geom_col(width = 0.4, show.legend = FALSE) +
    geom_errorbar(
      data = df_sub,
      aes(x = Genus, ymin = CPUE - SE, ymax = CPUE + SE),
      width = 0.1,
      color = "gray30"
    ) +
    labs(title = paste("CPUE for", g), x = "Genus", y = "CPUE (catch per unit effort)") +
    theme_classic() +
    theme(
      axis.text.x = element_text(face = "italic"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  print(p)
  readline(prompt = "Press [Enter] for next genus...")
}


#facetwrap
IEP2000_remove_NA <- IEP2000_remove[!is.na(IEP2000_remove$Genus), ]

ggplot(IEP2000_remove_NA, aes(x = Genus, y = CPUE, fill = Genus)) + 
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = CPUE, ymax = CPUE), width = 0.15, color = "gray30") +
  facet_wrap(~ Genus, scales = "free_x") +
  labs(title = "CPUE by Genus", x = "Genus", y = "CPUE (catch per unit effort)") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "italic"))
 #remove NAs from the facet_wrap too
