
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

#Loop one at a time by pressing enter

for (g in unique(IEP2000_remove$Genus)) {
  df_sub <- IEP2000_remove[IEP2000_remove$Genus == g, ]
  
  p <- ggplot(df_sub, aes(x = Genus, y = CPUE, fill = Genus)) +
    geom_col(width = 0.4, show.legend = FALSE) +
    geom_errorbar(
      data = df_sub,
      aes(x = Genus, ymin = CPUE, ymax = CPUE),
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

 #Individual histograms
library("tidyverse")
library("dplyr")
library("ggthemes")

ggplot(IEP2000_remove_NA, aes(fill=Genus, y=CPUE, x=Genus)) + 
  geom_bar(position='stack', stat='identity') +
  theme_wsj()

Acanthocyclops <- "Acanthocyclops"

# Filter for that genus
Acanthocyclops_filtered <- IEP2000_remove %>%
  filter(Genus == Acanthocyclops)

ggplot(Acanthocyclops_filtered, aes(x = Acanthocyclops)) +
  geom_bar(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "CPUE of Acanthocyclops", x = "Acanthocyclops", y = "CPUE")



Acartia <- "Acartia"

# Filter for that genus
Acartia_filtered <- IEP2000_remove %>%
  filter(Genus == Acartia)

ggplot(Acartia_filtered, aes(x = Acartia)) +
  geom_bar(binwidth = 0.5, fill = "#f18e8a", color = "black") +
  labs(title = "CPUE of Acartia", x = "Acartia", y = "CPUE")


Acartiella <- "Acartiella"

# Filter for that genus
Acartiella_filtered <- IEP2000_remove %>%
  filter(Genus == Acartiella)

ggplot(Acartiella_filtered, aes(x = Acartiella)) +
  geom_bar(binwidth = 0.5, fill = "#7e0c08", color = "black") +
  labs(title = "CPUE of Acartiella", x = "Acartiella", y = "CPUE")



Alienacanthomysis <- "Alienacanthomysis"

# Filter for that genus
Alienacanthomysis_filtered <- IEP2000_remove %>%
  filter(Genus == Alienacanthomysis)

ggplot(Alienacanthomysis_filtered, aes(x = Alienacanthomysis)) +
  geom_bar(binwidth = 0.5, fill = "orange", color = "black") +
  labs(title = "CPUE of Alienacanthomysis", x = "Alienacanthomysis", y = "CPUE")



Americorophium <- "Americorophium "

# Filter for that genus
Americorophium_filtered <- IEP2000_remove %>%
  filter(Genus == Americorophium)

ggplot(Americorophium_filtered, aes(x = Americorophium)) +
  geom_bar(binwidth = 0.5, fill = "#ecae59", color = "black") +
  labs(title = "CPUE of Americorophium", x = "Americorophium", y = "CPUE")



Asplanchna <- "Asplanchna"

# Filter for that genus
Asplanchna_filtered <- IEP2000_remove %>%
  filter(Genus == Asplanchna)

ggplot(Asplanchna_filtered, aes(x = Asplanchna)) +
  geom_bar(binwidth = 0.5, fill = "#955b0b", color = "black") +
  labs(title = "CPUE of Asplanchna", x = "Asplanchna", y = "CPUE")



Bosmina <- "Bosmina"

# Filter for that genus
Bosmina_filtered <- IEP2000_remove %>%
  filter(Genus == Bosmina)

ggplot(Bosmina_filtered, aes(x = Bosmina)) +
  geom_bar(binwidth = 0.5, fill = "yellow", color = "black") +
  labs(title = "CPUE of Bosmina", x = "Bosmina", y = "CPUE")



Crangonyx <- "Crangonyx"

# Filter for that genus
Crangonyx_filtered <- IEP2000_remove %>%
  filter(Genus == Crangonyx)

ggplot(Crangonyx_filtered, aes(x = Crangonyx)) +
  geom_bar(binwidth = 0.5, fill = "#d2d67a", color = "black") +
  labs(title = "CPUE of Crangonyx", x = "Crangonyx", y = "CPUE")



Daphnia <- "Daphnia"

# Filter for that genus
Daphnia_filtered <- IEP2000_remove %>%
  filter(Genus == Daphnia)

ggplot(Daphnia_filtered, aes(x = Daphnia)) +
  geom_bar(binwidth = 0.5, fill = "#a6ad06", color = "black") +
  labs(title = "CPUE of Daphnia", x = "Daphnia", y = "CPUE")



Eurytemora <- "Eurytemora"

# Filter for that genus
Eurytemora_filtered <- IEP2000_remove %>%
  filter(Genus == Eurytemora)

ggplot(Eurytemora_filtered, aes(x = Eurytemora)) +
  geom_bar(binwidth = 0.5, fill = "#91e296", color = "black") +
  labs(title = "CPUE of Eurytemora", x = "Eurytemora", y = "CPUE")



Gammarus <- "Gammarus"

# Filter for that genus
Gammarus_filtered <- IEP2000_remove %>%
  filter(Genus == Gammarus)

ggplot(Gammarus_filtered, aes(x = Gammarus)) +
  geom_bar(binwidth = 0.5, fill = "#037a0b", color = "black") +
  labs(title = "CPUE of Gammarus", x = "Gammarus", y = "CPUE")



Hyalella <- "Hyalella"

# Filter for that genus
Hyalella_filtered <- IEP2000_remove %>%
  filter(Genus == Hyalella)

ggplot(Hyalella_filtered, aes(x = Hyalella)) +
  geom_bar(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "CPUE of Hyalella", x = "Hyalella", y = "CPUE")


Hyperacanthomysis <- "Hyperacanthomysis"

# Filter for that genus
Hyperacanthomysis_filtered <- IEP2000_remove %>%
  filter(Genus == Hyperacanthomysis)

ggplot(Hyperacanthomysis_filtered, aes(x = Hyperacanthomysis)) +
  geom_bar(binwidth = 0.5, fill = "#50ecf2", color = "black") +
  labs(title = "CPUE of Hyperacanthomysis", x = "Hyperacanthomysis", y = "CPUE")


Keratella <- "Keratella"

# Filter for that genus
Keratella_filtered <- IEP2000_remove %>%
  filter(Genus == Keratella)

ggplot(Keratella_filtered, aes(x = Keratella)) +
  geom_bar(binwidth = 0.5, fill = "#a4b8ee", color = "black") +
  labs(title = "CPUE of Keratella", x = "Keratella", y = "CPUE")


Limnoithona <- "Limnoithona"

# Filter for that genus
Limnoithona_filtered <- IEP2000_remove %>%
  filter(Genus == Limnoithona)

ggplot(Limnoithona_filtered, aes(x = Limnoithona)) +
  geom_bar(binwidth = 0.5, fill = "#08319f", color = "black") +
  labs(title = "CPUE of Limnoithona", x = "Limnoithona", y = "CPUE")


Neomysis <- "Neomysis"

# Filter for that genus
Neomysis_filtered <- IEP2000_remove %>%
  filter(Genus == Neomysis)

ggplot(Neomysis_filtered, aes(x = Neomysis)) +
  geom_bar(binwidth = 0.5, fill = "#bceaec", color = "black") +
  labs(title = "CPUE of Neomysis", x = "Neomysis", y = "CPUE")


Oithona <- "Oithona"

# Filter for that genus
Oithona_filtered <- IEP2000_remove %>%
  filter(Genus == Oithona)

ggplot(Oithona_filtered, aes(x = Oithona)) +
  geom_bar(binwidth = 0.5, fill = "#07868b", color = "black") +
  labs(title = "CPUE of Oithona", x = "Oithona", y = "CPUE")


Orientomysis <- "Orientomysis"

# Filter for that genus
Orientomysis_filtered <- IEP2000_remove %>%
  filter(Genus == Orientomysis)

ggplot(Orientomysis_filtered, aes(x = Orientomysis)) +
  geom_bar(binwidth = 0.5, fill = "purple", color = "black") +
  labs(title = "CPUE of Orientomysis", x = "Orientomysis", y = "CPUE")


Polyarthra <- "Polyarthra"

# Filter for that genus
Polyarthra_filtered <- IEP2000_remove %>%
  filter(Genus == Polyarthra)

ggplot(Polyarthra_filtered, aes(x = Polyarthra)) +
  geom_bar(binwidth = 0.5, fill = "#d197dc", color = "black") +
  labs(title = "CPUE of Polyarthra", x = "Polyarthra", y = "CPUE")



Pseudodiaptomus <- "Pseudodiaptomus"

# Filter for that genus
Pseudodiaptomus_filtered <- IEP2000_remove %>%
  filter(Genus == Pseudodiaptomus)

ggplot(Pseudodiaptomus_filtered, aes(x = Pseudodiaptomus)) +
  geom_bar(binwidth = 0.5, fill = "#84049c", color = "black") +
  labs(title = "CPUE of Pseudodiaptomus", x = "Pseudodiaptomus", y = "CPUE")


Sinocalanus <- "Sinocalanus"

# Filter for that genus
Sinocalanus_filtered <- IEP2000_remove %>%
  filter(Genus == Sinocalanus)

ggplot(Sinocalanus_filtered, aes(x = Sinocalanus)) +
  geom_bar(binwidth = 0.5, fill = "#f133b4", color = "black") +
  labs(title = "CPUE of Sinocalanus", x = "Sinocalanus", y = "CPUE")


Sinocorophium <- "Sinocorophium"

# Filter for that genus
Sinocorophium_filtered <- IEP2000_remove %>%
  filter(Genus == Sinocorophium)

ggplot(Sinocorophium_filtered, aes(x = Sinocorophium)) +
  geom_bar(binwidth = 0.5, fill = "#e5a8d1", color = "black") +
  labs(title = "CPUE of Sinocorophium", x = "Sinocorophium", y = "CPUE")


Synchaeta <- "Synchaeta"

# Filter for that genus
Synchaeta_filtered <- IEP2000_remove %>%
  filter(Genus == Synchaeta)

ggplot(Synchaeta_filtered, aes(x = Synchaeta)) +
  geom_bar(binwidth = 0.5, fill = "#8d0560", color = "black") +
  labs(title = "CPUE of Synchaeta", x = "Synchaeta", y = "CPUE")


Tortanus <- "Tortanus"

# Filter for that genus
Tortanus_filtered <- IEP2000_remove %>%
  filter(Genus == Tortanus)

ggplot(Tortanus_filtered, aes(x = Tortanus)) +
  geom_bar(binwidth = 0.5, fill = "#81f94e", color = "black") +
  labs(title = "CPUE of Tortanus", x = "Tortanus", y = "CPUE")


Trichocerca <- "Trichocerca"

# Filter for that genus
Trichocerca_filtered <- IEP2000_remove %>%
  filter(Genus == Trichocerca)

ggplot(Trichocerca_filtered, aes(x = Trichocerca)) +
  geom_bar(binwidth = 0.5, fill = "#709f5c", color = "black") +
  labs(title = "CPUE of Trichocerca", x = "Trichocerca", y = "CPUE")