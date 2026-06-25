
library(tidyverse)
library(mgcv)

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- trimws("https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec") 
infile1 <- sub("^https","http",inUrl1)

dt1 <-read_delim(infile1
               ,delim=","  
               ,skip=1
               ,quote = "" 
               ,col_names=c(
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
                 "Undersampled"    ), col_types=list(
                   col_character(),
                   col_character(),
                   col_number(),
                   col_number(),
                   col_character(),
                   col_date("%Y-%m-%d"),
                   col_datetime("%Y-%m-%d %H:%M:%S"),
                   
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_number(),
                   col_number(),
                   col_number(),
                   col_number(),
                   col_number(),
                   col_character(),
                   col_number(),
                   col_number(),
                   col_number(),
                   col_number(),
                   col_character(),
                   col_number(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_number(),
                   col_character()),
               na=c("",".","NA",""))

unlink(infile1)

head(dt1)



Zoop1 <- subset(dt1, Year>2004)
head(Zoop1)
tail(Zoop1)
summary(Zoop1)

Zoop100 <- subset(dt1, Year<1994)
head(Zoop100)
tail(Zoop100)
summary(Zoop100)


Zoop2 <- Zoop1 %>% select(-BottomDepth, -Taxlifestage, -Lifestage, -AmphipodCode, -SalBott, -Species, -Phylum, -Class, -pH, -Family, -Taxname, -Datetime, -TowType, -SizeClass, -Volume, -Order)

Zoop101 <- Zoop100 %>% select(-BottomDepth, -Taxlifestage, -Lifestage, -AmphipodCode, -SalBott, -Species, -Phylum, -Class, -pH, -Family, -Taxname, -Datetime, -TowType, -SizeClass, -Volume, -Order)

library(dplyr)

Zoop3 <- Zoop2 %>%
  filter(Undersampled == FALSE)
summary(Zoop3)
head(Zoop3)
tail(Zoop3)

Zoop102 <- Zoop101 %>%
  filter(Undersampled == FALSE)



library(dplyr)
library(ggplot2)
library(lubridate)

#Gammarus Genus

#line and dot plot

Zoop4 <- Zoop3 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop4,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Acartiella

Zoop5 <- Zoop3 %>%
  filter(Genus == "Acartiella") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop5,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Acartiella CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()




#Eurytemora

Zoop6 <- Zoop3 %>%
  filter(Genus == "Eurytemora") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop6,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Eurytemora CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Pseudodiaptomus

Zoop7 <- Zoop3 %>%
  filter(Genus == "Pseudodiaptomus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop7,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Pseudodiaptomus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()



#Mysis
Zoop8 <- Zoop3 %>%
  filter(Genus %in% c(
    "Alienacanthomysis",
    "Deltamysis",
    "Hyperacanthomysis",
    "Neomysis",
    "Orientomysis"
  )) %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop8,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Mysis CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()

#######################first half of the data#######################


#Gammarus Genus

#line and dot plot

Zoop103 <- Zoop102 %>%
  filter(Genus == "Gammarus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop103,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Gammarus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Acartiella

Zoop104 <- Zoop102 %>%
  filter(Genus == "Acartiella") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop104,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Acartiella CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()




#Eurytemora

Zoop105 <- Zoop102 %>%
  filter(Genus == "Eurytemora") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop105,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Eurytemora CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()





#Pseudodiaptomus

Zoop106 <- Zoop102 %>%
  filter(Genus == "Pseudodiaptomus") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop106,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Pseudodiaptomus CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()



#Mysis
Zoop107 <- Zoop102 %>%
  filter(Genus %in% c(
    "Alienacanthomysis",
    "Deltamysis",
    "Hyperacanthomysis",
    "Neomysis",
    "Orientomysis"
  )) %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarize(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Zoop107,
       aes(x = Month,
           y = mean_CPUE,
           group = Year,
           color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Mean Mysis CPUE by Year",
    x = "Month",
    y = "Mean CPUE",
    color = "Year"
  ) +
  theme_bw()



