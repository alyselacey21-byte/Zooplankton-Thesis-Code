options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1036/6/7475c46f9c8bebf01ad24d50ee7a0ff9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Date",     
                 "Station",     
                 "LabSampleNumber",     
                 "Grab",     
                 "Year",     
                 "Month",     
                 "OrganismCode",     
                 "Count",     
                 "Phylum",     
                 "Class_level",     
                 "Order_level",     
                 "Family_level",     
                 "Genus",     
                 "Species",     
                 "Common_name",     
                 "Location",     
                 "Latitude",     
                 "Longitude"    ), check.names=TRUE)

unlink(infile1)

library(tidyverse)
library(mgcv)
library(lubridate)

#Clams are in the Phylum Mollusca, ID:6730 


#Specifically Asian and Overbite clams
dt2 %>%
  mutate(Year = year(as.Date(Date))) %>%
  filter(OrganismCode %in% c("6730", "6890")) %>%
  group_by(Year) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_count)) +
  geom_bar(stat = "identity", fill = "brown", color = "black") +
  labs(
    title = "Mean Asian clam and Overbite clam Density Over Time",
    x = "Year",
    y = "Average Clam Density"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





dt2 %>%
  mutate(Year = year(as.Date(Date)),
         OrganismCode = as.character(OrganismCode)) %>%
  filter(OrganismCode %in% c("6730", "6890")) %>%
  group_by(Year, OrganismCode) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_count, fill = OrganismCode)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(
    values = c("6730" = "steelblue", "6890" = "brown"),
    labels = c("6730" = "Asian Clam", "6890" = "Overbite Clam")
  ) +
  labs(
    title = "Mean Asian Clam and Overbite Clam Density Over Time",
    x = "Year",
    y = "Average Clam Density",
    fill = "Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



dt2_mollusca <- dt2 %>%
  filter(Phylum == "Mollusca") %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_mollusca, aes(x = Year, y = mean_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_mollusca$Common_name)))) +
  labs(x = "Year", y = "Mean Count", title = "Stacked Mean Count by Species - Mollusca") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



unique(dt2_mollusca$Common_name)




#Species categorized into general bins 

#####Clams (including macomas, tellin, semele, and other bivalves in the clam family) Amethyst Gemclam, Asian Clam, Asian Semele, Bent-nose Macoma, Brackish-water Corbula, California Lyonsia/sand clam, California Softshell Clam, Cooper Clam, Fingernail Clam, Jacknife Clam, Japanese Littleneck, Macoma, Nuttall Cockle, Ridgebeak Peaclam, Rough Piddock, San Pedro Thraciid, Softshell Clam, Tellin, Transparent Razor, Ubiquitous Peaclam, Unidentified Cardiidae
####Mussels Floater Mussel, Golden Mussel, Green Mussel, Mediterranean Mussel, Northern Horse Mussel, Straight Horse Mussel
####Snails California Ancylid, California Assiminea, Eastern Mudsnail, Gyraulus Snail, Hydrobe Snail, Menetus Snail, Pond Snail, Pouch Snail, Red-rim Melania Snail, Two-ridge Rams-horn Snail, Unidentified Hydrobioidea
###Slugs & Sea Slugs Flat Okenia/Sea Slug, Gastropod Slug, Sea Slug
####Oysters  Olympia Oyster
###Other / Miscellaneous Gastropods Eastern White Slipper Shell, Epitonium, Marine Gastropod, Paperbubble


#########Clams top 3

clam_species_Top <- c("Asian Clam", "Brackish-water Corbula", "Softshell Clam")

dt2_clams_top <- dt2 %>%
  filter(Common_name %in% clam_species_Top) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_clams_top, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Set1"))(3)) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Clam Species, Top 3 species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#########Clams other 14

clam_species <- c("Amethyst Gemclam", "Asian Semele", "Bent-nose Macoma", "California Lyonsia/sand clam", "California Softshell Clam",
                  "Cooper Clam", "Fingernail Clam", "Jacknife Clam", "Japanese Littleneck",
                  "Macoma", "Nuttall Cockle", "Ridgebeak Peaclam", "Rough Piddock",
                  "San Pedro Thraciid", "Tellin", "Transparent Razor",
                  "Ubiquitous Peaclam", "Unidentified Cardiidae")

dt2_clams <- dt2 %>%
  filter(Common_name %in% clam_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_clams, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_clams$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Clam Species, Bottom 14 Species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#########Mussels
#Just green mussels 
dt2_green_mussel <- dt2 %>%
  filter(Common_name == "Green Mussel") %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_green_mussel, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Green Mussel" = "darkgreen")) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count - Green Mussel") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



#Other 3 species
Mussel_species <- c("Floater Mussel", "Golden Mussel", "Mediterranean Mussel", "Northern Horse Mussel", "Straight Horse Mussel")

dt2_mussels <- dt2 %>%
  filter(Common_name %in% Mussel_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_mussels, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Set1"))(length(unique(dt2_mussels$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Mussel Species, Bottom 3 Species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

###All mussels

#Other 3 species
Mussel_species_all <- c("Floater Mussel", "Golden Mussel", "Green Mussel", "Mediterranean Mussel", "Northern Horse Mussel", "Straight Horse Mussel")

dt2_mussels_all <- dt2 %>%
  filter(Common_name %in% Mussel_species_all) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_mussels_all, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_mussels_all$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Mussel Species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )




##########Snails

Snail_species <- c("California Ancylid", "California Assiminea", "Eastern Mudsnail", "Gyraulus Snail", "Hydrobe Snail", "Menetus Snail", "Pond Snail", "Pouch Snail", "Red-rim Melania Snail", "Two-ridge Rams-horn Snail", "Unidentified Hydrobioidea")

dt2_snails <- dt2 %>%
  filter(Common_name %in% Snail_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_snails, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_snails$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Snail Species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


##########Slugs and Sea Slugs


Slug_species <- c("Flat Okenia, Sea Slug", "Gastropod, Slug", "Sea Slug")

dt2_slugs <- dt2 %>%
  filter(Common_name %in% Slug_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_slugs, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_slugs$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Slug and Sea Slug Species") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#Just Gastropod

Slug_species_1 <- c("Gastropod, Slug")

dt2_slugs_1 <- dt2 %>%
  filter(Common_name %in% Slug_species_1) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_slugs_1, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_slugs_1$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Slug and Sea Slug Species - Gastropod, Slug") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#The other two

Slug_species_2 <- c("Flat Okenia, Sea Slug", "Sea Slug")

dt2_slugs_2 <- dt2 %>%
  filter(Common_name %in% Slug_species_2) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")
ggplot(dt2_slugs_2, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(dt2_slugs_2$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Stacked Total Count by Slug and Sea Slug Species - Flat Okenia and Sea Slug") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


##########Oysters   

Oyster_species <- c("Olympia Oyster")

dt2_Oyster <- dt2 %>%
  filter(Common_name %in% Oyster_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_Oyster, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Olympia Oyster" = "steelblue")) +
  labs(x = "Year", y = "Total Count", title = "Total Count - Olympia Oyster") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


##########Other  Gastropods 

Other_species <- c("Eastern white slipper shell", "Epitonium", "Marine Gastropod", "Paperbubble")

dt2_Other <- dt2 %>%
  filter(Common_name %in% Other_species) %>%
  filter(!is.na(Common_name)) %>%
  group_by(Year, Common_name) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(dt2_Other, aes(x = Year, y = total_count, fill = Common_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Set2"))(length(unique(dt2_Other$Common_name)))) +
  labs(x = "Year", y = "Total Count", title = "Total Count - Other Molluscs") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
