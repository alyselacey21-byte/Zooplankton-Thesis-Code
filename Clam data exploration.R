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