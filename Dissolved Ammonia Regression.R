library(tidyverse)
library(mgcv)

#set UserAgent
options(HTTPUserAgent="EDI_CodeGen") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1282/3/c1b6ce974a8fbba8752ce5438ca729ee") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
dt1 <-read_delim(infile1  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "Year",   
                   "Month",   
                   "Region",   
                   "Flow",   
                   "Chlorophyll",   
                   "DIN",   
                   "DissAmmonia",   
                   "DissNitrateNitrite",   
                   "DissOrthophos",   
                   "Secchi",   
                   "Temperature",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_biomass_AmericanShad",   
                   "BayStudy_MidwaterTrawl_fish_biomass_PacificHerring",   
                   "BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad",   
                   "BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy",   
                   "BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt",   
                   "BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0",   
                   "BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above",   
                   "BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt",   
                   "BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_biomass_AmericanShad",   
                   "BayStudy_OtterTrawl_fish_biomass_PacificHerring",   
                   "BayStudy_OtterTrawl_fish_biomass_ThreadfinShad",   
                   "BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy",   
                   "BayStudy_OtterTrawl_fish_biomass_DeltaSmelt",   
                   "BayStudy_OtterTrawl_fish_biomass_StripedBass_age0",   
                   "BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above",   
                   "BayStudy_OtterTrawl_fish_biomass_LongfinSmelt",   
                   "BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes",   
                   "DJFMP_fish_catch_per_seine_Silverside",   
                   "DJFMP_fish_catch_per_seine_LargemouthBass",   
                   "DJFMP_fish_biomass_Silverside",   
                   "DJFMP_fish_biomass_LargemouthBass",   
                   "DJFMP_fish_catch_per_seine_Centrarchids",   
                   "DJFMP_fish_biomass_Centrarchids",   
                   "Cladoceran_BPUE",   
                   "Herbivorous_Copepods_BPUE",   
                   "Mysids_BPUE",   
                   "Predatory_Copepods_BPUE",   
                   "Rotifers_BPUE",   
                   "Cladoceran_CPUE",   
                   "Herbivorous_Copepods_CPUE",   
                   "Mysids_CPUE",   
                   "Predatory_Copepods_CPUE",   
                   "Rotifers_CPUE",   
                   "Cladoceran_JPUE",   
                   "Herbivorous_Copepods_JPUE",   
                   "Mysids_JPUE",   
                   "Predatory_Copepods_JPUE",   
                   "Rotifers_JPUE",   
                   "Corbicula_cpue",   
                   "Potamocorbula_cpue",   
                   "Amphipoda_CPUE",   
                   "Amphipoda_BPUE"   ), 
                 col_types=list(
                   col_character(),  
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ), 
                 na=c(" ",".","NA","")  )


# Convert Missing Values to NA for individual vectors 
dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
dt1$Flow <- ifelse((trimws(as.character(dt1$Flow))==trimws("NA")),NA,dt1$Flow)               
suppressWarnings(dt1$Flow <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Flow))==as.character(as.numeric("NA"))),NA,dt1$Flow))
dt1$Chlorophyll <- ifelse((trimws(as.character(dt1$Chlorophyll))==trimws("NA")),NA,dt1$Chlorophyll)               
suppressWarnings(dt1$Chlorophyll <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chlorophyll))==as.character(as.numeric("NA"))),NA,dt1$Chlorophyll))
dt1$DIN <- ifelse((trimws(as.character(dt1$DIN))==trimws("NA")),NA,dt1$DIN)               
suppressWarnings(dt1$DIN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIN))==as.character(as.numeric("NA"))),NA,dt1$DIN))
dt1$DissAmmonia <- ifelse((trimws(as.character(dt1$DissAmmonia))==trimws("NA")),NA,dt1$DissAmmonia)               
suppressWarnings(dt1$DissAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissAmmonia))==as.character(as.numeric("NA"))),NA,dt1$DissAmmonia))
dt1$DissNitrateNitrite <- ifelse((trimws(as.character(dt1$DissNitrateNitrite))==trimws("NA")),NA,dt1$DissNitrateNitrite)               
suppressWarnings(dt1$DissNitrateNitrite <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissNitrateNitrite))==as.character(as.numeric("NA"))),NA,dt1$DissNitrateNitrite))
dt1$DissOrthophos <- ifelse((trimws(as.character(dt1$DissOrthophos))==trimws("NA")),NA,dt1$DissOrthophos)               
suppressWarnings(dt1$DissOrthophos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissOrthophos))==as.character(as.numeric("NA"))),NA,dt1$DissOrthophos))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Temperature <- ifelse((trimws(as.character(dt1$Temperature))==trimws("NA")),NA,dt1$Temperature)               
suppressWarnings(dt1$Temperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature))==as.character(as.numeric("NA"))),NA,dt1$Temperature))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0 <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))
dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0 <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes))
dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes))
dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_AmericanShad))
dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_PacificHerring))
dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad))
dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy))
dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt))
dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0 <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0))
dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above))
dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt))
dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))
dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes))
dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_AmericanShad))
dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_PacificHerring))
dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_ThreadfinShad))
dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy))
dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_DeltaSmelt))
dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0 <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age0))
dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above))
dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_LongfinSmelt))
dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes))
dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes <- ifelse((trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes))==trimws("NA")),NA,dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes)               
suppressWarnings(dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes))==as.character(as.numeric("NA"))),NA,dt1$BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes))
dt1$DJFMP_fish_catch_per_seine_Silverside <- ifelse((trimws(as.character(dt1$DJFMP_fish_catch_per_seine_Silverside))==trimws("NA")),NA,dt1$DJFMP_fish_catch_per_seine_Silverside)               
suppressWarnings(dt1$DJFMP_fish_catch_per_seine_Silverside <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_catch_per_seine_Silverside))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_catch_per_seine_Silverside))
dt1$DJFMP_fish_catch_per_seine_LargemouthBass <- ifelse((trimws(as.character(dt1$DJFMP_fish_catch_per_seine_LargemouthBass))==trimws("NA")),NA,dt1$DJFMP_fish_catch_per_seine_LargemouthBass)               
suppressWarnings(dt1$DJFMP_fish_catch_per_seine_LargemouthBass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_catch_per_seine_LargemouthBass))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_catch_per_seine_LargemouthBass))
dt1$DJFMP_fish_biomass_Silverside <- ifelse((trimws(as.character(dt1$DJFMP_fish_biomass_Silverside))==trimws("NA")),NA,dt1$DJFMP_fish_biomass_Silverside)               
suppressWarnings(dt1$DJFMP_fish_biomass_Silverside <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_biomass_Silverside))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_biomass_Silverside))
dt1$DJFMP_fish_biomass_LargemouthBass <- ifelse((trimws(as.character(dt1$DJFMP_fish_biomass_LargemouthBass))==trimws("NA")),NA,dt1$DJFMP_fish_biomass_LargemouthBass)               
suppressWarnings(dt1$DJFMP_fish_biomass_LargemouthBass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_biomass_LargemouthBass))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_biomass_LargemouthBass))
dt1$DJFMP_fish_catch_per_seine_Centrarchids <- ifelse((trimws(as.character(dt1$DJFMP_fish_catch_per_seine_Centrarchids))==trimws("NA")),NA,dt1$DJFMP_fish_catch_per_seine_Centrarchids)               
suppressWarnings(dt1$DJFMP_fish_catch_per_seine_Centrarchids <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_catch_per_seine_Centrarchids))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_catch_per_seine_Centrarchids))
dt1$DJFMP_fish_biomass_Centrarchids <- ifelse((trimws(as.character(dt1$DJFMP_fish_biomass_Centrarchids))==trimws("NA")),NA,dt1$DJFMP_fish_biomass_Centrarchids)               
suppressWarnings(dt1$DJFMP_fish_biomass_Centrarchids <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DJFMP_fish_biomass_Centrarchids))==as.character(as.numeric("NA"))),NA,dt1$DJFMP_fish_biomass_Centrarchids))
dt1$Cladoceran_BPUE <- ifelse((trimws(as.character(dt1$Cladoceran_BPUE))==trimws("NA")),NA,dt1$Cladoceran_BPUE)               
suppressWarnings(dt1$Cladoceran_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cladoceran_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Cladoceran_BPUE))
dt1$Herbivorous_Copepods_BPUE <- ifelse((trimws(as.character(dt1$Herbivorous_Copepods_BPUE))==trimws("NA")),NA,dt1$Herbivorous_Copepods_BPUE)               
suppressWarnings(dt1$Herbivorous_Copepods_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Herbivorous_Copepods_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Herbivorous_Copepods_BPUE))
dt1$Mysids_BPUE <- ifelse((trimws(as.character(dt1$Mysids_BPUE))==trimws("NA")),NA,dt1$Mysids_BPUE)               
suppressWarnings(dt1$Mysids_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mysids_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Mysids_BPUE))
dt1$Predatory_Copepods_BPUE <- ifelse((trimws(as.character(dt1$Predatory_Copepods_BPUE))==trimws("NA")),NA,dt1$Predatory_Copepods_BPUE)               
suppressWarnings(dt1$Predatory_Copepods_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Predatory_Copepods_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Predatory_Copepods_BPUE))
dt1$Rotifers_BPUE <- ifelse((trimws(as.character(dt1$Rotifers_BPUE))==trimws("NA")),NA,dt1$Rotifers_BPUE)               
suppressWarnings(dt1$Rotifers_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rotifers_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Rotifers_BPUE))
dt1$Cladoceran_CPUE <- ifelse((trimws(as.character(dt1$Cladoceran_CPUE))==trimws("NA")),NA,dt1$Cladoceran_CPUE)               
suppressWarnings(dt1$Cladoceran_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cladoceran_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Cladoceran_CPUE))
dt1$Herbivorous_Copepods_CPUE <- ifelse((trimws(as.character(dt1$Herbivorous_Copepods_CPUE))==trimws("NA")),NA,dt1$Herbivorous_Copepods_CPUE)               
suppressWarnings(dt1$Herbivorous_Copepods_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Herbivorous_Copepods_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Herbivorous_Copepods_CPUE))
dt1$Mysids_CPUE <- ifelse((trimws(as.character(dt1$Mysids_CPUE))==trimws("NA")),NA,dt1$Mysids_CPUE)               
suppressWarnings(dt1$Mysids_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mysids_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Mysids_CPUE))
dt1$Predatory_Copepods_CPUE <- ifelse((trimws(as.character(dt1$Predatory_Copepods_CPUE))==trimws("NA")),NA,dt1$Predatory_Copepods_CPUE)               
suppressWarnings(dt1$Predatory_Copepods_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Predatory_Copepods_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Predatory_Copepods_CPUE))
dt1$Rotifers_CPUE <- ifelse((trimws(as.character(dt1$Rotifers_CPUE))==trimws("NA")),NA,dt1$Rotifers_CPUE)               
suppressWarnings(dt1$Rotifers_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rotifers_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Rotifers_CPUE))
dt1$Cladoceran_JPUE <- ifelse((trimws(as.character(dt1$Cladoceran_JPUE))==trimws("NA")),NA,dt1$Cladoceran_JPUE)               
suppressWarnings(dt1$Cladoceran_JPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cladoceran_JPUE))==as.character(as.numeric("NA"))),NA,dt1$Cladoceran_JPUE))
dt1$Herbivorous_Copepods_JPUE <- ifelse((trimws(as.character(dt1$Herbivorous_Copepods_JPUE))==trimws("NA")),NA,dt1$Herbivorous_Copepods_JPUE)               
suppressWarnings(dt1$Herbivorous_Copepods_JPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Herbivorous_Copepods_JPUE))==as.character(as.numeric("NA"))),NA,dt1$Herbivorous_Copepods_JPUE))
dt1$Mysids_JPUE <- ifelse((trimws(as.character(dt1$Mysids_JPUE))==trimws("NA")),NA,dt1$Mysids_JPUE)               
suppressWarnings(dt1$Mysids_JPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mysids_JPUE))==as.character(as.numeric("NA"))),NA,dt1$Mysids_JPUE))
dt1$Predatory_Copepods_JPUE <- ifelse((trimws(as.character(dt1$Predatory_Copepods_JPUE))==trimws("NA")),NA,dt1$Predatory_Copepods_JPUE)               
suppressWarnings(dt1$Predatory_Copepods_JPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Predatory_Copepods_JPUE))==as.character(as.numeric("NA"))),NA,dt1$Predatory_Copepods_JPUE))
dt1$Rotifers_JPUE <- ifelse((trimws(as.character(dt1$Rotifers_JPUE))==trimws("NA")),NA,dt1$Rotifers_JPUE)               
suppressWarnings(dt1$Rotifers_JPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rotifers_JPUE))==as.character(as.numeric("NA"))),NA,dt1$Rotifers_JPUE))
dt1$Corbicula_cpue <- ifelse((trimws(as.character(dt1$Corbicula_cpue))==trimws("NA")),NA,dt1$Corbicula_cpue)               
suppressWarnings(dt1$Corbicula_cpue <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Corbicula_cpue))==as.character(as.numeric("NA"))),NA,dt1$Corbicula_cpue))
dt1$Potamocorbula_cpue <- ifelse((trimws(as.character(dt1$Potamocorbula_cpue))==trimws("NA")),NA,dt1$Potamocorbula_cpue)               
suppressWarnings(dt1$Potamocorbula_cpue <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Potamocorbula_cpue))==as.character(as.numeric("NA"))),NA,dt1$Potamocorbula_cpue))
dt1$Amphipoda_CPUE <- ifelse((trimws(as.character(dt1$Amphipoda_CPUE))==trimws("NA")),NA,dt1$Amphipoda_CPUE)               
suppressWarnings(dt1$Amphipoda_CPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Amphipoda_CPUE))==as.character(as.numeric("NA"))),NA,dt1$Amphipoda_CPUE))
dt1$Amphipoda_BPUE <- ifelse((trimws(as.character(dt1$Amphipoda_BPUE))==trimws("NA")),NA,dt1$Amphipoda_BPUE)               
suppressWarnings(dt1$Amphipoda_BPUE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Amphipoda_BPUE))==as.character(as.numeric("NA"))),NA,dt1$Amphipoda_BPUE))


# Observed issues when reading the data. An empty list is good!
print("Here is a list of possible problems. An empty list is good!")
print(problems(dt1)) 
# Here is the structure of the input data tibble:
print(" ")
print("Glimpse of dt1")
print(glimpse(dt1)) 
# And some statistical summaries of the data 
print(summary(dt1))
# Get more details on character variables

print(" ")
print("Summary of Month")
print(summary(as.factor(dt1$Month)))
print(" ")
print("Summary of Region")
print(summary(as.factor(dt1$Region))) 
infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1282/3/3edb31f4b24e8e6221c660e6af2eb11f") 
infile2 <-sub("^https","http",infile2)
# This creates a tibble named: dt2 
dt2 <-read_delim(infile2  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "Year",   
                   "Region",   
                   "Flow",   
                   "Chlorophyll",   
                   "DIN",   
                   "DissAmmonia",   
                   "DissNitrateNitrite",   
                   "DissOrthophos",   
                   "Secchi",   
                   "Temperature",   
                   "FMWT_fish_biomass_AmericanShad",   
                   "FMWT_fish_biomass_PacificHerring",   
                   "FMWT_fish_biomass_ThreadfinShad",   
                   "FMWT_fish_biomass_NorthernAnchovy",   
                   "FMWT_fish_biomass_DeltaSmelt",   
                   "FMWT_fish_biomass_StripedBass_age0",   
                   "FMWT_fish_biomass_LongfinSmelt",   
                   "FMWT_fish_catch_per_tow_AmericanShad",   
                   "FMWT_fish_catch_per_tow_PacificHerring",   
                   "FMWT_fish_catch_per_tow_ThreadfinShad",   
                   "FMWT_fish_catch_per_tow_NorthernAnchovy",   
                   "FMWT_fish_catch_per_tow_DeltaSmelt",   
                   "FMWT_fish_catch_per_tow_StripedBass_age0",   
                   "FMWT_fish_catch_per_tow_LongfinSmelt",   
                   "FMWT_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "FMWT_fish_biomass_Marine_pelagic_forage_fishes",   
                   "FMWT_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "FMWT_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age1above",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age1above",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_biomass_AmericanShad",   
                   "BayStudy_MidwaterTrawl_fish_biomass_PacificHerring",   
                   "BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad",   
                   "BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy",   
                   "BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt",   
                   "BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0",   
                   "BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age1above",   
                   "BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt",   
                   "BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_biomass_AmericanShad",   
                   "BayStudy_OtterTrawl_fish_biomass_PacificHerring",   
                   "BayStudy_OtterTrawl_fish_biomass_ThreadfinShad",   
                   "BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy",   
                   "BayStudy_OtterTrawl_fish_biomass_DeltaSmelt",   
                   "BayStudy_OtterTrawl_fish_biomass_StripedBass_age0",   
                   "BayStudy_OtterTrawl_fish_biomass_StripedBass_age1above",   
                   "BayStudy_OtterTrawl_fish_biomass_LongfinSmelt",   
                   "BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes",   
                   "DJFMP_fish_catch_per_seine_Silverside",   
                   "DJFMP_fish_biomass_Silverside",   
                   "STN_fish_catch_per_tow_AmericanShad",   
                   "STN_fish_catch_per_tow_DeltaSmelt",   
                   "STN_fish_catch_per_tow_LongfinSmelt",   
                   "STN_fish_catch_per_tow_NorthernAnchovy",   
                   "STN_fish_catch_per_tow_PacificHerring",   
                   "STN_fish_catch_per_tow_StripedBass_age0",   
                   "STN_fish_catch_per_tow_ThreadfinShad",   
                   "STN_fish_catch_per_tow_Estuarine_pelagic_forage_fishes",   
                   "STN_fish_catch_per_tow_Marine_pelagic_forage_fishes",   
                   "STN_fish_biomass_AmericanShad",   
                   "STN_fish_biomass_DeltaSmelt",   
                   "STN_fish_biomass_LongfinSmelt",   
                   "STN_fish_biomass_NorthernAnchovy",   
                   "STN_fish_biomass_PacificHerring",   
                   "STN_fish_biomass_StripedBass_age0",   
                   "STN_fish_biomass_ThreadfinShad",   
                   "STN_fish_biomass_Estuarine_pelagic_forage_fishes",   
                   "STN_fish_biomass_Marine_pelagic_forage_fishes",   
                   "Cladoceran_BPUE",   
                   "Herbivorous_Copepods_BPUE",   
                   "Mysids_BPUE",   
                   "Predatory_Copepods_BPUE",   
                   "Rotifers_BPUE",   
                   "Cladoceran_CPUE",   
                   "Herbivorous_Copepods_CPUE",   
                   "Mysids_CPUE",   
                   "Predatory_Copepods_CPUE",   
                   "Rotifers_CPUE",   
                   "Cladoceran_JPUE",   
                   "Herbivorous_Copepods_JPUE",   
                   "Mysids_JPUE",   
                   "Predatory_Copepods_JPUE",   
                   "Rotifers_JPUE",   
                   "Corbicula_cpue",   
                   "Potamocorbula_cpue",   
                   "Amphipoda_CPUE",   
                   "Amphipoda_BPUE"   ), 
                 col_types=list(
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ), 
                 na=c(" ",".","NA","")  )




summary(dt1)
head(dt1)
tail(dt1)



ggplot(dt1, aes(x = Year, y = DissAmmonia)) +
  geom_line(color = "blue") +
  geom_point(color = "black") +
  labs(title = "Dissolved Ammonia Over Time",
       x = "Year",
       y = "Dissolved Ammonia (mg/L)") + theme(axis.text.x = element_text(angle = 45, hjust = 1))




options(HTTPUserAgent="EDI_CodeGen") 
infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/539/4/58dd1dde8e38614a9cc48794f527bdec") 
infile2 <-sub("^https","http",infile2)
# This creates a tibble named: dt2
dt2 <-read_delim(infile2  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
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
                   "Undersampled"   ), 
                 col_types=list( 
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_character(),  
                   col_date("%Y-%m-%d"),   
                   col_datetime("%Y-%m-%d %H:%M:%S"), 
                   
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() ,  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(), 
                   col_number() ,  
                   col_character()), 
                 na=c(" ",".","NA","")  )


dt1_sum <- dt1 %>%
  group_by(Year, Station) %>%
  summarise(CPUE = mean(CPUE, na.rm = TRUE), .groups = "drop")


