

infile1  <- file("PUT-LOCAL-PATH-TO-DATA-FILE-HERE", open="r")  
dataTable1 <-read.csv(infile1, 
                      ,sep=","  
                      ,quot='"' 
                      , col.names=c(
                        "UniqueID",     
                        "DietStudy",     
                        "LogNumber",     
                        "Project",     
                        "GearType",     
                        "Year",     
                        "Month",     
                        "Date",     
                        "Time",     
                        "Station",     
                        "SerialNumber",     
                        "CultureOrigin",     
                        "Depth",     
                        "SurfaceTemperature",     
                        "SurfaceConductivity",     
                        "SurfacePPT",     
                        "BottomTemperature",     
                        "BottomConductivity",     
                        "BottomPPT",     
                        "Secchi",     
                        "Turbidity",     
                        "TotalBodyWeight",     
                        "Length",     
                        "GutContents",     
                        "TotalGutContentWeight",     
                        "TotalNumberOfPrey",     
                        "TotalPreyWeight",     
                        "GutFullness",     
                        "FullnessRank",     
                        "DigestionRank",     
                        "Debris",     
                        "Unid animal material",     
                        "Unid plant material",     
                        "Stomach tissue",     
                        "Worm pieces",     
                        "Acanthocyclops spp",     
                        "Acartia copepodid",     
                        "Acartia spp",     
                        "Acartiella copepodid",     
                        "Acartiella sinensis",     
                        "Barnacle nauplii",     
                        "Bosmina spp",     
                        "Calanoid copepodid",     
                        "Ceriodaphnia spp",     
                        "Chironomid larvae",     
                        "Clams",     
                        "Copepod nauplii",     
                        "Corophium type",     
                        "Crab zoea",     
                        "Cumaceans",     
                        "Cyclopoid copepodid",     
                        "Daphnia spp",     
                        "Diacyclops spp",     
                        "Diaphanosoma spp",     
                        "Diaptomus copepodid",     
                        "Diaptomus spp",     
                        "Eucyclops spp",     
                        "Eurytemora copepodid",     
                        "Eurytemora nauplii",     
                        "Eurytemora spp",     
                        "Fish eggs",     
                        "Gammarus type",     
                        "Harpacticoids",     
                        "Hyperacanthomysis longirostris",     
                        "Isopods",     
                        "Limnoithona copepodid",     
                        "Limnoithona spp",     
                        "Longfin Smelt",     
                        "Neomysis kadiakensis",     
                        "Neomysis mercedis",     
                        "Nippoleucon hinumensis",     
                        "Oithona copepodid",     
                        "Oithona davisae",     
                        "Osphranticum",     
                        "Ostracods",     
                        "Other calanoid",     
                        "Other cladocera",     
                        "Other cyclopoid",     
                        "Other insect larvae",     
                        "Other malacostraca",     
                        "Other rotifer",     
                        "Other zooplankton",     
                        "Pacific Herring",     
                        "Palaemon",     
                        "Prickly Sculpin",     
                        "Pseudodiaptomus copepodid",     
                        "Pseudodiaptomus forbesi",     
                        "Pseudodiaptomus marinus",     
                        "Pseudodiaptomus nauplii",     
                        "Pseudodiaptomus spp",     
                        "Sinocalanus copepodid",     
                        "Sinocalanus nauplii",     
                        "Sinocalanus spp",     
                        "Synchaeta",     
                        "Terrestrial invertebrates",     
                        "Tortanus copepodid",     
                        "Tortanus dextrilobatus",     
                        "Tortanus spp",     
                        "Tridentiger spp",     
                        "Unid amphipod",     
                        "Unid calanoid",     
                        "Unid cladocera",     
                        "Unid copepod",     
                        "Unid cumacean",     
                        "Unid cyclopoid",     
                        "Unid fish",     
                        "Unid mysids"    ), check.names=TRUE)




tmp_var <- character()
tmp_label <- character()
labelFrame1<-data.frame(variable=tmp_var, label=tmp_label)
rm(tmp_var, tmp_label)        


tmp_var <- character()
tmp_code <- character()                
tmp_label <- character() 
tmp_var <- c(tmp_var,"DietStudy")   
tmp_code <- c(tmp_code,"DOP")   
tmp_label <- c(tmp_label,"Directed Outflow Project Study")  
tmp_var <- c(tmp_var,"DietStudy")   
tmp_code <- c(tmp_code,"FLaSH")   
tmp_label <- c(tmp_label,"Fall Low Salinity Habitat Study")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"CVP")   
tmp_label <- c(tmp_label,"Central Valley Project")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"DJFMP")   
tmp_label <- c(tmp_label,"Delta Juvenile Fish Monitoring Program")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"EDSM")   
tmp_label <- c(tmp_label,"Enhanced Delta Smelt Monitoring")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"ELS")   
tmp_label <- c(tmp_label,"Experimental Larval Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"FMWT")   
tmp_label <- c(tmp_label,"Fall Midwater Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"GES")   
tmp_label <- c(tmp_label,"Gear Efficiency Study")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SKT")   
tmp_label <- c(tmp_label,"Spring Kodiak Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"STN")   
tmp_label <- c(tmp_label,"Summer Townet Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SWP")   
tmp_label <- c(tmp_label,"State Water Project")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"20mm")   
tmp_label <- c(tmp_label,"20mm net")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"KT")   
tmp_label <- c(tmp_label,"Kodiak Trawl net")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"MANT")   
tmp_label <- c(tmp_label,"Manta Trawl net")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"MWT")   
tmp_label <- c(tmp_label,"Midwater Trawl net")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"MWTCC")   
tmp_label <- c(tmp_label,"Midwater Trawl Covered Codend net")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"SEIN")   
tmp_label <- c(tmp_label,"Beach seine")  
tmp_var <- c(tmp_var,"GearType")   
tmp_code <- c(tmp_code,"TWNET")   
tmp_label <- c(tmp_label,"Townet")  
tmp_var <- c(tmp_var,"CultureOrigin")   
tmp_code <- c(tmp_code,"M")   
tmp_label <- c(tmp_label,"Marked")  
tmp_var <- c(tmp_var,"CultureOrigin")   
tmp_code <- c(tmp_code,"U")   
tmp_label <- c(tmp_label,"Unmarked")  
tmp_var <- c(tmp_var,"GutContents")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"No")  
tmp_var <- c(tmp_var,"GutContents")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Yes")  
tmp_var <- c(tmp_var,"FullnessRank")   
tmp_code <- c(tmp_code,"0")   
tmp_label <- c(tmp_label,"None (empty)")  
tmp_var <- c(tmp_var,"FullnessRank")   
tmp_code <- c(tmp_code,"1")   
tmp_label <- c(tmp_label,"25% full")  
tmp_var <- c(tmp_var,"FullnessRank")   
tmp_code <- c(tmp_code,"2")   
tmp_label <- c(tmp_label,"50% full")  
tmp_var <- c(tmp_var,"FullnessRank")   
tmp_code <- c(tmp_code,"3")   
tmp_label <- c(tmp_label,"75% full")  
tmp_var <- c(tmp_var,"FullnessRank")   
tmp_code <- c(tmp_code,"4")   
tmp_label <- c(tmp_label,"100% full")  
tmp_var <- c(tmp_var,"DigestionRank")   
tmp_code <- c(tmp_code,"0")   
tmp_label <- c(tmp_label,"None (fully intact prey)")  
tmp_var <- c(tmp_var,"DigestionRank")   
tmp_code <- c(tmp_code,"1")   
tmp_label <- c(tmp_label,"25% digested")  
tmp_var <- c(tmp_var,"DigestionRank")   
tmp_code <- c(tmp_code,"2")   
tmp_label <- c(tmp_label,"50% digested")  
tmp_var <- c(tmp_var,"DigestionRank")   
tmp_code <- c(tmp_code,"3")   
tmp_label <- c(tmp_label,"75% digested")  
tmp_var <- c(tmp_var,"DigestionRank")   
tmp_code <- c(tmp_code,"4")   
tmp_label <- c(tmp_label,"100% digested (items heavily digested, prey items in pieces)")  
tmp_var <- c(tmp_var,"Debris")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"Absent")  
tmp_var <- c(tmp_var,"Debris")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Present")  
tmp_var <- c(tmp_var,"Unid animal material")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"Absent")  
tmp_var <- c(tmp_var,"Unid animal material")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Present")  
tmp_var <- c(tmp_var,"Unid plant material")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"Absent")  
tmp_var <- c(tmp_var,"Unid plant material")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Present")  
tmp_var <- c(tmp_var,"Stomach tissue")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"Absent")  
tmp_var <- c(tmp_var,"Stomach tissue")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Present")  
tmp_var <- c(tmp_var,"Worm pieces")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"Absent")  
tmp_var <- c(tmp_var,"Worm pieces")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Present")  
codeLabelFrame1 <- data.frame(variable=tmp_var, code=tmp_code, label=tmp_label) 
rm(tmp_var, tmp_label, tmp_code) 

# HERE IS A LIST  OF VARIABLES from  dataTable1  AND LABELS FOR THOSE VARIABLES
labelFrame1   

# HERE IS A LIST OF VARIABLES TO WHICH CODES HAD BEEN ASSIGNED               
codeLabelFrame1           

attach(dataTable1)               
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 


summary(as.factor(UniqueID))  
summary(as.factor(DietStudy))  
summary(as.factor(Project))  
summary(as.factor(GearType))  
summary(as.factor(Station))  
summary(as.factor(SerialNumber))  
summary(as.factor(CultureOrigin))  
summary(as.factor(GutContents))  
summary(as.factor(FullnessRank))  
summary(as.factor(DigestionRank))  
summary(as.factor(Debris))  
summary(as.factor(Unid animal material))  
summary(as.factor(Unid plant material))  
summary(as.factor(Stomach tissue))  
summary(as.factor(Worm pieces))  
summary(as.numeric(LogNumber))  
summary(as.numeric(Month))  
summary(as.numeric(Depth))  
summary(as.numeric(SurfaceTemperature))  
summary(as.numeric(SurfaceConductivity))  
summary(as.numeric(SurfacePPT))  
summary(as.numeric(BottomTemperature))  
summary(as.numeric(BottomConductivity))  
summary(as.numeric(BottomPPT))  
summary(as.numeric(Secchi))  
summary(as.numeric(Turbidity))  
summary(as.numeric(TotalBodyWeight))  
summary(as.numeric(Length))  
summary(as.numeric(TotalGutContentWeight))  
summary(as.numeric(TotalNumberOfPrey))  
summary(as.numeric(TotalPreyWeight))  
summary(as.numeric(GutFullness))  
summary(as.numeric(Acanthocyclops spp))  
summary(as.numeric(Acartia copepodid))  
summary(as.numeric(Acartia spp))  
summary(as.numeric(Acartiella copepodid))  
summary(as.numeric(Acartiella sinensis))  
summary(as.numeric(Barnacle nauplii))  
summary(as.numeric(Bosmina spp))  
summary(as.numeric(Calanoid copepodid))  
summary(as.numeric(Ceriodaphnia spp))  
summary(as.numeric(Chironomid larvae))  
summary(as.numeric(Clams))  
summary(as.numeric(Copepod nauplii))  
summary(as.numeric(Corophium type))  
summary(as.numeric(Crab zoea))  
summary(as.numeric(Cumaceans))  
summary(as.numeric(Cyclopoid copepodid))  
summary(as.numeric(Daphnia spp))  
summary(as.numeric(Diacyclops spp))  
summary(as.numeric(Diaphanosoma spp))  
summary(as.numeric(Diaptomus copepodid))  
summary(as.numeric(Diaptomus spp))  
summary(as.numeric(Eucyclops spp))  
summary(as.numeric(Eurytemora copepodid))  
summary(as.numeric(Eurytemora nauplii))  
summary(as.numeric(Eurytemora spp))  
summary(as.numeric(Fish eggs))  
summary(as.numeric(Gammarus type))  
summary(as.numeric(Harpacticoids))  
summary(as.numeric(Hyperacanthomysis longirostris))  
summary(as.numeric(Isopods))  
summary(as.numeric(Limnoithona copepodid))  
summary(as.numeric(Limnoithona spp))  
summary(as.numeric(Longfin Smelt))  
summary(as.numeric(Neomysis kadiakensis))  
summary(as.numeric(Neomysis mercedis))  
summary(as.numeric(Nippoleucon hinumensis))  
summary(as.numeric(Oithona copepodid))  
summary(as.numeric(Oithona davisae))  
summary(as.numeric(Osphranticum))  
summary(as.numeric(Ostracods))  
summary(as.numeric(Other calanoid))  
summary(as.numeric(Other cladocera))  
summary(as.numeric(Other cyclopoid))  
summary(as.numeric(Other insect larvae))  
summary(as.numeric(Other malacostraca))  
summary(as.numeric(Other rotifer))  
summary(as.numeric(Other zooplankton))  
summary(as.numeric(Pacific Herring))  
summary(as.numeric(Palaemon))  
summary(as.numeric(Prickly Sculpin))  
summary(as.numeric(Pseudodiaptomus copepodid))  
summary(as.numeric(Pseudodiaptomus forbesi))  
summary(as.numeric(Pseudodiaptomus marinus))  
summary(as.numeric(Pseudodiaptomus nauplii))  
summary(as.numeric(Pseudodiaptomus spp))  
summary(as.numeric(Sinocalanus copepodid))  
summary(as.numeric(Sinocalanus nauplii))  
summary(as.numeric(Sinocalanus spp))  
summary(as.numeric(Synchaeta))  
summary(as.numeric(Terrestrial invertebrates))  
summary(as.numeric(Tortanus copepodid))  
summary(as.numeric(Tortanus dextrilobatus))  
summary(as.numeric(Tortanus spp))  
summary(as.numeric(Tridentiger spp))  
summary(as.numeric(Unid amphipod))  
summary(as.numeric(Unid calanoid))  
summary(as.numeric(Unid cladocera))  
summary(as.numeric(Unid copepod))  
summary(as.numeric(Unid cumacean))  
summary(as.numeric(Unid cyclopoid))  
summary(as.numeric(Unid fish))  
summary(as.numeric(Unid mysids))  
# You should replace 'PUT-LOCAL-PATH-TO-DATA-FILE-HERE'  (below) with the appropriate path.   
#   to your data file (e.g., c:\mydata\datafile.txt).                         
infile2  <- file("PUT-LOCAL-PATH-TO-DATA-FILE-HERE", open="r")  
dataTable2 <-read.csv(infile2, 
                      ,sep=","  
                      ,quot='"' 
                      , col.names=c(
                        "UniqueID",     
                        "DietStudy",     
                        "LogNumber",     
                        "Project",     
                        "Station",     
                        "Date",     
                        "Time",     
                        "SerialNumber",     
                        "PreyCategory",     
                        "PreyLengthSpecies",     
                        "PreyLength",     
                        "LengthEstimate",     
                        "PreyWeight",     
                        "EyeDiameter",     
                        "PreyAntennaeLength",     
                        "PreySex",     
                        "Comments"    ), check.names=TRUE)




tmp_var <- character()
tmp_label <- character()
labelFrame2<-data.frame(variable=tmp_var, label=tmp_label)
rm(tmp_var, tmp_label)        


tmp_var <- character()
tmp_code <- character()                
tmp_label <- character() 
tmp_var <- c(tmp_var,"DietStudy")   
tmp_code <- c(tmp_code,"DOP")   
tmp_label <- c(tmp_label,"Directed Outflow Project Study")  
tmp_var <- c(tmp_var,"DietStudy")   
tmp_code <- c(tmp_code,"FLaSH")   
tmp_label <- c(tmp_label,"Fall Low Salinity Habitat Study")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"CVP")   
tmp_label <- c(tmp_label,"Central Valley Project")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"DJFMP")   
tmp_label <- c(tmp_label,"Delta Juvenile Fish Monitoring Program")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"EDSM")   
tmp_label <- c(tmp_label,"Enhanced Delta Smelt Monitoring")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"ELS")   
tmp_label <- c(tmp_label,"Experimental Larval Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"FMWT")   
tmp_label <- c(tmp_label,"Fall Midwater Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"GES")   
tmp_label <- c(tmp_label,"Gear Efficiency Study")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SKT")   
tmp_label <- c(tmp_label,"Spring Kodiak Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"STN")   
tmp_label <- c(tmp_label,"Summer Townet Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SWP")   
tmp_label <- c(tmp_label,"State Water Project")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"AL")   
tmp_label <- c(tmp_label,"Antennae length")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"ED")   
tmp_label <- c(tmp_label,"Eye diameter")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"G")   
tmp_label <- c(tmp_label,"Gut of the same fish")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"M")   
tmp_label <- c(tmp_label,"Month")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"S")   
tmp_label <- c(tmp_label,"Station")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"U")   
tmp_label <- c(tmp_label,"Survey")  
tmp_var <- c(tmp_var,"LengthEstimate")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Year")  
tmp_var <- c(tmp_var,"PreySex")   
tmp_code <- c(tmp_code,"F")   
tmp_label <- c(tmp_label,"Female")  
tmp_var <- c(tmp_var,"PreySex")   
tmp_code <- c(tmp_code,"M")   
tmp_label <- c(tmp_label,"Male")  
codeLabelFrame2 <- data.frame(variable=tmp_var, code=tmp_code, label=tmp_label) 
rm(tmp_var, tmp_label, tmp_code) 

# HERE IS A LIST  OF VARIABLES from  dataTable2  AND LABELS FOR THOSE VARIABLES
labelFrame2   

# HERE IS A LIST OF VARIABLES TO WHICH CODES HAD BEEN ASSIGNED               
codeLabelFrame2           

attach(dataTable2)               
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 


summary(as.factor(UniqueID))  
summary(as.factor(DietStudy))  
summary(as.factor(Project))  
summary(as.factor(Station))  
summary(as.factor(SerialNumber))  
summary(as.factor(PreyCategory))  
summary(as.factor(PreyLengthSpecies))  
summary(as.factor(LengthEstimate))  
summary(as.factor(PreySex))  
summary(as.factor(Comments))  
summary(as.numeric(LogNumber))  
summary(as.numeric(PreyLength))  
summary(as.numeric(PreyWeight))  
summary(as.numeric(EyeDiameter))  
summary(as.numeric(PreyAntennaeLength))  
# You should replace 'PUT-LOCAL-PATH-TO-DATA-FILE-HERE'  (below) with the appropriate path.   
#   to your data file (e.g., c:\mydata\datafile.txt).                         
infile3  <- file("PUT-LOCAL-PATH-TO-DATA-FILE-HERE", open="r")  
dataTable3 <-read.csv(infile3, 
                      ,sep=","  
                      ,quot='"' 
                      , col.names=c(
                        "PreyCategory",     
                        "LifeStage",     
                        "PreyCategoryDefinition",     
                        "RoutinelyMeasured",     
                        "Phylum",     
                        "Class",     
                        "Order",     
                        "Family",     
                        "Genus",     
                        "Species",     
                        "MeasuredWetWeight",     
                        "CalculatedWetWeight",     
                        "CarbonWeight",     
                        "DryWeight",     
                        "Reference",     
                        "StartDate",     
                        "EndDate",     
                        "Comments"    ), check.names=TRUE)




tmp_var <- character()
tmp_label <- character()
labelFrame3<-data.frame(variable=tmp_var, label=tmp_label)
rm(tmp_var, tmp_label)        


tmp_var <- character()
tmp_code <- character()                
tmp_label <- character() 
tmp_var <- c(tmp_var,"RoutinelyMeasured")   
tmp_code <- c(tmp_code,"N")   
tmp_label <- c(tmp_label,"No")  
tmp_var <- c(tmp_var,"RoutinelyMeasured")   
tmp_code <- c(tmp_code,"Y")   
tmp_label <- c(tmp_label,"Yes")  
codeLabelFrame3 <- data.frame(variable=tmp_var, code=tmp_code, label=tmp_label) 
rm(tmp_var, tmp_label, tmp_code) 

# HERE IS A LIST  OF VARIABLES from  dataTable3  AND LABELS FOR THOSE VARIABLES
labelFrame3   

# HERE IS A LIST OF VARIABLES TO WHICH CODES HAD BEEN ASSIGNED               
codeLabelFrame3           

attach(dataTable3)               
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 


summary(as.factor(PreyCategory))  
summary(as.factor(LifeStage))  
summary(as.factor(PreyCategoryDefinition))  
summary(as.factor(RoutinelyMeasured))  
summary(as.factor(Phylum))  
summary(as.factor(Class))  
summary(as.factor(Order))  
summary(as.factor(Family))  
summary(as.factor(Genus))  
summary(as.factor(Species))  
summary(as.factor(Reference))  
summary(as.factor(Comments))  
summary(as.numeric(MeasuredWetWeight))  
summary(as.numeric(CalculatedWetWeight))  
summary(as.numeric(CarbonWeight))  
summary(as.numeric(DryWeight))  
# You should replace 'PUT-LOCAL-PATH-TO-DATA-FILE-HERE'  (below) with the appropriate path.   
#   to your data file (e.g., c:\mydata\datafile.txt).                         
infile4  <- file("PUT-LOCAL-PATH-TO-DATA-FILE-HERE", open="r")  
dataTable4 <-read.csv(infile4, 
                      ,sep=","  
                      ,quot='"' 
                      , col.names=c(
                        "PreyCategory",     
                        "PreyGroup",     
                        "Preservative",     
                        "PreySex",     
                        "EquationType",     
                        "a_constant",     
                        "b_exponent",     
                        "m_slope",     
                        "b_intercept",     
                        "Reference",     
                        "LengthUsed",     
                        "EqnUpdated",     
                        "Comments"    ), check.names=TRUE)




tmp_var <- character()
tmp_label <- character()
labelFrame4<-data.frame(variable=tmp_var, label=tmp_label)
rm(tmp_var, tmp_label)        


tmp_var <- character()
tmp_code <- character()                
tmp_label <- character() 
tmp_var <- c(tmp_var,"PreySex")   
tmp_code <- c(tmp_code,"All")   
tmp_label <- c(tmp_label,"Female and Male")  
tmp_var <- c(tmp_var,"PreySex")   
tmp_code <- c(tmp_code,"F")   
tmp_label <- c(tmp_label,"Female")  
tmp_var <- c(tmp_var,"PreySex")   
tmp_code <- c(tmp_code,"M")   
tmp_label <- c(tmp_label,"Male")  
tmp_var <- c(tmp_var,"EquationType")   
tmp_code <- c(tmp_code,"AL")   
tmp_label <- c(tmp_label,"Antennae length- body length")  
tmp_var <- c(tmp_var,"EquationType")   
tmp_code <- c(tmp_code,"ED")   
tmp_label <- c(tmp_label,"Eye diameter – body length")  
tmp_var <- c(tmp_var,"EquationType")   
tmp_code <- c(tmp_code,"LW")   
tmp_label <- c(tmp_label,"Length-weight")  
codeLabelFrame4 <- data.frame(variable=tmp_var, code=tmp_code, label=tmp_label) 
rm(tmp_var, tmp_label, tmp_code) 

# HERE IS A LIST  OF VARIABLES from  dataTable4  AND LABELS FOR THOSE VARIABLES
labelFrame4   

# HERE IS A LIST OF VARIABLES TO WHICH CODES HAD BEEN ASSIGNED               
codeLabelFrame4           

attach(dataTable4)               
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 


summary(as.factor(PreyCategory))  
summary(as.factor(PreyGroup))  
summary(as.factor(Preservative))  
summary(as.factor(PreySex))  
summary(as.factor(EquationType))  
summary(as.factor(Reference))  
summary(as.factor(LengthUsed))  
summary(as.factor(EqnUpdated))  
summary(as.factor(Comments))  
summary(as.numeric(a_constant))  
summary(as.numeric(b_exponent))  
summary(as.numeric(m_slope))  
summary(as.numeric(b_intercept))  
# You should replace 'PUT-LOCAL-PATH-TO-DATA-FILE-HERE'  (below) with the appropriate path.   
#   to your data file (e.g., c:\mydata\datafile.txt).                         
infile5  <- file("PUT-LOCAL-PATH-TO-DATA-FILE-HERE", open="r")  
dataTable5 <-read.csv(infile5, 
                      ,sep=","  
                      ,quot='"' 
                      , col.names=c(
                        "Project",     
                        "Station",     
                        "Latitude",     
                        "Longitude"    ), check.names=TRUE)




tmp_var <- character()
tmp_label <- character()
labelFrame5<-data.frame(variable=tmp_var, label=tmp_label)
rm(tmp_var, tmp_label)        


tmp_var <- character()
tmp_code <- character()                
tmp_label <- character() 
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"CVP")   
tmp_label <- c(tmp_label,"Central Valley Project")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"DJFMP")   
tmp_label <- c(tmp_label,"Delta Juvenile Fish Monitoring Program")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"EDSM")   
tmp_label <- c(tmp_label,"Enhanced Delta Smelt Monitoring")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"ELS")   
tmp_label <- c(tmp_label,"Experimental Larval Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"FMWT")   
tmp_label <- c(tmp_label,"Fall Midwater Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"GES")   
tmp_label <- c(tmp_label,"Gear Efficiency Study")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SKT")   
tmp_label <- c(tmp_label,"Spring Kodiak Trawl")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"STN")   
tmp_label <- c(tmp_label,"Summer Townet Survey")  
tmp_var <- c(tmp_var,"Project")   
tmp_code <- c(tmp_code,"SWP")   
tmp_label <- c(tmp_label,"State Water Project")  
codeLabelFrame5 <- data.frame(variable=tmp_var, code=tmp_code, label=tmp_label) 
rm(tmp_var, tmp_label, tmp_code) 

# HERE IS A LIST  OF VARIABLES from  dataTable5  AND LABELS FOR THOSE VARIABLES
labelFrame5   

# HERE IS A LIST OF VARIABLES TO WHICH CODES HAD BEEN ASSIGNED               
codeLabelFrame5           

attach(dataTable5)               
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 


summary(as.factor(Project))  
summary(as.factor(Station))  
summary(as.numeric(Latitude))  
summary(as.numeric(Longitude)) 