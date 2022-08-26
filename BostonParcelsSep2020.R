---
title: "City of Boston Parcel-Address Unit Processor"
output: html_notebook
---
#PREP
LOADING PACKAGES
```{r message=FALSE, warning = FALSE}
library(readr)
library(tidyverse)
library(foreign)
library(dtplyr)
```
IMPORTING DATASETS
```{r}
#Reading in the LUT lookup table from MassGIS and split into two data sets - one where TOWN_ID is populated and one for the generic/standardized use codes/descriptions where TOWN_ID = 0. The first will be used to create as many matches on the town level as possible. For those that didn't match, standardized town-independent use codes/descriptions will be joined.
LUT <- read.dbf("C:/Users/tom/Downloads/Statewide_parcels_SHP_Sep2020/L3_UC_LUT.dbf")
LUT$USE_CODE <- as.character(LUT$USE_CODE)
LUT2 <- LUT %>% subset(TOWN_ID==0) %>% select(-TOWN_ID)

#Importing the table of joined parcels and addresses. This is the result of a one-to-many spatial join executed in ArcGIS.
BostonParcelsAdds <- read_csv("~/BostonAddsParcels.txt")

#Readying the use code field for joining
BostonParcelsAdds$USE_CODE <- as.character(BostonParcelsAdds$USE_CODE)

#Importing MAPC's lookup table for standardizing use codes. This standardization will help simplify decision rules when imputing number of residential units.
MAPC_Codes <- read_delim("land_use_lookup_boston_included.txt", 
    "\t", escape_double = FALSE, trim_ws = TRUE)
names(MAPC_Codes)[names(MAPC_Codes) == 'USE_DESC'] <- 'MAPC_USE_DESC'
MAPC_Codes %>% group_by(TOWN_ID) %>% summarise(count = n())
```
JOINING THE THREE DATA SETS
```{r}
#Removing parcel dupes and joining to the MassGIS land use tables
#First isolating and removing dupes from the parcel side of the parcel-add join (we will rejoin later to get more accurate counts)
BostonParcels <- BostonParcelsAdds %>% select(-c(1:4,7:10,12:13,49:74))
#removing dupes
BostonParcels2 <- unique(BostonParcels)

#first joining on the muni-specific use_codes from MassGIS
BostonParcelsj1 <- BostonParcels2 %>% left_join(LUT, by = c('TOWN_ID','USE_CODE')) %>% subset(!is.na(USE_DESC))
#Any use codes that did not have a muni-specific match will be joined on the generic use codes (not tied to any specific TOWN_ID)
BostonParcelsj2 <- BostonParcels2 %>% subset(!(PROP_ID %in% BostonParcelsj1$PROP_ID)) %>% left_join(LUT2, by = c('USE_CODE'))
#Binding the two use_desc joins back into one data set
BostonParcels3 <- rbind(BostonParcelsj1,BostonParcelsj2)

#joining mapc standardized use codes where special exceptions exist:
BostonParcels4 <- BostonParcels3 %>% left_join(MAPC_Codes, by = c('TOWN_ID','USE_CODE'))
#Assigning a final standardized use code - MAPC's conversion if relevant, initial MassGIS use code if not:
BostonParcels4$LUCStandard <- ifelse(is.na(BostonParcels4$LUC_Assign)==TRUE, BostonParcels4$USE_CODE,BostonParcels4$LUC_Assign)

#Creating a LOC_ID level summary data set to join to address counts.
BostonParcelJoin <- BostonParcels4 %>% group_by(LOC_ID) %>% summarise(AssessorCount = n(),
                                                                      AssessorUnits = sum(UNITS),
                                                                      UseCodes = toString(unique(LUCStandard)),
                                                                      UseDesc = toString(unique(USE_DESC)))

#creating a summary df on the LOC_ID/Use Code level for reference on tricky parcels
BostonParcelReference <- BostonParcels4 %>% group_by(LOC_ID,LUCStandard,USE_DESC) %>% summarise(AssessorCount = n(),
                                                                      AssessorUnits = sum(UNITS))

#head(Boston_Residential_LOC)
#View(subset(Boston_Residential_LOC,UseCodes == '102'))
View(subset(BostonLOCs, LOC_ID == "F_775233_2954125"))

#Creating a dataframe of just the LOC_ID/Address joins
BostonAdds <- BostonParcelsAdds %>% select(c(6,59:60))
#Removing dupes
BostonAdds2 <- unique(BostonAdds)

#Creating a summary of the addresses on each LOC_ID to get a count of unique matches per LOC and a count of unique matches that have a unit number (helpful for multi-unit buildings sometimes)
BostonAddJoin <- BostonAdds2 %>% group_by(LOC_ID) %>% summarise(AddressCount = n(),
                                                                AddressCountwUnit = sum(!is.na(UNIT)))

#Joining our parcel summary with our address count summary. This is the data set that will be used to calculate units.
BostonLOCs <- BostonParcelJoin %>% left_join(BostonAddJoin, by = c("LOC_ID"))

#removing dfs that are no longer needed to help with memory usage. May also help to run gc()
rm(list = c('Boston_Residential_LOC','ResidentialBoston','BostonParcelsj1','BostonParcelsj2','BostonParcels2','BostonParcels3'))
```

#SETTING UNITS THROUGH A SERIES OF DECISION RULES
#Prepping the final units field where we will assign/impute units based on decision rules
```{r}
#initialize the final units field
BostonLOCs$residentialunits <- ""
```
#summary table to show the most frequent use codes so that we can start from the most common parcel types
```{r}
View(subset(BostonLOCs, residentialunits == "") %>% group_by(UseCodes,UseDesc) %>% summarise(count = n()) %>% arrange(desc(count)))
```


###RESIDENTIAL USES
#Single Family
```{r}
#Use the below to explore parcel details on a specific LOC_ID
BostonParcels4[BostonParcels4$LOC_ID == "F_772557_2950736",]
BostonAdds2[BostonAdds2$LOC_ID == "F_759737_2936684",]

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985'),]);nrow(BostonLOCs[BostonLOCs$UseCodes%in%  c('101', '101, 906', '101, 985') &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes%in%  c('101', '101, 906', '101, 985') &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes%in%  c('101', '101, 906', '101, 985') &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985'),])

BostonLOCs %>% subset(UseCodes %in% c('101', '101, 906', '101, 985') & residentialunits == "") %>% group_by(AssessorUnits,AddressCount,AddressCountwUnit) %>% summarise(count=n())

BostonLOCs %>% subset(UseCodes %in% c('101', '101, 906', '101, 985') & residentialunits == "")

#If the Assessor records say 1 unit, then we will set as 1 unit
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 1 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$residentialunits == ""] <-1

#If the Assessor records say 0 units but the Address count = 1, we will set as 1 unit
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$AddressCount == 1 & BostonLOCs$residentialunits == ""] <-1

#If the assessor says more than 1 unit, but address count is only 1 unit, then we will set as 1 unit
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits > 1 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$AddressCount == 1 & BostonLOCs$residentialunits == ""] <-1

#If the assessor says 0 units, and the address count has more than one and fewer than four unique unit identifiers, then we are going with the unique unit identifier count
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$AddressCountwUnit > 1  & BostonLOCs$AddressCountwUnit <= 4 & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$AddressCountwUnit > 1  & BostonLOCs$AddressCountwUnit <= 4 & BostonLOCs$residentialunits == ""]

#If the assessor says 0 units and there is only 1 or 0 unique unit identifiers associated with the address record, use 1 unit
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$AddressCountwUnit <= 1 & BostonLOCs$residentialunits == ""] <- 1

#The remaining parcels are coded as 101 but have more than 4 unique unit addresses. After investigating a handful, none appear to be single-family, but the address counts also appear to be too high. Imputing as 1 unit for all remaining 101 properties, but this rule should be revisited.
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% c('101', '101, 906', '101, 985')& BostonLOCs$residentialunits == ""] <- 1
```

#Two-Family
```{r}
#Use the below to explore parcel details on a specific LOC_ID
BostonParcels4[BostonParcels4$LOC_ID == "F_745336_2954045",]
BostonAdds2[BostonAdds2$LOC_ID == "F_759737_2936684",]

#View unmatched records
View(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985') & BostonLOCs$residentialunits == "",])

#summary of matches/percent matches
nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985'),]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985') &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985') &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985') &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% c('104', '104, 985'),])


#If Assessor says 2 units and there are two addresses, then assigning two units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 2 & BostonLOCs$UseCodes %in% c('104', '104, 985')& BostonLOCs$residentialunits == ""] <-2

#If assessor says 0 units but there are two address records, assigning two units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('104', '104, 985')& BostonLOCs$AddressCount == 2 & BostonLOCs$residentialunits == ""] <-2

#If assessor says more than two units but there are fewer than two address records, assigning two units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits > 2 & BostonLOCs$UseCodes %in% c('104', '104, 985')& BostonLOCs$AddressCount <= 2 & BostonLOCs$residentialunits == ""] <-2

#There are many listed as two-fam that have three address records, but one or more has no unit number listed. So, for those with unit address count of 2 or less we will impute as 2 units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('104', '104, 985') & BostonLOCs$AddressCountwUnit <= 2 & BostonLOCs$residentialunits == ""] <-2


#there are a significant number of records that have three addresses with units listed. We will allow props coded as 2-fam to have three units in these cases
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits <= 1 & BostonLOCs$UseCodes %in% c('104', '104, 985')& BostonLOCs$AddressCount >= 3 & BostonLOCs$AddressCountwUnit == 3 & BostonLOCs$residentialunits == ""] <- 3

#remaining addresses have assessor units of less than two and more than 3 addresses with unit counts. Spot check reveals that most of these appear to be two-families where each unit has multiple address records (listed as A&B or 1&2 for example). Making an assumption and coding all remaining two-family as two units
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% c('104', '104, 985') & BostonLOCs$residentialunits == ""] <- 2

```

#three family
```{r}
#Use the below to explore parcel details on a specific LOC_ID
BostonParcels4[BostonParcels4$LOC_ID == "F_749321_2949373",]
BostonAdds2[BostonAdds2$LOC_ID == "F_744229_2953242",]

#View unmatched records
View(BostonLOCs[BostonLOCs$UseCodes == "105" & BostonLOCs$residentialunits == "",])

#view summary table of total, matched, unmatched and percentage matched as you progress through decision rules
nrow(BostonLOCs[BostonLOCs$UseCodes == "105",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "105" &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "105" &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "105" &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes == "105",])


#if assessor says three units, then assigning three units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 3 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$residentialunits == ""] <-3

#If assessor says something other than 3 units but we have three addresses or three unit addresses, then assigning three units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits != 3 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$AddressCount == 3 & BostonLOCs$residentialunits == ""] <-3
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits != 3 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$AddressCountwUnit == 3 & BostonLOCs$residentialunits == ""] <-3

#those listed as three-family with three addresses with unit fields will be imputed as 3
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$AddressCount != 3 & BostonLOCs$AddressCountwUnit == 3 & BostonLOCs$residentialunits == ""] <-3

#records where there are no assessor units and only 2 or fewer unit address counts will be imputed as three units after spot check
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$AddressCountwUnit <=2 & BostonLOCs$residentialunits == ""] <-3

#remainder of records have too many unit addresses for the use code. 
#If a record has 4 unit addresses, we will allow four units in unit count since spot check of address records indicates strong likelihood that there are four distinct units.
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('105')& BostonLOCs$AddressCountwUnit == 4 & BostonLOCs$residentialunits == ""] <-4

#remainder with no assessor units will be coded with 3 units, since address records seem to be a bit of a mess for Boston three-fams. Spot check revealed address overcounts.
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == 0 & BostonLOCs$UseCodes %in% c('105', '105, 985') & BostonLOCs$residentialunits == ""] <- 3
```

#condos - almost all have combo codes of '102, 995' where the 995 parcel is not associated with a particular unit, but the property as a whole
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_759652_2927159",])
BostonAdds2[BostonAdds2$LOC_ID == "F_745416_2954799",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_759652_2927159",])

BostonLOCs$residentialunits[BostonLOCs$LOC_ID %in% CondoLOCS$LOC_ID] <- ""
nrow(BostonLOCs[BostonLOCs$LOC_ID %in% CondoLOCS$LOC_ID & BostonLOCs$residentialunits == "",])

condocodes <- c("102, 995","995,102","102","102, 908, 995","102, 902","102, 985","102, 995, 908","357, 102, 995","357, 995, 102","358, 357, 102, 995","358, 995, 102","102, 356, 995","102, 908, 995","357, 102","357, 358, 102, 995","320, 102, 995","")

View(BostonLOCs[BostonLOCs$UseCodes %in% condocodes & BostonLOCs$residentialunits == "",])

nrow(BostonLOCs[BostonLOCs$UseCodes %in% condocodes,]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% condocodes &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% condocodes &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% condocodes &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% condocodes,])

#where assessor units and addresses match, or where they match with address w unit counts we will go with assessor units
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == BostonLOCs$AddressCount & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$AssessorUnits == BostonLOCs$AddressCount & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""]
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits == BostonLOCs$AddressCountwUnit & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$AssessorUnits == BostonLOCs$AddressCountwUnit & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] 

#if the assessor units are within 1 unit of the address count or address count with unit, we will use the assessor units
BostonLOCs$residentialunits[abs(BostonLOCs$AssessorUnits-BostonLOCs$AddressCount) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[abs(BostonLOCs$AssessorUnits-BostonLOCs$AddressCount) <= 1  & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""]
BostonLOCs$residentialunits[abs(BostonLOCs$AssessorUnits-BostonLOCs$AddressCountwUnit) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[abs(BostonLOCs$AssessorUnits-BostonLOCs$AddressCountwUnit) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""]

#if the assessor count is within 1 unit of the address count or address count with unit, we will use the assessor units
BostonLOCs$residentialunits[abs(BostonLOCs$AssessorCount-BostonLOCs$AddressCount) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[abs(BostonLOCs$AssessorCount-BostonLOCs$AddressCount) <= 1  & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""]
BostonLOCs$residentialunits[abs(BostonLOCs$AssessorCount-BostonLOCs$AddressCountwUnit) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[abs(BostonLOCs$AssessorCount-BostonLOCs$AddressCountwUnit) <= 1 & BostonLOCs$UseCodes %in% condocodes& BostonLOCs$residentialunits == ""]

#if the assessor count is more than 0 but under 50 and the use code is 102,995, then we will impute as assessor count minus one (to remove the 995 condo code record which is a duplication)
BostonLOCs$residentialunits[BostonLOCs$AssessorCount > 0 & BostonLOCs$AssessorCount < 50 & BostonLOCs$UseCodes %in% c('102, 995','995, 102','108, 102, 995')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[BostonLOCs$AssessorCount > 0 & BostonLOCs$AssessorCount < 50 & BostonLOCs$UseCodes %in% c('102, 995','995, 102','108, 102, 995')& BostonLOCs$residentialunits == ""]-1

#if the use code is just 102 (no 995 record) and the assessor count is under 50, we will use the assessor count.
BostonLOCs$residentialunits[BostonLOCs$AssessorCount > 0 & BostonLOCs$AssessorCount < 50 & BostonLOCs$UseCodes %in% c('102')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[BostonLOCs$AssessorCount > 0 & BostonLOCs$AssessorCount < 50 & BostonLOCs$UseCodes %in% c('102')& BostonLOCs$residentialunits == ""]

#placeholder - for remaining properties using the assessor count minus one for LOCs coded as 102,995 and assessor counts for those coded 102 alone
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% condocodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[BostonLOCs$UseCodes %in% condocodes & BostonLOCs$residentialunits == ""]-1

BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% c('102')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorCount[BostonLOCs$UseCodes %in% c('102')& BostonLOCs$residentialunits == ""]

#fixing a crazy record or two:
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_754050_2957751"] <- 85

condounitsummary <- BostonLOCs$residentialunits[BostonLOCs$LOC_ID %in% CondoLOCS]
summary(as.numeric(condounitsummary, na.rm=TRUE))


#finding other LOCs where it is condo and another code, summing the number of 102 codes and then using that to impute values
CondoLOCS <- unique(subset(BostonParcels4, USE_CODE == '102')) %>% group_by(LOC_ID) %>% summarise(count=n())
nrow(CondoLOCS)
View(CondoLOCS)
nrow(BostonLOCs[BostonLOCs$LOC_ID %in% CondoLOCS$LOC_ID & BostonLOCs$residentialunits == "",])
unmatchedcondos <- BostonLOCs$LOC_ID[BostonLOCs$LOC_ID %in% CondoLOCS$LOC_ID & BostonLOCs$residentialunits == ""]
unmatchedcondos <- unique(unmatchedcondos)
length(unmatchedcondos);nrow(CondoLOCS[CondoLOCS$LOC_ID %in% unmatchedcondos,])

View(CondoLOCS)
BostonLOCs$residentialunits[BostonLOCs$LOC_ID %in% CondoLOCS$LOC_ID & BostonLOCs$residentialunits == ""]<- CondoLOCS$count[CondoLOCS$LOC_ID %in% unmatchedcondos]

```

#Apartments 4-8 units
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_756177_2952845",])
BostonParcelsAdds[BostonParcelsAdds$LOC_ID == "F_746571_2925861",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_745776_2951185",])

View(BostonLOCs[BostonLOCs$residentialunits == 0 & BostonLOCs$UseCodes %in% Codes4_8Units,])
BostonLOCs$residentialunits[BostonLOCs$residentialunits == 0 & BostonLOCs$UseCodes %in% Codes4_8Units] <- ""
Codes4_8Units <- c('111', '111, 904', '111, 945', '111, 952', '111, 985', '111, 995, 326, 903')

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes %in% Codes4_8Units,]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% Codes4_8Units &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% Codes4_8Units &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% Codes4_8Units &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% Codes4_8Units,])

View(BostonLOCs %>% subset(UseCodes %in% Codes4_8Units & residentialunits == 0))
BostonLOCs %>% subset(UseCodes %in% Codes4_8Units & residentialunits != "") %>% group_by(AssessorUnits,AddressCount,AddressCountwUnit) %>% summarise(count=n())

#If the Assessor records say between 4-8 units, then we will set as assessor units. 
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits >=4 & BostonLOCs$AssessorUnits <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$AssessorUnits >=4 & BostonLOCs$AssessorUnits <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""]

#Almost no 4-8 unit properties in Boston have unit counts from the assessor records, so we will rely heavily on address records. If the Address Count is 4-8 units we will use the address count
BostonLOCs$residentialunits[BostonLOCs$AddressCount >=4 & BostonLOCs$AddressCount <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$AddressCount >=4 & BostonLOCs$AddressCount <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""]
#if the unit address count is 4-8 units we will use the unit address count
BostonLOCs$residentialunits[BostonLOCs$AddressCountwUnit >=4 & BostonLOCs$AddressCountwUnit <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$AddressCountwUnit >=4 & BostonLOCs$AddressCountwUnit <=8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""]

#the majority of 4-8 unit properties in Boston have either less than four or more than eight addresses that matched with the location. Spot check revealed errors are all over the map. Generally, spot checked properties where there was just 1 address tended to look like 4 unit buildings and those with more than 8 unit addresses appeared to be accurate (especially for a number of larger properties that clearly have the wrong use code). We will impute four units for records with unit addresses less than 4 and we will use the unit address counts where they are over 8.
BostonLOCs$residentialunits[BostonLOCs$AddressCountwUnit < 4 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""] <- 4
BostonLOCs$residentialunits[BostonLOCs$AddressCountwUnit > 8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$AddressCountwUnit > 8 & BostonLOCs$UseCodes %in% Codes4_8Units& BostonLOCs$residentialunits == ""]

```

#Apartments with more than 8 units
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_756088_2952337",])
BostonParcelsAdds[BostonParcelsAdds$LOC_ID == "F_756970_2955366",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_745776_2951185",])

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes == "112",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "112" &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "112" &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "112" &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes == "112",])

View(BostonLOCs %>% subset(UseCodes %in% c('112') & residentialunits == ""))
BostonLOCs %>% subset(UseCodes %in% c('112') & residentialunits != "") %>% group_by(AssessorUnits,AddressCount,AddressCountwUnit) %>% summarise(count=n())

#if the assessor has a unit count higher than 8 then we will use the assessor's unit count
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits > 8 & BostonLOCs$UseCodes %in% c('112') & BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$AssessorUnits > 8 & BostonLOCs$UseCodes %in% c('112') & BostonLOCs$residentialunits == ""]

#If the Assessor records say more than 8 units and the address count or the unit address count matches we will use the assessor units 
BostonLOCs$residentialunits[BostonLOCs$AssessorUnits > 8 & BostonLOCs$AssessorUnits == BostonLOCs$AddressCount  & BostonLOCs$UseCodes %in% c('112') & BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$AssessorUnits > 8 & BostonLOCs$AssessorUnits == BostonLOCs$AddressCount  & BostonLOCs$UseCodes %in% c('112') & BostonLOCs$residentialunits == ""]

#Almost no 8+ unit properties in Boston have unit counts from the assessor records, so we will rely heavily on address records. If the address count is over 8 units, we will use it. Spot check indicated that address count may be more useful than unit address count.
BostonLOCs$residentialunits[BostonLOCs$AddressCount >8 & BostonLOCs$UseCodes %in% c('112')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$AddressCount >8 & BostonLOCs$UseCodes %in% c('112')& BostonLOCs$residentialunits == ""]

#if the address count is under 8 units, it appears to be a mixed bag. In some cases where the address count is 6-8 the address count appears to be accurate. 
BostonLOCs$residentialunits[BostonLOCs$AddressCount %in% c(6,7,8) & BostonLOCs$UseCodes %in% c('112')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$AddressCount %in% c(6,7,8) & BostonLOCs$UseCodes %in% c('112')& BostonLOCs$residentialunits == ""]

#In cases where there are 5 or fewer addresses it appears to be an error or these properties have been recently redeveloped and have more units than the initial record indicates. Assuming the assessor records and addresses will change over time and get updated as municipal records adjust, we will use 9 units for these properties.
BostonLOCs$residentialunits[BostonLOCs$AddressCount <=5 & BostonLOCs$UseCodes %in% c('112')& BostonLOCs$residentialunits == ""] <- 9
```

#Mixed Use
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_760977_2943939",])
BostonParcelsAdds[BostonParcelsAdds$LOC_ID == "F_745686_2953210",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_779407_2960656",])

#setting which use codes/groups of use codes we will handle as mixed use:
mixedusecodes <- c("013","031","013, 104","013, 300, 995", "013, 905", "013, 985", "013, 995", "013, 326", "031, 985", "031, 985, 125", "031, 326", "031, 995, 125")


#get building and residential square footage for mixed use parcels as well as the style and number of stories
MixedUseReferenceData <- subset(BostonParcels4, LUCStandard %in% mixedusecodes) %>% select(c(2,32,34,35,38))
BostonLOCs <- BostonLOCs %>% left_join(MixedUseReferenceData)

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes %in% mixedusecodes,]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% mixedusecodes &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% mixedusecodes &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% mixedusecodes &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% mixedusecodes,])

#View unmatched records
View(BostonLOCs %>% subset(UseCodes %in% mixedusecodes & residentialunits == "") %>% group_by(STYLE) %>% summarise(count=n()))
View(BostonLOCs %>% subset(UseCodes %in% mixedusecodes & residentialunits == "" & STYLE %in% c("LUXURY APARTMENT")))

#If the style column explicitly states the number of units, then we will use that information
BostonLOCs$residentialunits[BostonLOCs$STYLE == "RC: ONE RES UNIT" & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 1
BostonLOCs$residentialunits[BostonLOCs$STYLE == "RC: TWO RES UNITS" & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 2
BostonLOCs$residentialunits[BostonLOCs$STYLE == "RC: THREE RES UNITS" & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 3

#If the Style column says 4-6 unit apartments, we will impute with unit address counts as long as they are between 4-6 units. Same with 7-30 unit codes and 31-99 unit codes and 100+ unit codes
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 4-6 UNITS") & BostonLOCs$AddressCountwUnit %in% c(4,5,6) & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("APT 4-6 UNITS") & BostonLOCs$AddressCountwUnit %in% c(4,5,6) & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 7-30 UNITS") & BostonLOCs$AddressCountwUnit >=7 & BostonLOCs$AddressCountwUnit <=30 &BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("APT 7-30 UNITS") & BostonLOCs$AddressCountwUnit >=7 & BostonLOCs$AddressCountwUnit <=30 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 31-99 UNITS") & BostonLOCs$AddressCountwUnit >=31 & BostonLOCs$AddressCountwUnit <=99 &BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("APT 31-99 UNITS") & BostonLOCs$AddressCountwUnit >=31 & BostonLOCs$AddressCountwUnit <=99 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AddressCountwUnit >=100 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AddressCountwUnit >=100 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

#if the addresses were over or under in the above test, we will impute the highest in the range when unit address counts are higher and the lowest in the range when units are lower, up to 99 units.
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 4-6 UNITS") & BostonLOCs$AddressCountwUnit < 4 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 4
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 4-6 UNITS") & BostonLOCs$AddressCountwUnit > 6 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 6

BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 7-30 UNITS") & BostonLOCs$AddressCountwUnit < 7 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 7
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 7-30 UNITS") & BostonLOCs$AddressCountwUnit > 30 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 30

BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 31-99 UNITS") & BostonLOCs$AddressCountwUnit < 7 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 31
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 31-99 UNITS") & BostonLOCs$AddressCountwUnit > 30 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 99

#A few of the 100+ properties had the right range of address counts (as opposed to unit address counts) - these seemed relatively accurate and were often 100+ unit properties that are split across multiple LOC_IDs (Baker Chocolate is one example), so going with the address counts (if they are over 20 units).
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AddressCount >=20 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AddressCount >=20 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]
#one of the remaining 100+ unit properties has assessor units of over 100, so we will use that
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AssessorUnits >=100 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AssessorUnits[BostonLOCs$STYLE %in% c("APT 100+ UNITS") & BostonLOCs$AssessorUnits >=100 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

BostonParcels4[BostonParcels4$LOC_ID == "F_759652_2927159",]
View(BostonParcels4[BostonParcels4$OWNER1 == "FRANKLIN HIGHLANDS LP",])
#The remaining 100+ unit properties will need some additional research. Spot checking revealed massive undercounts of units
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_760977_2943939"] <- 149 #The Brynx 201 S Huntington
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_761209_2945325"] <- 195 #Serenity 101 S Huntington
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_763995_2947132"] <- 289 #CityView at Longwood

BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_766671_2938349"] <- 54  #Franklin Highlands - 270 units in 12 buildings across 5 LOCs coded as mixed use.
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_767226_2939198"] <- 54  #Franklin Highlands Pt 2
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_766870_2938685"] <- 54  #Franklin Highlands Pt 3
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_767311_2938608"] <- 54  #Franklin Highlands Pt 4
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_766131_2939252"] <- 54  #Franklin Highlands Pt 5
  
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_768421_2951361"] <- 218  #30 Dalton residences
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_769878_2952353"] <- 187  #Avalon Exeter
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_774231_2953527"] <- 381   #The Kensington
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_774554_2943572"] <- 238 #The Andi pt1 475 units tot
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_774786_2943910"] <- 237  #The Andi pt 2
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_776273_2950106"] <- 139 #50 W Broadway
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_779153_2960786"] <- 0   #vacant land at The Eddy
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_779407_2960656"] <- 259 #The Eddy - 10 New St
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_780185_2952188"] <- 236 #Waterside Place
BostonLOCs$residentialunits[BostonLOCs$LOC_ID == "F_781213_2952030"] <- 304 #Ora Seaport


#Will Need to revisit, but using unit address count for LOCs coded as subsidized housing 
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("SUBSD HOUSING S- 8") & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("SUBSD HOUSING S- 8") & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""]

#Same for Luxury developments where address unit counts are greater than 10
BostonLOCs$residentialunits[BostonLOCs$STYLE %in% c("LUXURY APARTMENT") & BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$STYLE %in% c("LUXURY APARTMENT") & BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$residentialunits == ""]

#Need to work on this section - for now imputing remaining mixed use parcels as 1 unit
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% mixedusecodes & BostonLOCs$residentialunits == ""] <- 1

```

#Other congregate housing
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_749893_2954017",])
BostonParcelsAdds[BostonParcelsAdds$LOC_ID == "F_756125_2955366",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_745776_2951185",])

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes == "125",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "125" &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "125" &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "125" &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes == "125",])


View(BostonLOCs %>% subset(UseCodes %in% c('125') & residentialunits == ""))
BostonLOCs %>% subset(UseCodes %in% c('125') & residentialunits != "") %>% group_by(AssessorUnits,AddressCount,AddressCountwUnit) %>% summarise(count=n())

#Assessor does not include units, so we will use unit address counts where they are greater than 10 and address counts elsewhere
BostonLOCs$residentialunits[BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$UseCodes %in% c('125')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$UseCodes %in% c('125')& BostonLOCs$residentialunits == ""]

BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% c('125')& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$UseCodes %in% c('125')& BostonLOCs$residentialunits == ""]
```

#Housing Authority - coded as all kinds of 9xx codes. Could isolate these records based on owner name, though that will certainly miss some, especially when owner is City of Boston instead of Boston Housing Authority - should total to 12,623 units in BHA properties
```{r}
#Use the below to explore parcel details on a specific LOC_ID
View(BostonParcels4[BostonParcels4$LOC_ID == "F_752281_2951259",])
BostonParcelsAdds[BostonParcelsAdds$LOC_ID == "F_745686_2953210",]
View(BostonLOCs[BostonLOCs$LOC_ID == "F_779407_2960656",])

names(BostonParcelsAdds)
BHALOCS <- BostonParcelsAdds$LOC_ID[grep("BOSTON HOUSING AUTH",BostonParcelsAdds$OWNER1)]
BHALOCS <- unique(BHALOCS)
BHALOCS
View(BostonLOCs[BostonLOCs$LOC_ID %in% BHALOCS & BostonLOCs$residentialunits == "",])
sum(as.numeric(BostonLOCs$residentialunits[BostonLOCs$LOC_ID %in% BHALOCS]))

#placeholder decision rule - imputing using the address count - still undercounts BHA units, and maybe doesn't place them in all the right places. Need to revisit with better use codes
BostonLOCs$residentialunits[BostonLOCs$LOC_ID %in% BHALOCS] <- BostonLOCs$AddressCount[BostonLOCs$LOC_ID %in% BHALOCS]

```

#multiple houses on one parcel
```{r}
BostonParcels4[BostonParcels4$LOC_ID == "F_772557_2950736",]
BostonAdds2[BostonAdds2$LOC_ID == "F_759737_2936684",]

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes == "109",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "109" &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "109" &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes == "109" &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes == "109",])

View(BostonLOCs %>% subset(UseCodes %in% c('109') & residentialunits == ""))
#all LOCs coded as 109 have relatively few addresses associated with them, so we will impute with address count
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% c('109') & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$UseCodes %in% c('109') & BostonLOCs$residentialunits == ""]

```

#very large multifamily - 113 & 114
```{r}
largemfcodes <- c("113","114")

#Use the below to explore parcel details on a specific LOC_ID
BostonParcels4[BostonParcels4$LOC_ID == "F_756897_2911563",]
BostonAdds2[BostonAdds2$LOC_ID == "F_749627_2921051",]
BostonLOCs[BostonLOCs$LOC_ID == "F_749627_2921051",]

View(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes & BostonLOCs$residentialunits == "",])

nrow(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes,]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% largemfcodes,])

#not all of these coded as large mf actually have development on them. as plaecholder, using address counts on these codes:
BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% largemfcodes & BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$UseCodes %in% largemfcodes & BostonLOCs$residentialunits == ""]

```

#dorms and group quarters
```{r}
groupquarterscodes <- c('121','121, 905', '122', '123', '125')

#use below to track progress. Counts number of records, unmatched, matched and percentage matched
nrow(BostonLOCs[BostonLOCs$UseCodes %in% groupquarterscodes,]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% groupquarterscodes &BostonLOCs$residentialunits =="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% groupquarterscodes &BostonLOCs$residentialunits !="",]);nrow(BostonLOCs[BostonLOCs$UseCodes %in% groupquarterscodes &BostonLOCs$residentialunits !="",])/nrow(BostonLOCs[BostonLOCs$UseCodes %in% groupquarterscodes,])


View(BostonLOCs %>% subset(UseCodes %in% groupquarterscodes & residentialunits == ""))
BostonLOCs %>% subset(UseCodes %in% groupquarterscodes & residentialunits != "") %>% group_by(AssessorUnits,AddressCount,AddressCountwUnit) %>% summarise(count=n())

#Assessor does not include units, so we will use unit address counts where they are greater than 10 and address counts elsewhere
BostonLOCs$residentialunits[BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$UseCodes %in% groupquarterscodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCountwUnit[BostonLOCs$AddressCountwUnit >=10 & BostonLOCs$UseCodes %in% groupquarterscodes & BostonLOCs$residentialunits == ""]

BostonLOCs$residentialunits[BostonLOCs$UseCodes %in% groupquarterscodes& BostonLOCs$residentialunits == ""] <- BostonLOCs$AddressCount[BostonLOCs$UseCodes %in% groupquarterscodes& BostonLOCs$residentialunits == ""]
```

#summarize total units and prep data for export
```{r}
#according to census, City of Boston has an estimated 303,791 housing units, most recent results show parcel analysis at 263,382, about 40k units short
getunitsummary <- BostonLOCs %>% select(c(LOC_ID, UseCodes, residentialunits))
getunitsummary$residentialunits <- as.numeric(getunitsummary$residentialunits)
#remove any outstanding dupes
getunitsummary <- unique(getunitsummary)


#this will tell you where we are in our data set in terms of total units
sum(getunitsummary$residentialunits, na.rm = TRUE)
#this will set all remaining NAs to zero
getunitsummary$residentialunits[is.na(getunitsummary$residentialunits)] <- 0

```
#export dataset with units
```{r}
write.csv(getunitsummary, "BostonLOCUnits.csv")
```





