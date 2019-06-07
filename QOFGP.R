library(jsonlite);
library(fingertipsR); 
library(plyr)
library(tidyverse)
library(stringr);
library(dplyr)

indicators <- indicators()

GPIndicators <- indicator_areatypes(,7)

#Indicators Metadata

indicatorsMetaQOF <- indicator_metadata(GPIndicators$IndicatorID) %>%
  filter(str_detect(`Data source`, regex('QOF', ignore_case = TRUE))) %>%
  filter(!grepl("retired", Indicator)) %>%
  distinct() %>%
  arrange(IndicatorID)

write_csv(indicatorsMetaQOF, "IndicatorsQOF.csv")

#Indicators Domains

domains <- indicators %>% filter(IndicatorID %in% indicatorsMetaQOF$IndicatorID) %>% 
  group_by(DomainID, DomainName) %>% 
  dplyr::summarize(No = n(),indicators = list(IndicatorID)) %>%
  filter(DomainID %in% c(2000006,2000002,2000003,3000010,3000007,3000009,3000008,2000009,1938133108,1938132829,1938133110,1938133109,2000004)) %>% toJSON()

#Print selectedDomains on the console and copy and save the result as  Domains.json

#Indicators data

#for GPS within Trafford CCG

QOF96 <- read_csv(paste('https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=', paste(indicatorsMetaQOF$IndicatorID,collapse = ","),'&child_area_type_id=7&parent_area_type_id=154&parent_area_code=E38000187', sep=""))

QOF96GP <- QOF96%>%filter(`Parent Code`=="E38000187")

#For England, Greater Manchester and Trafford CCG

ContextQOF <- fingertips_data(IndicatorID = as.vector(indicatorsMetaQOF$IndicatorID), AreaTypeID = 154) %>%
  filter(AreaCode %in% c("E38000187","E92000001","E39000037")) %>%
  rename_(`Indicator ID` = "IndicatorID", `Indicator Name` = "IndicatorName", `Parent Code` = "ParentCode",  `Parent Name` = "ParentName", `Area Code` = "AreaCode", `Area Name` = "AreaName", `Area Type` = "AreaType", `Category Type` = "CategoryType", `Time period` = "Timeperiod", `Lower CI 95.0 limit` = "LowerCI95.0limit", `Upper CI 95.0 limit` = "UpperCI95.0limit", `Lower CI 99.8 limit` = "LowerCI99.8limit", `Upper CI 99.8 limit` = "UpperCI99.8limit", `Value note`="Valuenote", `Recent Trend` = "RecentTrend", `Compared to England value or percentiles` = "ComparedtoEnglandvalueorpercentiles", `Time period Sortable` = "TimeperiodSortable", `New data` = "Newdata", `Compared to goal` = "Comparedtogoal")

totalList<- QOF96 %>% 
  filter(`Indicator ID`==114 & `Area Code` %in% c("E38000187"))

QOFCGP <- rbind.fill(QOF96GP, ContextQOF, totalList)

QOFCGP$`ComparedtoSub-regionvalueorpercentiles`[is.na(QOFCGP$`ComparedtoSub-regionvalueorpercentiles`)]<-"Not compared"

QOFCGP$`Compared to CCGs (2018/19) value or percentiles`[is.na(QOFCGP$`Compared to CCGs (2018/19) value or percentiles`)]<-"Not compared"

write_csv(QOFCGP, "QOFCGP.csv")


# GP Addresses
GP_address <- fromJSON("https://fingertips.phe.org.uk/api/area_addresses/by_parent_area_code?parent_area_code=E38000187&area_type_id=7", flatten = TRUE) %>%
  select(`Name`,`AreaTypeId`,`A1`,`A2`,`A3`,`A4`,`Postcode`,`Code`,latitude="Pos.Lat",longitude="Pos.Lng")

write_csv(GP_address, "Trafford_GPAddress.csv")

