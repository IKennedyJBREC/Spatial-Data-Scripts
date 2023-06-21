library(tidyverse)
library(readxl)
library(magrittr)
library(ggthemes)
library(writexl)
library(car)
library(openxlsx)
library(writexl)
library(h2o)
library(h2otools)

#Need to have 'ModelData' present from Radius Modeling Scheme
ModelData <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\ModelData.xlsx")
#TPO2020 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2020 Mill Data (SY2021)\\SURVEY OUTPUT\\TELEFORM CSV\\2020_TPO_Mail_Phone_2.2.22.csv")
TPO2021 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\TELEFORM CSV\\2021_TPO_Mail_Phone_1.19.23.CSV") 

Overflow <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\TELEFORM CSV\\TPO_2021_SPECIES_OVERFLOW_1.19.23.xlsx")

Overflow <- Overflow %>%
  select(TPOID, SPECIES_CODE,COUNTYNUMBER:COUNTY_SPECIES_PCT)

OTest <- Overflow %>%
  group_by(TPOID, COUNTY) %>%
  pivot_wider(values_from = COUNTY, names_from = COUNTYNUMBER, names_prefix = "COUNTY_NAME_") %>%
  select(-c(SPECIES_CODE, COUNTY_SPECIES_PCT)) 

ExtraCounties <- OTest %>%
  distinct(TPOID, COUNTY_NAME_9) %>%
  filter(COUNTY_NAME_9 != "NULL")

for (i in 10:78){
  Rando <- as.symbol(paste0("COUNTY_NAME_", i))

  x <- OTest %>%
    distinct(TPOID, .data[[Rando]]) %>%
    filter(.data[[Rando]] != "NULL")
  ExtraCounties <- ExtraCounties %>%
    full_join(x, by = 'TPOID')
  rm(x)

  next
}
rm(i, Rando, OTest)

Overflow <- Overflow %>%
  pivot_wider(names_from = c(COUNTYNUMBER,SPECIES_CODE), values_from = COUNTY_SPECIES_PCT, names_glue = "COUNTY_{COUNTYNUMBER}_AMT_{SPECIES_CODE}") 


State_Code_Conversion <- function(Data){
  Data %>%
    mutate(MILL_STATE = case_when(MILL_STATECD == 9 ~ "CT",
                                  MILL_STATECD == 10 ~ "DE",
                                  MILL_STATECD == 17 ~ "IL", 
                                  MILL_STATECD == 18 ~ "IN",
                                  MILL_STATECD == 19 ~ "IA",
                                  MILL_STATECD == 20 ~ "KS",
                                  MILL_STATECD == 23 ~ "ME",
                                  MILL_STATECD == 24 ~ "MD",
                                  MILL_STATECD == 25 ~ "MA",
                                  MILL_STATECD == 26 ~ "MI",
                                  MILL_STATECD == 27 ~ "MN",
                                  MILL_STATECD == 29 ~ "MO",
                                  MILL_STATECD == 31 ~ "NE",
                                  MILL_STATECD == 33 ~ "NH",
                                  MILL_STATECD == 34 ~ "NJ",
                                  MILL_STATECD == 36 ~ "NY",
                                  MILL_STATECD == 38 ~ "ND",
                                  MILL_STATECD == 39 ~ "OH",
                                  MILL_STATECD == 42 ~ "PA",
                                  MILL_STATECD == 44 ~ "RI",
                                  MILL_STATECD == 46 ~ "SD",
                                  MILL_STATECD == 50 ~ "VT",
                                  MILL_STATECD == 54 ~ "WV",
                                  MILL_STATECD == 55 ~ "WI"))
}
rmse <- function (y_pred, y_true) {
  RMSE <- sqrt(mean((y_true - y_pred)^2))
  return(RMSE)
}


TPO2021 <- TPO2021 %>%
  filter(TPOID %in% ModelData$TPOID) %>%
  filter(!is.na(AMOUNT)) %>%
  filter(is.na(RWAMT_UNIT_MEASURE_CD_OTHERTXT) | RWAMT_UNIT_MEASURE_CD_OTHERTXT != "Logs") %>%
  mutate(RWAMT_UNIT_MEASURE_CD = ifelse(is.na(RWAMT_UNIT_MEASURE_CD) | RWAMT_UNIT_MEASURE_CD == 99, 1, RWAMT_UNIT_MEASURE_CD)) 

TPO2021OG <- TPO2021

TPO2021 <- TPO2021 %>%
  select(TPOID, MILL_NAME, AMOUNT, UNIT_MEASURE_CD:AMOUNT_TOTAL_OTHERTXT) %>%
  filter(!is.na(AMOUNT_TOTAL_19)| 
           !is.na(AMOUNT_TOTAL_20)| 
           !is.na(AMOUNT_TOTAL_21)| 
           !is.na(AMOUNT_TOTAL_22)| 
           !is.na(AMOUNT_TOTAL_124)| 
           !is.na(AMOUNT_TOTAL_23)| 
           !is.na(AMOUNT_TOTAL_24)| 
           !is.na(AMOUNT_TOTAL_25)| 
           !is.na(AMOUNT_TOTAL_26)| 
           !is.na(AMOUNT_TOTAL_27)| 
           !is.na(AMOUNT_TOTAL_1)| 
           !is.na(AMOUNT_TOTAL_2)| 
           !is.na(AMOUNT_TOTAL_4)| 
           !is.na(AMOUNT_TOTAL_5)| 
           !is.na(AMOUNT_TOTAL_28)| 
           !is.na(AMOUNT_TOTAL_29)| 
           !is.na(AMOUNT_TOTAL_30)| 
           !is.na(AMOUNT_TOTAL_131)| 
           !is.na(AMOUNT_TOTAL_133)| 
           !is.na(AMOUNT_TOTAL_7)| 
           !is.na(AMOUNT_TOTAL_8)| 
           !is.na(AMOUNT_TOTAL_10)| 
           !is.na(AMOUNT_TOTAL_11)| 
           !is.na(AMOUNT_TOTAL_12)| 
           !is.na(AMOUNT_TOTAL_14)| 
           !is.na(AMOUNT_TOTAL_15)| 
           !is.na(AMOUNT_TOTAL_17)| 
           !is.na(AMOUNT_TOTAL_35)| 
           !is.na(AMOUNT_TOTAL_36)| 
           !is.na(AMOUNT_TOTAL_6)| 
           !is.na(AMOUNT_TOTAL_37)| 
           !is.na(AMOUNT_TOTAL_38)| 
           !is.na(AMOUNT_TOTAL_39)| 
           !is.na(AMOUNT_TOTAL_40)| 
           !is.na(AMOUNT_TOTAL_999)| 
           !is.na(AMOUNT_TOTAL_998)| 
           !is.na(AMOUNT_TOTAL_887))

TPO2021$AMOUNT_TOTAL_2 <- as.numeric(TPO2021$AMOUNT_TOTAL_2)
TPO2021$AMOUNT_TOTAL_131 <- as.numeric(TPO2021$AMOUNT_TOTAL_131)

Percents <- rowSums(TPO2021[, 6:42], na.rm = TRUE) == 100

TPO2021 <- TPO2021 %>%
  cbind(Percents)

AddtoHundred <- TPO2021 %>%
  filter(Percents == TRUE) %>%
  select(-Percents) 

AddtoHundred_Amounts <- AddtoHundred %>%
  select(AMOUNT_TOTAL_19:AMOUNT_TOTAL_887) 

AddtoHundredIDs <- AddtoHundred$TPOID

AddtoHundred_Amounts <- AddtoHundred_Amounts * .01

AddtoHundredIDs <- AddtoHundredIDs %>%
  cbind(AddtoHundred_Amounts)  %>%
  dplyr::rename("TPOID" = ".")

rm(AddtoHundred_Amounts)

Volumes <- TPO2021 %>%
  filter(Percents == FALSE) 

Volumes$VolumeBinary <- rowSums(Volumes[, 6:42], na.rm = TRUE) == Volumes$AMOUNT
#sum(VolumeBinary, na.rm = TRUE)

Issues <- Volumes %>%
  filter(VolumeBinary != TRUE)

Volumes <- Volumes %>%
  filter(VolumeBinary == TRUE) %>%
  select(-c(UNIT_MEASURE_CD:UNIT_MEASURE_CD_OTHERTXT, AMOUNT_TOTAL_OTHERTXT:VolumeBinary))

rm(Percents)

VolumesIds <- Volumes[,1]

Volumes <- Volumes %>%
  select(-TPOID) 

Volumes[,3:39] <- Volumes[,3:39]/as.numeric(Volumes[, 2])

Volumes <- Volumes %>%
  select(-AMOUNT) %>%
  cbind(VolumesIds) %>%
  dplyr::rename("TPOID" = "VolumesIds") 

Volumes <- Volumes[, c(39,1:38)]

AddtoHundred <- AddtoHundred %>%
  select(TPOID, MILL_NAME)


AddtoHundredIDs <- AddtoHundredIDs %>%
  left_join(AddtoHundred, by = 'TPOID')

AddtoHundred <- AddtoHundredIDs[, c(1, 39, 2:38)]


AddtoHundred <- AddtoHundred %>%
  rbind(Volumes)

TPO2021OG <- TPO2021OG %>%
  filter(TPOID %in% AddtoHundred$TPOID) 
TPO2021 <- TPO2021OG
rm(TPO2021OG, Volumes)

GeoInfo <- TPO2021 %>%
  select(c(TPOID, COUNTY_NAME_1:SPEC_OTHERTXT_887))

Test <- names(GeoInfo) %>%
  str_match("AMT_19") 

Test1 <- which(!is.na(Test))

Test2 <- GeoInfo %>% 
  summarize(AMT_19 = rowSums(GeoInfo[, Test1], na.rm = TRUE))

Species <- c(20:22, 124, 23:24, 1, 25:26, 2, 27, 4:5, 28:30, 131, 133, 7:8, 10:12, 14:15, 17, 35:36, 6, 37:40, 999, 998, 887)
Species<- paste0("AMT_", Species)



for (i in Species){
  
  Test <- names(GeoInfo) %>%
    str_match(i) 
  Test1 <- which(!is.na(Test))
  Test3 <- GeoInfo[, Test1]

  if (sum(sapply(Test3, is.character)) == 0){
    Test3
  }else{
    Test3 <- Test3 %>%
      mutate_if(is.character, as.numeric)
  }
 
Test2[[i]] <- rowSums(Test3, na.rm = TRUE)

}

