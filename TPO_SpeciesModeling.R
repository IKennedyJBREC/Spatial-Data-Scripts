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

TPO2020 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2020 Mill Data (SY2021)\\SURVEY OUTPUT\\TELEFORM CSV\\2020_TPO_Mail_Phone_2.2.22.csv")
TPO2021 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\TELEFORM CSV AND OVERFLOW SHEETS\\2021_TPO_Mail_Phone_3.28.23.CSV")

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

TylerMills <- c('09-2021-11-10', '09-2021-18-10', '09-2021-23-10', '09-2021-36-10',
                '17-2021-414-10', '18-2021-218-10','19-2021-55-10', '31-2021-56-110', 
                '33-2021-22-10', '33-2021-28-10', '33-2021-41-10', '34-2021-4-10',
                '34-2021-7-10', '34-2021-13-10', '34-2021-27-10', '38-2021-46-10', 
                '39-2021-151-10', '39-2021-163-10', '42-2021-139-10', '44-2021-6-10', 
                '46-2021-4-60', '44-2021-1-10', '46-2021-29-90')

TPO2021 <- TPO2021 %>%
  filter(!TPOID %in% TylerMills)

rm(TylerMills)

# Maine LAT/LON
# METest <- TPO2021 %>%
#   filter(MILL_STATE == "ME") %>%
#   mutate(MILL_STREET1 = tolower(MILL_STREET1)) 
# 
# ME <- read.xlsx("C:\\Users\\ikenn\\Downloads\\MEGeocodes.xlsx")
# MEAddys <- ME %>%
#   select(Address, MILL_LAT, MILL_LON) %>%
#   mutate(Address = tolower(Address))
# rm(ME)
# 
# METest <- METest %>%
#   left_join(MEAddys, by = c('MILL_STREET1' = 'Address'))

TPO2021_ExtraVars <- TPO2021 %>%
  select(TPOID, PORTABLE_CD, NBR_EMPLOYEES_ALL, URBAN_WOOD_PCT, EXPORTED_CD, EQUIPMENT_CD_112:EQUIPMENT_CD_307, EQUIPMENT_CD_302:EQUIPMENT_CD_317, EQUIPMENT_NONE) %>%
  mutate(URBAN_WOOD_PCT = ifelse(is.na(URBAN_WOOD_PCT), 0, URBAN_WOOD_PCT))

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

rm(Volumes, TPO2021, VolumesIds, AddtoHundredIDs)

DFNames <- names(AddtoHundred)

TPO2020 <- TPO2020 %>%
  select(TPOID, AMOUNT, UNIT_MEASURE_CD:AMOUNT_TOTAL_OTHERTXT) %>%
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
TPO2020$AMOUNT_TOTAL_19 <- as.numeric(TPO2020$AMOUNT_TOTAL_19)

Percents2020 <- rowSums(TPO2020[, 5:41], na.rm = TRUE) == 100

TPO2020 <- TPO2020 %>%
  cbind(Percents2020)

AddtoHundred2020 <- TPO2020 %>%
  filter(Percents2020 == TRUE) %>%
  select(-Percents2020) 

AddtoHundred_Amounts2020 <- AddtoHundred2020 %>%
  select(AMOUNT_TOTAL_19:AMOUNT_TOTAL_887) 

AddtoHundred2020 <- AddtoHundred2020$TPOID

AddtoHundred_Amounts2020 <- AddtoHundred_Amounts2020 * .01

AddtoHundred2020 <- AddtoHundred2020 %>%
  cbind(AddtoHundred_Amounts2020) %>%
  dplyr::rename('TPOID' = '.')

rm(AddtoHundred_Amounts2020)

Volumes2020 <- TPO2020 %>%
  filter(Percents2020 == FALSE) 

Volumes2020$VolumeBinary <- rowSums(Volumes2020[, 5:41], na.rm = TRUE) == Volumes2020$AMOUNT
#sum(VolumeBinary, na.rm = TRUE)

Issues2020 <- Volumes2020 %>%
  filter(VolumeBinary != TRUE)

Volumes2020 <- Volumes2020 %>%
  filter(VolumeBinary == TRUE) %>%
  select(-c(UNIT_MEASURE_CD:UNIT_MEASURE_CD_OTHERTXT, AMOUNT_TOTAL_OTHERTXT:VolumeBinary))

rm(Percents2020)

VolumesIds2020 <- Volumes2020[,1]

Volumes2020 <- Volumes2020 %>%
  select(-TPOID) 

Volumes2020[,2:38] <- Volumes2020[,2:38]/as.numeric(Volumes2020[, 1])

Volumes2020 <- Volumes2020 %>%
  select(-AMOUNT) %>%
  cbind(VolumesIds2020) %>%
  dplyr::rename(TPOID = VolumesIds2020) 

Volumes2020 <- Volumes2020[, c(38,1:37)]

AddtoHundred2020 <- AddtoHundred2020 %>%
  rbind(Volumes2020)

rm(Volumes2020, TPO2020, VolumesIds2020)



Sample2020 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2020 Mill Data (SY2021)\\2021_Survey Year_(2020 Timber Processing Info)\\DATA\\MAILING_LIST\\SAMPLE.csv")

Sample2020 <- Sample2020 %>%
  select(MILL_STATECD:MILL_NAME)

Sample2020 <- Sample2020 %>%
  mutate(MILL_TYPE_CD = ifelse(MILL_TYPE_CD %in% c(100, 111), 110, MILL_TYPE_CD),
         TPOID = ifelse(MILL_STATECD == 9, 
                        paste0("0", MILL_STATECD, "-2021-", MILL_NBR, "-", MILL_TYPE_CD), 
                        paste0(MILL_STATECD, "-2021-", MILL_NBR, "-", MILL_TYPE_CD))) %>%
  select(-c(MILL_STATECD, MILL_NBR, MILL_TYPE_CD)) %>%
  filter(MILL_FORM_ID %in% AddtoHundred2020$TPOID)

# Need to figure out WV Join issues
AddtoHundred2020 <- AddtoHundred2020 %>%
  left_join(Sample2020, by = c('TPOID'= 'MILL_FORM_ID')) %>%
  mutate(TPOID = TPOID.y) %>%
  select(-TPOID.y)

AddtoHundred2020 <- AddtoHundred2020 %>%
  filter(!is.na(TPOID))

AddtoHundred2020 <- AddtoHundred2020[, c(1, 39, 2:38)]

Test2020 <- AddtoHundred2020 %>%
  select(TPOID, MILL_NAME, AMOUNT_TOTAL_133, AMOUNT_TOTAL_39, AMOUNT_TOTAL_999, AMOUNT_TOTAL_14)

TestJoin <- AddtoHundred %>%
  select(TPOID, MILL_NAME, AMOUNT_TOTAL_133, AMOUNT_TOTAL_39, AMOUNT_TOTAL_999, AMOUNT_TOTAL_14) %>%
  full_join(Test2020, by = 'TPOID')

AddtoHundredFinal <- AddtoHundred %>%
  full_join(AddtoHundred2020, by = 'TPOID')


AddtoHundredFinal <- AddtoHundredFinal %>%
  mutate(AMOUNT_TOTAL_19.x = if_else(!is.na(AMOUNT_TOTAL_19.x), AMOUNT_TOTAL_19.x, AMOUNT_TOTAL_19.y),
         AMOUNT_TOTAL_20.x = if_else(!is.na(AMOUNT_TOTAL_20.x), AMOUNT_TOTAL_20.x, AMOUNT_TOTAL_20.y),
         AMOUNT_TOTAL_21.x = if_else(!is.na(AMOUNT_TOTAL_21.x), AMOUNT_TOTAL_21.x, AMOUNT_TOTAL_21.y),
         AMOUNT_TOTAL_22.x = if_else(!is.na(AMOUNT_TOTAL_22.x), AMOUNT_TOTAL_22.x, AMOUNT_TOTAL_22.y),
         AMOUNT_TOTAL_124.x = if_else(!is.na(AMOUNT_TOTAL_124.x), AMOUNT_TOTAL_124.x, AMOUNT_TOTAL_124.y),
         AMOUNT_TOTAL_23.x = if_else(!is.na(AMOUNT_TOTAL_23.x), AMOUNT_TOTAL_23.x, AMOUNT_TOTAL_23.y),
         AMOUNT_TOTAL_24.x = if_else(!is.na(AMOUNT_TOTAL_24.x), AMOUNT_TOTAL_24.x, AMOUNT_TOTAL_24.y),
         AMOUNT_TOTAL_25.x = if_else(!is.na(AMOUNT_TOTAL_25.x), AMOUNT_TOTAL_25.x, AMOUNT_TOTAL_25.y),
         AMOUNT_TOTAL_26.x = if_else(!is.na(AMOUNT_TOTAL_26.x), AMOUNT_TOTAL_26.x, AMOUNT_TOTAL_26.y),
         AMOUNT_TOTAL_27.x = if_else(!is.na(AMOUNT_TOTAL_27.x), AMOUNT_TOTAL_27.x, AMOUNT_TOTAL_27.y),
         AMOUNT_TOTAL_1.x = if_else(!is.na(AMOUNT_TOTAL_1.x), AMOUNT_TOTAL_1.x, AMOUNT_TOTAL_1.y),
         AMOUNT_TOTAL_2.x = if_else(!is.na(AMOUNT_TOTAL_2.x), AMOUNT_TOTAL_2.x, AMOUNT_TOTAL_2.y),
         AMOUNT_TOTAL_4.x = if_else(!is.na(AMOUNT_TOTAL_4.x), AMOUNT_TOTAL_4.x, AMOUNT_TOTAL_4.y),
         AMOUNT_TOTAL_5.x = if_else(!is.na(AMOUNT_TOTAL_5.x), AMOUNT_TOTAL_5.x, AMOUNT_TOTAL_5.y),
         AMOUNT_TOTAL_28.x = if_else(!is.na(AMOUNT_TOTAL_28.x), AMOUNT_TOTAL_28.x, AMOUNT_TOTAL_28.y),
         AMOUNT_TOTAL_29.x = if_else(!is.na(AMOUNT_TOTAL_29.x), AMOUNT_TOTAL_29.x, AMOUNT_TOTAL_29.y),
         AMOUNT_TOTAL_30.x = if_else(!is.na(AMOUNT_TOTAL_30.x), AMOUNT_TOTAL_30.x, AMOUNT_TOTAL_30.y),
         AMOUNT_TOTAL_131.x = if_else(!is.na(AMOUNT_TOTAL_131.x), AMOUNT_TOTAL_131.x, AMOUNT_TOTAL_131.y),
         AMOUNT_TOTAL_133.x = if_else(!is.na(AMOUNT_TOTAL_133.x), AMOUNT_TOTAL_133.x, AMOUNT_TOTAL_133.y),
         AMOUNT_TOTAL_7.x = if_else(!is.na(AMOUNT_TOTAL_7.x), AMOUNT_TOTAL_7.x, AMOUNT_TOTAL_7.y),
         AMOUNT_TOTAL_8.x = if_else(!is.na(AMOUNT_TOTAL_8.x), AMOUNT_TOTAL_8.x, AMOUNT_TOTAL_8.y),
         AMOUNT_TOTAL_10.x = if_else(!is.na(AMOUNT_TOTAL_10.x), AMOUNT_TOTAL_10.x, AMOUNT_TOTAL_10.y),
         AMOUNT_TOTAL_11.x = if_else(!is.na(AMOUNT_TOTAL_11.x), AMOUNT_TOTAL_11.x, AMOUNT_TOTAL_11.y),
         AMOUNT_TOTAL_12.x = if_else(!is.na(AMOUNT_TOTAL_12.x), AMOUNT_TOTAL_12.x, AMOUNT_TOTAL_12.y),
         AMOUNT_TOTAL_14.x = if_else(!is.na(AMOUNT_TOTAL_14.x), AMOUNT_TOTAL_14.x, AMOUNT_TOTAL_14.y),
         AMOUNT_TOTAL_15.x = if_else(!is.na(AMOUNT_TOTAL_15.x), AMOUNT_TOTAL_15.x, AMOUNT_TOTAL_15.y),
         AMOUNT_TOTAL_17.x = if_else(!is.na(AMOUNT_TOTAL_17.x), AMOUNT_TOTAL_17.x, AMOUNT_TOTAL_17.y),
         AMOUNT_TOTAL_35.x = if_else(!is.na(AMOUNT_TOTAL_35.x), AMOUNT_TOTAL_35.x, AMOUNT_TOTAL_35.y),
         AMOUNT_TOTAL_36.x = if_else(!is.na(AMOUNT_TOTAL_36.x), AMOUNT_TOTAL_36.x, AMOUNT_TOTAL_36.y),
         AMOUNT_TOTAL_6.x = if_else(!is.na(AMOUNT_TOTAL_6.x), AMOUNT_TOTAL_6.x, AMOUNT_TOTAL_6.y),
         AMOUNT_TOTAL_37.x = if_else(!is.na(AMOUNT_TOTAL_37.x), AMOUNT_TOTAL_37.x, AMOUNT_TOTAL_37.y),
         AMOUNT_TOTAL_38.x = if_else(!is.na(AMOUNT_TOTAL_38.x), AMOUNT_TOTAL_38.x, AMOUNT_TOTAL_38.y),
         AMOUNT_TOTAL_39.x = if_else(!is.na(AMOUNT_TOTAL_39.x), AMOUNT_TOTAL_39.x, AMOUNT_TOTAL_39.y),
         AMOUNT_TOTAL_40.x = if_else(!is.na(AMOUNT_TOTAL_40.x), AMOUNT_TOTAL_40.x, AMOUNT_TOTAL_40.y),
         AMOUNT_TOTAL_999.x = if_else(!is.na(AMOUNT_TOTAL_999.x), AMOUNT_TOTAL_999.x, AMOUNT_TOTAL_999.y),
         AMOUNT_TOTAL_998.x = if_else(!is.na(AMOUNT_TOTAL_998.x), AMOUNT_TOTAL_998.x, AMOUNT_TOTAL_998.y),
         AMOUNT_TOTAL_887.x = if_else(!is.na(AMOUNT_TOTAL_887.x), AMOUNT_TOTAL_887.x, AMOUNT_TOTAL_887.y))

AddtoHundredFinal <- AddtoHundredFinal[, 1:39]

names(AddtoHundredFinal) <- DFNames

AddtoHundredFinal <- AddtoHundredFinal %>%
  select(-MILL_NAME)



AddtoHundredFinal[is.na(AddtoHundredFinal)] = 0


rm(TestJoin, Test2020, Sample2020, AddtoHundred2020, AddtoHundred)

# Sample2021 <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\NR_CALLS\\SampleFrame_2021_Updated.xlsx") %>%
#   select(TPOID, MILL_NAME, MILL_STATECD, TOT_MCF, MILL_TYPE_CD) %>%
#   mutate(MILL_TYPE_CD = if_else(MILL_TYPE_CD %in% c(100,111), 110, MILL_TYPE_CD))

Predictions = read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Downloads\\TPO_Test\\TPOSamplePredicted_1.19.23.xlsx")
#   "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\TPOSamplePredicted.xlsx"
# Predictions <- read_xlsx(path = path, sheet = 1) %>%
#   rbind(read_xlsx(path = path, sheet = 2)) %>%
#   rbind(read_xlsx(path = path, sheet = 3)) %>%
#   rbind(read_xlsx(path = path, sheet = 4)) %>%
#   rbind(read_xlsx(path = path, sheet = 5)) %>%
#   rbind(read_xlsx(path = path, sheet = 6)) %>%
#   rbind(read_xlsx(path = path, sheet = 7)) %>%
#   rbind(read_xlsx(path = path, sheet = 8)) %>%
#   rbind(read_xlsx(path = path, sheet = 9)) 

# AllMills <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MillLocations.xlsx")



# Predictions <- Predictions %>%
#   left_join(AllMills, by = 'TPOID') %>%
#   mutate(MILL_LAT.x = as.numeric(MILL_LAT.x), 
#          MILL_LON.x = as.numeric(MILL_LON.x)) %>%
#   dplyr::rename(MILL_LAT = MILL_LAT.x, MILL_LON = MILL_LON.x) %>%
#   mutate(MILL_LAT = if_else(is.na(MILL_LAT), MILL_LAT.y, MILL_LAT),
#          MILL_LON = if_else(is.na(MILL_LON), MILL_LON.y, MILL_LON)) %>%
#   select(-c(MILL_LAT.y, MILL_LON.y))

# rm(AllMills)

# Predictions <- Predictions %>%
#   #mutate(TPOID = paste0(MILL_STATECD, "-2021-", MILL_NBR, "-", MTC)) %>%
#   mutate(MILL_STATE = ifelse(MILL_STATE == "45690", "OH", MILL_STATE),
#          TPOID = ifelse(MILL_STATE == "CT", paste0("0", TPOID), TPOID)) %>%
#   mutate(TOT_MCF = TOT_MCF^2)

# Predictions <- Predictions[-323,]

# Sample2021 <- Sample2021 %>%
#   mutate(TPOID = ifelse(MILL_STATECD == 9, paste0("0", TPOID), TPOID)) %>%
#   select(MILL_NAME, MILL_TYPE_CD, TPOID, MILL_STATECD)

# PredictionsTest <- Predictions %>%
#   filter(is.na(TPOID) | str_detect(TPOID, "NA")) 

# Predictions <- Predictions %>%
#   filter(!is.na(TPOID) | !str_detect(TPOID, "NA")) 


# Predictions <- Predictions %>%
#   mutate(MILL_TYPE_CD = if_else(MILL_TYPE_CD == "S", 10, 110))
# 
# Predictions <- Predictions %>%
#          mutate(MILL_TYPE_CD = if_else(as.character(MILL_TYPE_CD) != str_extract(TPOID, "\\d+$"), as.numeric(str_extract(TPOID, "\\d+$")), MILL_TYPE_CD))

# sum(tolower(Sample2021$MILL_NAME) %in% tolower(Predictions$MILL_NAME))

Predictions$MILL_NAME <- tolower(Predictions$MILL_NAME)
# Sample2021$MILL_NAME <- tolower(Sample2021$MILL_NAME)

# PredictionsTest <- Predictions %>%
#   filter(is.na(TPOID)) %>%
#   left_join(Sample2021, by = 'MILL_NAME') %>%
#   mutate(MILL_TYPE_CD.x = MILL_TYPE_CD.y,
#          TPOID.x = TPOID.y) %>%
#   select(-c(MILL_TYPE_CD.y:MILL_STATECD)) %>%
#   dplyr::rename(MILL_TYPE_CD = MILL_TYPE_CD.x, TPOID = TPOID.x)

# Predictions <- Predictions %>%
#   filter(!is.na(TPOID)) %>%
#   rbind(PredictionsTest)

Dups <- Predictions %>%
  filter(duplicated(TPOID))
rm(Sample2021)

CoordReplacements <- read.csv("C:\\Users\\ikenn\\Downloads\\NoCoords2.csv")

CoordReplacements <- CoordReplacements %>%
  select(TPOID, POINT_X, POINT_Y)

Predictions <- Predictions %>%
  left_join(CoordReplacements, by = 'TPOID')

Predictions <- Predictions %>%
  mutate(MILL_LAT = ifelse(!is.na(POINT_Y), POINT_Y, MILL_LAT),
         MILL_LON = ifelse(!is.na(POINT_X), POINT_X, MILL_LON)) %>%
  select(-c(POINT_X, POINT_Y))

rm(CoordReplacements)

# NoCoords <- Predictions %>%
#   filter(is.na(MILL_LAT) | is.na(MILL_LON))
# 
# write.xlsx(NoCoords, file = "C:\\Users\\ikenn\\Downloads\\NoCoords.xlsx")


#Need to join WV using Mill Name, missing MILL_NBR in Predicted Sample
# WV <- AddtoHundredFinal %>%
#   filter(startsWith(TPOID, "54"))
# 
# AddtoHundredFinal <- AddtoHundredFinal %>%
#   filter(!startsWith(TPOID, "54"))

Predictions <- Predictions %>%
  left_join(AddtoHundredFinal, by = 'TPOID')

Predictions <- Predictions %>%
  rename(MILL_TYPE_CD = MTC) %>%
  mutate(Region = as.factor(Region),
         MILL_STATE = as.factor(MILL_STATE),
         MILL_TYPE_CD = as.factor(MILL_TYPE_CD))

HighVolMTCs <- Predictions %>%
  filter(MILL_TYPE_CD %in% c(30,40,117))

Predictions <- Predictions %>%
  filter(!MILL_TYPE_CD %in% c(30,40,117))

PredictionsJB <- Predictions %>%
  distinct(TPOID, .keep_all = TRUE) %>%
  select(!c(TPOID, MILL_STATE, Region, MILL_TYPE_CD, MILL_LAT, MILL_LON, TOT_MCF, AMOUNT_TOTAL_19:AMOUNT_TOTAL_887)) 


Predictions <- Predictions %>%
  mutate(LOG_TOT_MCF = log10(TOT_MCF)) %>%
  select(-TOT_MCF) %>%
  select(TPOID, MILL_STATE, Region, MILL_TYPE_CD, MILL_LAT, MILL_LON, LOG_TOT_MCF, AMOUNT_TOTAL_19:AMOUNT_TOTAL_887) %>%
  distinct(TPOID, .keep_all = TRUE)


CompleteCoords <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\SAMPLE RESEARCH\\SampleFrame_2022_LatLonCheck.xlsx", sheet = 2)
CompleteCoords <- CompleteCoords %>%
  mutate(MILL_STATECD = ifelse(MILL_STATECD == "9", "09", MILL_STATECD),
         TPOID = paste0(MILL_STATECD, "-2021-", MILL_NBR, "-", MILL_TYPE_CD)) %>%
  select(TPOID, MILL_LAT, MILL_LON)

Predictions <- Predictions %>%
  left_join(CompleteCoords, by = 'TPOID')

Predictions <- Predictions %>%
  mutate(MILL_LAT.x = ifelse(!is.na(MILL_LAT.y), MILL_LAT.y, MILL_LAT.x),
         MILL_LON.x = ifelse(!is.na(MILL_LON.y), MILL_LON.y, MILL_LON.x)) %>%
  select(-c(MILL_LAT.y, MILL_LON.y)) %>%
  rename(MILL_LAT = MILL_LAT.x, MILL_LON = MILL_LON.x)

Predictions$MILL_LAT <- as.numeric(Predictions$MILL_LAT)
Predictions$MILL_LON <- as.numeric(Predictions$MILL_LON)

ModelData <- Predictions %>%
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


# Rename Species Variables
# RenameSpeciesVars <- function(x) {
#   x <- x %>%
#     rename("Ash" = AMOUNT_TOTAL_19,
#            "Aspen/B. Poplar" = AMOUNT_TOTAL_20,
#            "Basswood" = AMOUNT_TOTAL_21, 
#            "Beech" = AMOUNT_TOTAL_22, 
#            "W./Paper Birch" = AMOUNT_TOTAL_124,
#            "Y. Birch" = AMOUNT_TOTAL_23,
#            "Other Birch" = AMOUNT_TOTAL_24,
#            "Cedar/Juniper" = AMOUNT_TOTAL_1,
#            "B. Cherry" = AMOUNT_TOTAL_25,
#            "Cottonwood" = AMOUNT_TOTAL_26,
#            "Cypress" = AMOUNT_TOTAL_2,
#            "Elm" = AMOUNT_TOTAL_27,
#            "B. Fir" = AMOUNT_TOTAL_4,
#            "Hemlock" = AMOUNT_TOTAL_5,
#            "Hickory" = AMOUNT_TOTAL_28,
#            "H. Maple" = AMOUNT_TOTAL_29,
#            "S. Maple" = AMOUNT_TOTAL_30,
#            "Red Oak Group" = AMOUNT_TOTAL_131,
#            "White Oak Group" = AMOUNT_TOTAL_133,
#            "Jack Pine" = AMOUNT_TOTAL_7,
#            "Lob./Shortleaf Pine" = AMOUNT_TOTAL_8,
#            "Slash/Longleaf Pine" = AMOUNT_TOTAL_10,
#            "Pond./Jeffrey Pine" = AMOUNT_TOTAL_11,
#            "Red Pine" = AMOUNT_TOTAL_12, 
#            "White Pine" = AMOUNT_TOTAL_14, 
#            "Other Pine" = AMOUNT_TOTAL_15,
#            "Yellow Poplar" = AMOUNT_TOTAL_39,
#            "Spruce" = AMOUNT_TOTAL_17,
#            "Sweetgum" = AMOUNT_TOTAL_35,
#            "Sycamore" = AMOUNT_TOTAL_36,
#            "Tamarack" = AMOUNT_TOTAL_6,
#            "Tupelo/Black Gum" = AMOUNT_TOTAL_37,
#            "Black Walnut" = AMOUNT_TOTAL_38,
#            "Other HWs" = AMOUNT_TOTAL_40,
#            "Mixed SWs" = AMOUNT_TOTAL_998,
#            "Mixed HWs" = AMOUNT_TOTAL_999,
#            "Other" = AMOUNT_TOTAL_887)
#    
# }
# ModelData <- ModelData %>%
#   RenameSpeciesVars()
# Predictions <- Predictions %>%
#   RenameSpeciesVars()

ModelData <- ModelData %>%
  left_join(TPO2021_ExtraVars, by = 'TPOID') %>%
  mutate(EQUIPMENT_NONE = ifelse(EQUIPMENT_NONE == "Mulch Grinder", NA, EQUIPMENT_NONE)) %>%
  distinct(TPOID, .keep_all = TRUE)

FactorVars <- c(45, 48:72)

Testing <- names(ModelData[,FactorVars])

ModelData <- ModelData %>%
  mutate(EQUIPMENT_CD_112 = ifelse(is.na(EQUIPMENT_CD_112), 0, EQUIPMENT_CD_112),
         EQUIPMENT_CD_121 = ifelse(is.na(EQUIPMENT_CD_121), 0, EQUIPMENT_CD_121),
         EQUIPMENT_CD_124 = ifelse(is.na(EQUIPMENT_CD_124), 0, EQUIPMENT_CD_124),
         EQUIPMENT_CD_122 = ifelse(is.na(EQUIPMENT_CD_122), 0, EQUIPMENT_CD_122),
         EQUIPMENT_CD_131 = ifelse(is.na(EQUIPMENT_CD_131), 0, EQUIPMENT_CD_131),
         EQUIPMENT_HEADSAW_NONE = ifelse(is.na(EQUIPMENT_HEADSAW_NONE), 0, EQUIPMENT_HEADSAW_NONE),
         EQUIPMENT_CD_211 = ifelse(is.na(EQUIPMENT_CD_211), 0, EQUIPMENT_CD_211),
         EQUIPMENT_CD_220 = ifelse(is.na(EQUIPMENT_CD_220), 0, EQUIPMENT_CD_220),
         EQUIPMENT_CD_306 = ifelse(is.na(EQUIPMENT_CD_306), 0, EQUIPMENT_CD_306),
         EQUIPMENT_CD_307 = ifelse(is.na(EQUIPMENT_CD_307), 0, EQUIPMENT_CD_307),
         EQUIPMENT_CD_302 = ifelse(is.na(EQUIPMENT_CD_302), 0, EQUIPMENT_CD_302),
         EQUIPMENT_CD_303 = ifelse(is.na(EQUIPMENT_CD_303), 0, EQUIPMENT_CD_303),
         EQUIPMENT_CD_304 = ifelse(is.na(EQUIPMENT_CD_304), 0, EQUIPMENT_CD_304),
         EQUIPMENT_CD_305 = ifelse(is.na(EQUIPMENT_CD_305), 0, EQUIPMENT_CD_305),
         EQUIPMENT_CD_308 = ifelse(is.na(EQUIPMENT_CD_308), 0, EQUIPMENT_CD_308),
         EQUIPMENT_CD_309 = ifelse(is.na(EQUIPMENT_CD_309), 0, EQUIPMENT_CD_309),
         EQUIPMENT_CD_312 = ifelse(is.na(EQUIPMENT_CD_312), 0, EQUIPMENT_CD_312),
         EQUIPMENT_CD_313 = ifelse(is.na(EQUIPMENT_CD_313), 0, EQUIPMENT_CD_313),
         EQUIPMENT_CD_314 = ifelse(is.na(EQUIPMENT_CD_314), 0, EQUIPMENT_CD_314),
         EQUIPMENT_CD_315 = ifelse(is.na(EQUIPMENT_CD_315), 0, EQUIPMENT_CD_315),
         EQUIPMENT_CD_316 = ifelse(is.na(EQUIPMENT_CD_316), 0, EQUIPMENT_CD_316),
         EQUIPMENT_CD_317 = ifelse(is.na(EQUIPMENT_CD_317), 0, EQUIPMENT_CD_317),
         EQUIPMENT_NONE = ifelse(is.na(EQUIPMENT_NONE), 0, EQUIPMENT_NONE))

ModelData[,FactorVars] <- lapply(ModelData[,FactorVars], factor)

rm(Issues, Issues2020, Dups, AddtoHundredFinal, Testing)


# Working in Extra Vars (Equipment/Portable/etc.) to Predictions
Predictions <- Predictions %>%
  left_join(TPO2021_ExtraVars, by = 'TPOID') %>%
  mutate(EQUIPMENT_NONE = ifelse(EQUIPMENT_NONE == "Mulch Grinder", NA, EQUIPMENT_NONE)) %>%
  distinct(TPOID, .keep_all = TRUE)

FactorVars <- c(45, 48:72)

Testing <- names(ModelData[,FactorVars])

Predictions <- Predictions %>%
  mutate(EQUIPMENT_CD_112 = ifelse(is.na(EQUIPMENT_CD_112), 0, EQUIPMENT_CD_112),
         EQUIPMENT_CD_121 = ifelse(is.na(EQUIPMENT_CD_121), 0, EQUIPMENT_CD_121),
         EQUIPMENT_CD_124 = ifelse(is.na(EQUIPMENT_CD_124), 0, EQUIPMENT_CD_124),
         EQUIPMENT_CD_122 = ifelse(is.na(EQUIPMENT_CD_122), 0, EQUIPMENT_CD_122),
         EQUIPMENT_CD_131 = ifelse(is.na(EQUIPMENT_CD_131), 0, EQUIPMENT_CD_131),
         EQUIPMENT_HEADSAW_NONE = ifelse(is.na(EQUIPMENT_HEADSAW_NONE), 0, EQUIPMENT_HEADSAW_NONE),
         EQUIPMENT_CD_211 = ifelse(is.na(EQUIPMENT_CD_211), 0, EQUIPMENT_CD_211),
         EQUIPMENT_CD_220 = ifelse(is.na(EQUIPMENT_CD_220), 0, EQUIPMENT_CD_220),
         EQUIPMENT_CD_306 = ifelse(is.na(EQUIPMENT_CD_306), 0, EQUIPMENT_CD_306),
         EQUIPMENT_CD_307 = ifelse(is.na(EQUIPMENT_CD_307), 0, EQUIPMENT_CD_307),
         EQUIPMENT_CD_302 = ifelse(is.na(EQUIPMENT_CD_302), 0, EQUIPMENT_CD_302),
         EQUIPMENT_CD_303 = ifelse(is.na(EQUIPMENT_CD_303), 0, EQUIPMENT_CD_303),
         EQUIPMENT_CD_304 = ifelse(is.na(EQUIPMENT_CD_304), 0, EQUIPMENT_CD_304),
         EQUIPMENT_CD_305 = ifelse(is.na(EQUIPMENT_CD_305), 0, EQUIPMENT_CD_305),
         EQUIPMENT_CD_308 = ifelse(is.na(EQUIPMENT_CD_308), 0, EQUIPMENT_CD_308),
         EQUIPMENT_CD_309 = ifelse(is.na(EQUIPMENT_CD_309), 0, EQUIPMENT_CD_309),
         EQUIPMENT_CD_312 = ifelse(is.na(EQUIPMENT_CD_312), 0, EQUIPMENT_CD_312),
         EQUIPMENT_CD_313 = ifelse(is.na(EQUIPMENT_CD_313), 0, EQUIPMENT_CD_313),
         EQUIPMENT_CD_314 = ifelse(is.na(EQUIPMENT_CD_314), 0, EQUIPMENT_CD_314),
         EQUIPMENT_CD_315 = ifelse(is.na(EQUIPMENT_CD_315), 0, EQUIPMENT_CD_315),
         EQUIPMENT_CD_316 = ifelse(is.na(EQUIPMENT_CD_316), 0, EQUIPMENT_CD_316),
         EQUIPMENT_CD_317 = ifelse(is.na(EQUIPMENT_CD_317), 0, EQUIPMENT_CD_317),
         EQUIPMENT_NONE = ifelse(is.na(EQUIPMENT_NONE), 0, EQUIPMENT_NONE))

Predictions[,FactorVars] <- lapply(Predictions[,FactorVars], factor)

write.xlsx(ModelData, "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\ModelData.xlsx")

h2o.init()
H2OData <- as.h2o(ModelData)

y <- readline(prompt = "Enter the field name for the species of interest: ")
x <- names(ModelData[,c(2,4:7,45:72)])


# Split data into train & validation sets
Splits <- h2o.splitFrame(H2OData, seed = 221, ratios = .75)
train <- Splits[[1]]
valid <- Splits[[2]]

# Run automatic machine learning
automl_model <- h2o.automl(x = x,
                           y = y,
                           training_frame = train,
                           max_models = 10000,
                           sort_metric = "RMSE",
                           stopping_metric = "RMSE",
                           stopping_tolerance = .05,
                           exclude_algos = c("DeepLearning", "StackedEnsemble"),
                           max_runtime_secs = 5000,
                           validation_frame = valid,
                           leaderboard_frame = valid,
                           preprocessing = list("target_encoding"),
                           nfolds = 5,
                           seed = 221)

# Find the Leader/Summary, and VI Scores
lb <- automl_model@leaderboard
leader <- automl_model@leader
summary(leader)
VI <- h2o.varimp(leader)
SpeciesVar <- eply::unquote(y)

VI %>%
  top_n(scaled_importance, n = 20) %>%
  mutate(variable = as.factor(variable),
         variable = fct_reorder(variable, scaled_importance)) %>%
  filter(scaled_importance > 0) %>%
  ggplot(aes(y = fct_reorder(variable, scaled_importance), x = scaled_importance)) +
  geom_col() +
  scale_x_continuous(breaks = c(seq(0,1,.1)), limits = c(0,1)) +
  labs(title = "h2o Regression - Scaled Variable Importance Scores", subtitle = "Scores calculated using test data.", y = "Scaled VI Score", x = "Variable") +
  theme_fivethirtyeight(base_size = 20, base_family = 'serif') +
  theme(axis.title = element_text(family = 'serif', size = 20))

# Test Data
pred <- h2o.predict(leader, valid) %>%
  as.data.frame() 

Results <- as.data.frame(valid) %>%
  select(TPOID:LOG_TOT_MCF, SpeciesVar) %>%
  cbind(pred) %>%
  mutate(AMOUNT_TOTAL_17 = ifelse(AMOUNT_TOTAL_17 < 0, 0, AMOUNT_TOTAL_17),
         Residuals = abs(predict - AMOUNT_TOTAL_17))
MeanRes <- mean(Results$Residuals, na.rm = TRUE)

ggplot(Results, aes(predict, AMOUNT_TOTAL_17)) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0,1,.1))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1,.1))) +
  #facet_wrap(~Region, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "Spruce Proportion - Test Data Residual Plot" , x = "Prediction", y = "Observation") +
  theme(legend.position="none") +
  geom_label(label = paste0("Mean Res. = ", as.character(round(MeanRes,4))), x = .2, y = 1)

# Aggregate Data
Predictions <- Predictions %>%
  filter(!MILL_TYPE_CD %in% c(30, 40, 117))

pred <- h2o.predict(leader, as.h2o(Predictions)) %>%
  as.data.frame() 

Results <- Predictions %>%
  select(TPOID:LOG_TOT_MCF, SpeciesVar) %>%
  cbind(pred) %>%
  mutate(AMOUNT_TOTAL_17 = ifelse(AMOUNT_TOTAL_17 < 0, 0, AMOUNT_TOTAL_17),
         Residuals = abs(predict - AMOUNT_TOTAL_17))

# Create df to output species-results
OutputResults <- Results %>%
  cbind(PredictionsJB) %>%
  mutate(AMOUNT_TOTAL_17 = if_else(is.na(AMOUNT_TOTAL_17), 0, AMOUNT_TOTAL_17),
         TOT_MCF = 10^LOG_TOT_MCF,
         AMOUNT_TOTAL_17 = AMOUNT_TOTAL_17*TOT_MCF) %>%
  select(-c(LOG_TOT_MCF, predict, Residuals))
OutputResults <- OutputResults[, c(1:6, 8:15,7, 16:17)]

MeanRes <- mean(Results$Residuals, na.rm = TRUE)

ggplot(Results, aes(predict, AMOUNT_TOTAL_17)) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0,1,.1))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1,.1))) +
  #facet_wrap(~Region, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "Spruce Proportion - Aggregate Residual Plot" , x = "Prediction", y = "Observation") +
  theme(legend.position="none") +
  geom_label(label = paste0("Mean Res. = ", as.character(round(MeanRes,4))), x = .2, y = 1)

Results <- Results %>%
  mutate(predict = if_else(predict < 0, 0, predict),
         predict = if_else(predict > 1, 1, predict),
         TOT_MCF = 10^LOG_TOT_MCF,
         PredVol = TOT_MCF * predict,
         ObsVol = TOT_MCF * AMOUNT_TOTAL_17)

ResultsSummary <- Results %>%
  group_by(MILL_STATE) %>%
  summarize(VolPred = sum(PredVol, na.rm = TRUE),
            VolObs = sum(ObsVol, na.rm = TRUE))

ResultsSummary <- ResultsSummary %>%
  mutate(MILL_STATE = as.factor(MILL_STATE),
         MILL_STATE = fct_reorder(MILL_STATE, -VolPred))

ggplot(Results, aes(MILL_STATE, predict, color = MILL_STATE)) +
  geom_jitter(width = .25, size = .8, alpha = .4) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1,.10))) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "Spruce Proportion - Aggregate Modeling Results", x = "State", y = "Spruce Prop. (Predicted)") +
  theme(legend.position="none") 

ggplot(OutputResults, aes(MILL_STATE, log10(AMOUNT_TOTAL_17), color = MILL_STATE)) +
  geom_jitter(width = .25, size = 1) +
  scale_y_continuous(limits = c(-2, 5), breaks = c(seq(-2,5,.5))) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "Spruce Lumber Volumes - Aggregate Modeling Results", x = "State", y = "Spruce Lumber Volume (Predicted) - Log10(MCF)") +
  theme(legend.position="none") 

OutputResults %>%
  group_by(MILL_STATE) %>%
  summarise(TotVol = sum(AMOUNT_TOTAL_17, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(MILL_STATE, -TotVol), TotVol)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 45000), breaks = c(seq(0, 45000,5000))) +
  #facet_wrap(~Region, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "Spruce Volumes by State - Aggregate Modeling Results", x = "State", y = "MCF (Predicted)") +
  theme(legend.position="none") 

OutputResults <- OutputResults %>%
  filter(AMOUNT_TOTAL_17 != 0) %>%
  mutate(MILL_LAT = as.numeric(MILL_LAT),
         MILL_LON = as.numeric(MILL_LON),
         MILL_ZIP_CD = ifelse(str_length(MILL_ZIP_CD) < 5, paste0("0", MILL_ZIP_CD), MILL_ZIP_CD),
         PHYSICAL_ZIP = ifelse(str_length(PHYSICAL_ZIP) < 5, paste0("0", PHYSICAL_ZIP), PHYSICAL_ZIP))

write_xlsx(OutputResults, path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\Species\\Spruce\\SprucePredicted.xlsx")
