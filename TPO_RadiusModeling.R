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
library(ggthemes)

rmse <- function (y_pred, y_true) {
  RMSE <- sqrt(mean((y_true - y_pred)^2))
  return(RMSE)
}

# Radii from 2020 already worked in!
ModelData <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\ModelData.xlsx")
TPO2021 <- read_csv(file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\TELEFORM CSV\\2021_TPO_Mail_Phone_1.19.23.CSV") 

TylerMills <- c('09-2021-11-10', '09-2021-18-10', '09-2021-23-10', '09-2021-36-10',
                '17-2021-414-10', '18-2021-218-10','19-2021-55-10', '31-2021-56-110', 
                '33-2021-22-10', '33-2021-28-10', '33-2021-41-10', '34-2021-4-10',
                '34-2021-7-10', '34-2021-13-10', '34-2021-27-10', '38-2021-46-10', 
                '39-2021-151-10', '39-2021-163-10', '42-2021-139-10', '44-2021-6-10', 
                '46-2021-4-60', '44-2021-1-10', '46-2021-29-90')

TPO2021 <- TPO2021 %>%
  filter(!TPOID %in% TylerMills)

rm(TylerMills)

TPO2021_Dup <- TPO2021

TPO2021 <- TPO2021 %>%
  select(TPOID, PROCUREMENT_RADIUS) %>%
  filter(!is.na(PROCUREMENT_RADIUS)) %>%
  distinct(TPOID, .keep_all = TRUE)

ModelData <- ModelData %>%
  left_join(TPO2021, by = 'TPOID') %>%
  filter(!is.na(PROCUREMENT_RADIUS)) %>%
  select(-c(AMOUNT_TOTAL_19:AMOUNT_TOTAL_887))

ModelData[,c(2:4, 8, 11:35)] <- lapply(ModelData[,c(2:4, 8, 11:35)], factor)

ModelData <- ModelData %>%
  filter(!(MILL_STATE == "MN" & PROCUREMENT_RADIUS > 100)) %>%
  filter(PROCUREMENT_RADIUS < 200) %>%
  mutate(SqRt_ProRad = sqrt(PROCUREMENT_RADIUS)) %>%
  select(-PROCUREMENT_RADIUS)

path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\TPOSamplePredicted.xlsx"
Predictions <- read_xlsx(path = path, sheet = 1) %>%
  rbind(read_xlsx(path = path, sheet = 2)) %>%
  rbind(read_xlsx(path = path, sheet = 3)) %>%
  rbind(read_xlsx(path = path, sheet = 4)) %>%
  rbind(read_xlsx(path = path, sheet = 5)) %>%
  rbind(read_xlsx(path = path, sheet = 6)) %>%
  rbind(read_xlsx(path = path, sheet = 7)) %>%
  rbind(read_xlsx(path = path, sheet = 8)) %>%
  rbind(read_xlsx(path = path, sheet = 9)) 

AllMills <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MillLocations.xlsx")


Predictions <- Predictions %>%
  left_join(AllMills, by = 'TPOID') %>%
  mutate(MILL_LAT.x = as.numeric(MILL_LAT.x), 
         MILL_LON.x = as.numeric(MILL_LON.x)) %>%
  dplyr::rename(MILL_LAT = MILL_LAT.x, MILL_LON = MILL_LON.x) %>%
  mutate(MILL_LAT = if_else(is.na(MILL_LAT), MILL_LAT.y, MILL_LAT),
         MILL_LON = if_else(is.na(MILL_LON), MILL_LON.y, MILL_LON)) %>%
  select(-c(MILL_LAT.y, MILL_LON.y))

rm(AllMills)

Predictions <- Predictions %>%
  mutate(MILL_STATE = ifelse(MILL_STATE == "45690", "OH", MILL_STATE),
         TPOID = ifelse(MILL_STATE == "CT", paste0("0", TPOID), TPOID)) %>%
  mutate(TOT_MCF = TOT_MCF^2)

Predictions <- Predictions[-323,]

Predictions <- Predictions %>%
   mutate(Region = dplyr::if_else(TPOID == "55-2021-493-10", "Plains_WI", Region))

Sample2021 <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\NR_CALLS\\SampleFrame_2021_Updated.xlsx") %>%
  mutate(TPOID = ifelse(MILL_STATECD == 9, paste0("0", TPOID), TPOID)) %>%
  select(MILL_NAME, MILL_TYPE_CD, TPOID, MILL_STATECD)

Matches <- Predictions %>%
  distinct(TPOID, .keep_all = TRUE) %>%
  filter(TPOID %in% Sample2021$TPOID & !is.na(TPOID))

Sample2021 <- Sample2021 %>%
  left_join(Matches, by = "TPOID")

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

Sample2021 <- Sample2021 %>%
  dplyr::rename(MILL_NAME = MILL_NAME.x, MILL_TYPE_CD = MILL_TYPE_CD.x) %>%
  State_Code_Conversion() %>%
  select(-c(MILL_TYPE_CD.y, Pro_Rad, MILL_NAME.y, MILL_STATECD)) 

#2021 sample is ready to use for modeling. Need to find a way to work in non-sample based info
# That info includes equipment, portable?, etc.
Sample2021 <- Sample2021[,c(1, 4:13, 2, 3, 14, 15)] 

ID_NA <- Sample2021 %>%
  filter(is.na(TPOID))

Sample2021 <- Sample2021 %>%
  filter(!is.na(TPOID)) %>%
  distinct(TPOID, .keep_all = TRUE)

Sample2021 <- Sample2021 %>%
  rbind(ID_NA)

TPO2021_Dup <- TPO2021_Dup %>%
  select(TPOID, PORTABLE_CD, NBR_EMPLOYEES_ALL, URBAN_WOOD_PCT, EXPORTED_CD, EQUIPMENT_CD_112:EQUIPMENT_CD_307, EQUIPMENT_CD_302:EQUIPMENT_CD_317, EQUIPMENT_NONE, PROCUREMENT_RADIUS) %>%
  mutate(URBAN_WOOD_PCT = ifelse(is.na(URBAN_WOOD_PCT), 0, URBAN_WOOD_PCT),
         SqRt_ProRad = sqrt(PROCUREMENT_RADIUS)) %>%
  select(-PROCUREMENT_RADIUS)

TPO2021_Dup <- TPO2021_Dup %>%
  mutate(EQUIPMENT_NONE = ifelse(EQUIPMENT_NONE == "Mulch Grinder", NA, EQUIPMENT_NONE)) %>%
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

TPO2021_Dup <- TPO2021_Dup %>%
  distinct(TPOID, .keep_all = TRUE)

Sample2021 <- Sample2021 %>%
  left_join(TPO2021_Dup, by = 'TPOID') 

Sample2021 <- Sample2021 %>%
  mutate(MILL_TYPE_CD = ifelse(MILL_TYPE_CD %in% c(100,111), 110, MILL_TYPE_CD))

FactorVars <- c(5, 12, 14, 16, 19:43)

Sample2021[,FactorVars] <- lapply(Sample2021[,FactorVars], factor)

OOB_Dismantled <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\TPO_ReturnLogs_2021.xlsx", sheet = 3)
  
OOB_Dismantled <- OOB_Dismantled %>%
  select(TPOID, RESPONSE) %>%
  filter(RESPONSE %in% c("OOB", "DISMANTLED"))



# Sample2021 is all set, work in WV, and remove Pulp/Composite (if warranted)
Sample2021 <- Sample2021 %>%
  left_join(OOB_Dismantled, by = "TPOID")

Sample2021 <- Sample2021 %>%
  filter(!RESPONSE %in% c("OOB", "DISMANTLED")) %>%
  select(-RESPONSE)

Sample2021 <- Sample2021 %>%
  mutate(TOT_MCF = log10(TOT_MCF)) %>%
  rename(LOG_TOT_MCF = TOT_MCF)

ModelData <- ModelData %>%
  mutate(Region = as.character(Region),
         Region = dplyr::if_else(TPOID == "55-2021-493-10", "Plains_WI", Region))
ModelData$Region <- factor(ModelData$Region)

# UnmatchedMills <- read.csv("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\UnmatchedMills.csv")
# UnmatchedMills <- UnmatchedMills$TPOID

# UnmatchedSample <- Sample2021 %>%
#   mutate(Unmatched = ifelse(TPOID %in% UnmatchedMills, 1, 0)) %>%
#   filter(Unmatched == 1) 

# write.xlsx(UnmatchedSample, "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\Ian's TPO Docs\\MLHeatMap\\UnmatchedSample.xlsx")

h2o.init()
H2OData <- as.h2o(ModelData)

y <- "SqRt_ProRad"
x <- names(ModelData[,c(2:35)])

# Split data into train & validation sets
Splits <- h2o.splitFrame(H2OData, seed = 45, ratios = .75)
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
                           max_runtime_secs = 10000,
                           validation_frame = valid,
                           leaderboard_frame = valid,
                           preprocessing = list("target_encoding"),
                           nfolds = 3,
                           seed = 45)

lb <- automl_model@leaderboard
leader <- automl_model@leader
lb
summary(leader)

SpeciesVar <- eply::unquote(y)

VI <- h2o.varimp(leader)

VI <- VI %>%
  top_n(scaled_importance, n = 20) %>%
  mutate(variable = as.factor(variable),
         variable = fct_reorder(variable, scaled_importance)) %>%
  filter(scaled_importance > 0)
 
VI %>% 
ggplot(aes(y = fct_reorder(variable, scaled_importance), x = scaled_importance)) +
  geom_col() +
  scale_x_continuous(breaks = c(seq(0,1,.1)), limits = c(0,1)) +
  labs(title = "h2o Regression - Scaled Variable Importance Scores", subtitle = "Scores calculated using test data.", y = "Scaled VI Score", x = "Variable") +
  theme_fivethirtyeight(base_size = 20, base_family = 'serif') +
  theme(axis.title = element_text(family = 'serif', size = 20))


testpreds <- h2o.predict(leader, valid) %>%
  as.data.frame()

TestData <- as.data.frame(valid) %>%
  cbind(testpreds) %>%
  mutate(Res = abs(predict - SqRt_ProRad)) 
mean(TestData$Res)

test <- TestData %>%
  group_by(Region) %>%
  summarise_at(vars(Res), list(MeanRes = mean))

test2 <- TestData %>%
  dplyr::group_by(Region) %>%
  dplyr::summarize(rmse = rmse(predict, SqRt_ProRad))

test <- test %>%
  left_join(test2, by = 'Region') %>%
  mutate(MeanRes = round(MeanRes, 2),
         rmse = round(rmse, 2))
rm(test2)

NewPredictions <- h2o.predict(leader, as.h2o(Sample2021)) %>%
  as.data.frame() %>%
  cbind(Sample2021) %>%
  mutate(SqRt_ProRad = if_else(is.na(SqRt_ProRad), predict, SqRt_ProRad)) 

FinalModelData <- NewPredictions %>%
  select(MILL_NAME:LOG_TOT_MCF, SqRt_ProRad) %>%
  mutate(LOG_TOT_MCF = 10^LOG_TOT_MCF,
         SqRt_ProRad = SqRt_ProRad^2,
         MILL_STATE = as.character(MILL_STATE),
         MILL_TYPE_CD = as.character(MILL_TYPE_CD),
         Region = as.character(Region)) %>%
  rename(TOT_MCF = LOG_TOT_MCF, Pro_Rad = SqRt_ProRad, MILL_CITY.x = MILL_CITY,
         MTC = MILL_TYPE_CD, PHYSICAL_STREET = PHYSICAL_ADDRESS)

TestData %>%
  ggplot(aes(predict, SqRt_ProRad)) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 9), breaks = c(seq(0,9,1))) +
  scale_y_continuous(limits = c(0, 13), breaks = c(seq(0,13,1))) +
  facet_wrap(~Region, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "h2o Regression - Test Data Residual Plot", 
       subtitle = "SqRt(Procurement Radius)" , x = "Prediction", y = "Observation") +
  geom_text(x = 3.75, y = 10, aes(label = MeanRes, size = 1), data = test) +
  annotate("text", x = 1.5, y = 10, label = "Mean Res: ") +
  geom_text(x = 3.75, y = 13, aes(label = rmse, size = 1), data = test) +
  annotate("text", x = 2., y = 13, label = "RMSE: ") +
  theme(legend.position="none") +
  ggpubr::stat_regline_equation(label.x = 5.75, label.y = 13, aes(label = ..rr.label..))
  
Predictions <- h2o.predict(leader, H2OData) %>%
  as.data.frame()

Predictions <- ModelData %>%
  cbind(Predictions) %>%
  mutate(Res = abs(predict - SqRt_ProRad)) 
mean(Predictions$Res)

test <- Predictions %>%
  group_by(Region) %>%
  summarise_at(vars(Res), list(MeanRes = mean))

test2 <- Predictions %>%
  dplyr::group_by(Region) %>%
  dplyr::summarize(rmse = rmse(predict, SqRt_ProRad))

test <- test %>%
  left_join(test2, by = 'Region') %>%
  mutate(MeanRes = round(MeanRes, 2),
         rmse = round(rmse, 2))
rm(test2)

Predictions %>%
  ggplot(aes(predict, SqRt_ProRad)) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 13), breaks = c(seq(0,13,1))) +
  scale_y_continuous(limits = c(0, 13), breaks = c(seq(0,13,1))) +
  facet_wrap(~Region, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "h2o Regression - Aggregate Residual Plot", 
       subtitle = "SqRt(Procurement Radius)" , x = "Prediction", y = "Observation") +
  geom_text(x = 3.75, y = 10, aes(label = MeanRes, size = 1), data = test) +
  annotate("text", x = 1.5, y = 10, label = "Mean Res: ") +
  geom_text(x = 3.75, y = 13, aes(label = rmse, size = 1), data = test) +
  annotate("text", x = 2., y = 13, label = "RMSE: ") +
  theme(legend.position="none") +
  ggpubr::stat_regline_equation(label.x = 5.75, label.y = 13, aes(label = ..rr.label..))


Predictions %>%
  ggplot(aes(predict, SqRt_ProRad)) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 13), breaks = c(seq(0,13,1))) +
  scale_y_continuous(limits = c(0, 13.5), breaks = c(seq(0,13.5,1))) +
  theme_fivethirtyeight(base_size = 30, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "h2o Regression - Aggregate Residual Plot", 
       subtitle = "SqRt(Procurement Radius)" , x = "Prediction", y = "Observation") +
  theme(legend.position="none") +
  ggpubr::stat_regline_equation(label.x = 5.75, label.y = 13, aes(label = ..rr.label..))

# PredsOld <- read.xlsx('C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Downloads\\TPO_Test\\TPOSamplePredicted.xlsx')
write.xlsx(FinalModelData, 'C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Downloads\\TPO_Test\\TPOSamplePredicted_1.19.23.xlsx')
