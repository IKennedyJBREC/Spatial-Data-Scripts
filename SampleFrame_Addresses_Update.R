library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)

#Read in the Calls spreadsheets, coerce each sheet (State) to a df, and join the dfs to 
#Clog_Joined
path <- "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\NR_CALLS\\TPO2021Calls_UMass.xlsx"
sheets <- openxlsx::getSheetNames(path)
CLog <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)
CT <- as.data.frame(CLog[3])
KS <- as.data.frame(CLog[4])
IA <- as.data.frame(CLog[5])
IL <- as.data.frame(CLog[6])
IN <- as.data.frame(CLog[7])
MA <- as.data.frame(CLog[8])
ND <- as.data.frame(CLog[9])
NE <- as.data.frame(CLog[10])
NH <- as.data.frame(CLog[11])
NJ <- as.data.frame(CLog[12])
OH <- as.data.frame(CLog[13])
RI <- as.data.frame(CLog[14])
SD <- as.data.frame(CLog[15])
WI <- as.data.frame(CLog[16])
DE <- as.data.frame(CLog[17])
NY <- as.data.frame(CLog[18])
PA <- as.data.frame(CLog[19])
ME <- as.data.frame(CLog[20])
MO <- as.data.frame(CLog[21])
WV <- as.data.frame(CLog[22])
CYards <- as.data.frame(CLog[23])
MD <- as.data.frame(CLog[24])
CYards <- CYards %>%
  dplyr::rename(MILL_STATE = Exported_2021)
WI <- WI %>%
  select(-WI_DNR..Mills.that.will.be.handled.by.the.state.)
CLog_Joined <- CT %>% 
  rbind.data.frame(DE, IA, IL, IN, KS, MA, ME, MO, ND, NE, NH, NJ, NY, OH, PA, RI, SD, WI, WV, CYards)


#Remove Call log columns
CLog_Joined <- CLog_Joined %>%
  select(-c(Call.1_Date.Time:MILL_STATE))

#Read in OOB/Dismantled info from RLs
OOB_Dismantled <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\TPO_ReturnLogs_2021.xlsx", sheet = 4) 
#Select/rename a few columns
OOB_Dismantled <- OOB_Dismantled %>%
  select(TPOID:`IDLE/OOB/DISMANTLED`, MILL_STATUS_CD:Notes) %>%
  dplyr::rename(STATUS_INFO_SOURCE = Source.of.Status.Info, STATUS_NOTES_2021 = Notes) 

rm(CT, DE, IA, IL, IN, KS, MA, ME, MO, ND, NE, NH, NJ, NY, OH, PA, RI, SD, WI, WV, CYards, path, sheets)

#Read in the 2021 SampleFrame
SampleFrame <- read_xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\NR_CALLS\\SampleFrame_2021.xlsx")

#Create TPOID string
SampleFrame <- SampleFrame %>%
  mutate(MTC = ifelse(MILL_TYPE_CD %in% c(100,111), 110, MILL_TYPE_CD),
         TPOID = paste0(MILL_STATECD, "-2021-", MILL_NBR, "-", MTC)) 

#Change mills with 'NA' MILL_NBR to have an 'NA' TPOID, remove MTC column
SampleFrame <- SampleFrame %>%
  mutate(TPOID = ifelse(str_detect(TPOID, "NA"), NA, TPOID)) %>%
  select(-MTC)

#Filter out mills with non-matching TPOIDs
ProblemMills <- CLog_Joined %>%
  mutate(UNIQUE_ID = if_else(is.na(UNIQUE_ID_CORRECTED), UNIQUE_ID, UNIQUE_ID_CORRECTED)) %>%
  dplyr::rename(TPOID = UNIQUE_ID) %>%
  filter(MILL_NAME %in% c("RAM Forest Products Inc.", "Endless Wood Sawmill", "A. Swarey & Sons Hardwoods, LLC", "AMERICAN STAVE CO", "Rose Lumber"))

CLog_Joined <- CLog_Joined %>%
  mutate(UNIQUE_ID = if_else(is.na(UNIQUE_ID_CORRECTED), UNIQUE_ID, UNIQUE_ID_CORRECTED)) %>%
  dplyr::rename(TPOID = UNIQUE_ID) %>%
  filter(!MILL_NAME %in% c("RAM Forest Products Inc.", "Endless Wood Sawmill", "A. Swarey & Sons Hardwoods, LLC", "AMERICAN STAVE CO", "Rose Lumber"))

#Join the Call Logs to the OOB/Dismantled info
CLog_Joined <- CLog_Joined %>%
  left_join(OOB_Dismantled, by = "TPOID")

#Join all info to the SampleFrame
SampleFrame <- SampleFrame %>%
  left_join(CLog_Joined, by = "TPOID")

#Select columns of interest/rename other columns
SampleFrame <- SampleFrame %>%
  select(-c(MILL_TYPE_CD.y, Return.Date:Comments, Notes))

SampleFrame <- SampleFrame %>%
  dplyr::rename(MILL_NAME = MILL_NAME.x, MILL_STATUS_CD = MILL_STATUS_CD.x, STATUS_CD_2021 = MILL_STATUS_CD.y, WOOD_PROCESSED_CD_2021 = WOOD_PROCESSED_CD, MILL_TYPE_CD = MILL_TYPE_CD.x, MILL_NAME_2021 = MILL_NAME.y, MILL_MAILING_ADDRESS = MAILING_ADDRESS,
         MILL_MAILING_CITY = MAILING_CITY, MILL_MAILING_STATE = MAILING_STATE, MILL_MAILING_ZIP = MAILING_ZIP, IDLE_OOB_DISMANTLED = `IDLE/OOB/DISMANTLED`)


##This section writes up missed OOB/DISMANTLED mills from 2020
# OOB_Dismantled_2020 <- read_csv("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2020 TPO Survey\\TPO2020_Returns_Compiled_0203.csv") 
#   
# OOB_Dismantled_2020 <- OOB_Dismantled_2020 %>%
#   filter(!is.na(`"Mill Status" Notes`)) 
# 
# OOB_Dismantled_2020 <- OOB_Dismantled_2020[, c(4, 7:10, 12:13)]
# OOB_Dismantled_2020 <- OOB_Dismantled_2020 %>%
#   rename("IDLE_OOB_DISMANTLED" = `"Mill Status" Notes`, "STATUS_INFO_SOURCE_2020"  = 'Comments', 'TPOID' = 'UNIQUE_ID', 'MILL_NAME' = 'FULL_NAME',
#         'MILL_MAILING_ADDRESS_2020' ='DLVRYADDRS', 'MILL_MAILING_CITY_2020' ='CITY', 'MILL_STATE' = 'STATE')
# 
# 
# OOB_Dismantled_2020 <- OOB_Dismantled_2020 %>%
#   mutate(IDLE_OOB_DISMANTLED = ifelse(IDLE_OOB_DISMANTLED == "C/OOB", "OOB", IDLE_OOB_DISMANTLED)) %>%
#   filter(IDLE_OOB_DISMANTLED %in% c("IDLE", "OOB", "DISMANTLED"))
# 
# OOB_Dismantled_2020 <- OOB_Dismantled_2020 %>% 
#   mutate(MILL_NBR = case_when(str_length(TPOID) == 12 ~ str_extract(string = TPOID, pattern = "\\d+$"),
#                               str_length(TPOID) == 15 ~ str_extract(string = TPOID, pattern = "0\\d{3}")),
#          MILL_NBR = str_remove(MILL_NBR, "^0+")) %>%
#   filter(MILL_NAME %in% c("ALDEN BRODMERKLE",
#                           "DAVID ANDREWS",
#                           "ROBERT KORPI",
#                           "S YODER LUMBER CO"))
#write.xlsx(OOB_Dismantled_2020, file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\SURVEY OUTPUT\\NR_CALLS\\RScripts\\OOB_DISMANTLED_2020.xlsx")

  
# Test <- SampleFrame %>%
#   filter(toupper(MILL_NAME) != toupper(MILL_NAME_2021)) %>%
#   select(c(MILL_NAME:MILL_CITY, MILL_NAME_2021:MILL_MAILING_CITY))

#Output the updated SampleFrame...Need to update 'ProblemMills' after uploading to OneDrive
write.xlsx(SampleFrame, file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 Mill Data (SY2022)\\SURVEY OUTPUT\\NR_CALLS\\SampleFrame_2021_Updated.xlsx")


# Test <- SampleFrame %>%
#   filter(!is.na(MILL_NAME.y)) %>%
#   filter(toupper(MILL_NAME.x) != toupper(MILL_NAME.y)) %>%
#   select(MILL_NAME.x, MILL_STATECD, MILL_NAME.y, MILL_ZIP_CODE, TPOID)



##Code from here out only for 'StatusCheck Doc'
# RL <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\TPO_ReturnLogs_2021.xlsx", sheet = 2) %>%
#   filter(!is.na(`Return Date`))
# 
# RL <- RL$MILL_FORM_ID
# 
# SampleFrame <- SampleFrame %>%
#   filter(!(TPOID %in% RL)) %>%
#   mutate(Status = NA,
#          Evidence = NA,
#          LogsPilesSeen = NA,
#          'Checked?' = NA) %>%
#   select(-c(SURVEY_YEAR, MILL_NBR, HS, STATUS_INFO_SOURCE:STATUS_NOTES_2021, MILL_STATUS_CD:note, UNIQUE_ID_CORRECTED))
# 
# SampleFrame <- SampleFrame[,c(28:31, 1:10, 12:27, 11)]
# 
# State_Code_Conversion <- function(Data){
#   Data %>%
#     mutate(MILL_STATE = case_when(MILL_STATECD == 9 ~ "CT",
#                                   MILL_STATECD == 10 ~ "DE",
#                                   MILL_STATECD == 17 ~ "IL", 
#                                   MILL_STATECD == 18 ~ "IN",
#                                   MILL_STATECD == 19 ~ "IA",
#                                   MILL_STATECD == 20 ~ "KS",
#                                   MILL_STATECD == 23 ~ "ME",
#                                   MILL_STATECD == 24 ~ "MD",
#                                   MILL_STATECD == 25 ~ "MA",
#                                   MILL_STATECD == 26 ~ "MI",
#                                   MILL_STATECD == 27 ~ "MN",
#                                   MILL_STATECD == 29 ~ "MO",
#                                   MILL_STATECD == 31 ~ "NE",
#                                   MILL_STATECD == 33 ~ "NH",
#                                   MILL_STATECD == 34 ~ "NJ",
#                                   MILL_STATECD == 36 ~ "NY",
#                                   MILL_STATECD == 38 ~ "ND",
#                                   MILL_STATECD == 39 ~ "OH",
#                                   MILL_STATECD == 42 ~ "PA",
#                                   MILL_STATECD == 44 ~ "RI",
#                                   MILL_STATECD == 46 ~ "SD",
#                                   MILL_STATECD == 50 ~ "VT",
#                                   MILL_STATECD == 54 ~ "WV",
#                                   MILL_STATECD == 55 ~ "WI"))
# }
# 
# SampleFrame <- SampleFrame %>%
#   State_Code_Conversion() %>%
#   select(-MILL_STATECD)
# 
# SampleFrame <- SampleFrame[, c(1:7,31,8:30)]
# 
# write.xlsx(SampleFrame, file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\SAMPLE RESEARCH\\SampleFrame_2021_StatusCheck.xlsx")

# StatusCheck <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\SAMPLE RESEARCH\\SampleFrame_2021_StatusCheck.xlsx")
# Phone_Email_IDs <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\SURVEY OUTPUT\\NR_CALLS\\RScripts\\Phone_Email_Responses.xlsx")
# 
# Phone_Email_IDs <- Phone_Email_IDs %>%
#   mutate(TPOID = str_split_fixed(TPOID, pattern = ".pdf", n = 2)[,1])
# 
# StatusCheck <- StatusCheck %>%
#   filter(!(TPOID %in% Phone_Email_IDs$TPOID))
# 
# write.xlsx(StatusCheck, file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\SAMPLE RESEARCH\\SampleFrame_2021_StatusCheckTest.xlsx")

Test <- read_xlsx(path = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\SURVEY OUTPUT\\NR_CALLS\\SampleFrame_2021_Updated.xlsx")
