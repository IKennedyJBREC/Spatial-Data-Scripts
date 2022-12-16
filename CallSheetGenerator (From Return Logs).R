library(tidyverse)
library(readxl)
library(magrittr)
library(ggthemes)
library(ggpubr)
library(writexl)
library(openxlsx)

# THIS SCRIPT OUTPUTS THE 'UG CALLS' SPREADSHEET FROM THE RETURN LOGS SPREADSHEET.

# MAKE SURE TO CHECK WHICH STATES ARE NOT FFRC-IMPLEMENTED...THIS SCRIPT RELIES ON THE 'ADMINISTRATOR' FIELD (RETURN LOG) BEING ACCURATE!

# As of 10.26.22: MI, MN, VT, 1/2 of MO, & ~ 1/2 of WI are non-FFRC administered

# THE SCRIPT OUTPUTs CALL SHEETS FOR ALL OF THESE STATES, THOUGH THE SHEET(S) ARE BLANK IF NON-FFRC ADMINISTERED.


# THE FIRST 2 LINES MUST BE RUN INDEPENDETLY TO ASK FOR FILE PATHS
RLPath <- readline(prompt = "Enter File Path to Return Log Here (No quotation marks!): ")
OutputPath <- readline(prompt = "Enter File Path to the Output Destination for the 'Calls' Spreadsheet' (No quotation marks!): ")


# Read in the 'Return Log' and save as 'RL'. Make sure sheet is correct!
RL <- read_xlsx(path = RLPath, sheet = 2)

#Filter out the CYards
CYards <- read_xlsx(path = RLPath, sheet = 5)


RL <- RL %>%
  #Filter out partner/independent states
  ## CHECK TO MAKE SURE ALL STATES NOT LISTED WITH AN 'ADMINISTRATOR' AS 'FFRC' ARE CORRECT!!!
  filter(Administrator == "FFRC") %>%
  mutate(`Return Date` = as.character(`Return Date`)) %>%
  #Sort and group by State
  arrange(MILL_STATECD) %>%
  group_by(MILL_STATECD) %>%
  #Arrange alphabetically within each state
  arrange(FULL_NAME, .by_group = TRUE) %>%
  #Drop irrelevant columns
  select(-c(Administrator:`Source of "Mill Status" Notes`)) %>%
  #Rename a bunch of columns for the calls spreadsheet and ungroup
  rename("MILL_NAME" = "FULL_NAME",
         # "MAILING_ADDRESS" = "MILL_STREET1",
         # "MAILING_CITY" = "CITY",
         # "MAILING_STATE" = "STATE",
         # "MAILING_ZIP" = "ZIP",
         "MILL_PHYSICAL_ADDRESS" = "MILL_PHYSICAL_STREET",
         "MILL_PHYSICAL_ZIP" = "MILL_PHYSICAL_ZIP_CODE",
         "COMPANY_ZIP" = "COMPANY_ZIP_CODE") %>%
         # "MILL_EMAIL" = "COMPANY_EMAIL") %>%
  ungroup() %>%
  select(-UNIQUE_ID_CORRECTED)

CYards <- CYards %>%
  mutate(`Return Date` = as.character(`Return Date`)) %>%
  #Sort and group by State
  arrange(MILL_STATECD) %>%
  group_by(MILL_STATECD) %>%
  #Arrange alphabetically within each state
  arrange(FULL_NAME, .by_group = TRUE) %>%
  #Drop irrelevant columns
  select(-c(Administrator:`Source of "Mill Status" Notes`)) %>%
  #Rename a bunch of columns for the calls spreadsheet and ungroup
  rename("MILL_NAME" = "FULL_NAME",
         # "MAILING_ADDRESS" = "MILL_STREET_1",
         # "MAILING_CITY" = "CITY",
         # "MAILING_STATE" = "STATE",
         # "MAILING_ZIP" = "ZIP",
         "MILL_PHYSICAL_ADDRESS" = "MILL_PHYSICAL_STREET",
         "MILL_PHYSICAL_ZIP" = "MILL_PHYSICAL_ZIP_CODE",
         "COMPANY_ZIP" = "COMPANY_ZIP_CODE") %>%
         # "MILL_EMAIL" = "COMPANY_EMAIL") %>%
  ungroup() %>%
  select(-UNIQUE_ID_CORRECTED)


ColOrder <- c(4, 1, 2, 5:9, 18:21, 10, 13:15, 17, 16, 22:26,27:29, 11:12)

RL <- RL[, ColOrder]
CYards <- CYards[, ColOrder]

#Concatenate fields for Contact's Phone # and Extension and drop the 'EXT' column...Not needed for 2022
# RL <- RL %>%
#   mutate(CONTACT_PHONE = as.character(CONTACT_PHONE),
#          CONTACT_PHONE_EXT = as.character(CONTACT_PHONE_EXT)) %>%
#   mutate(CONTACT_PHONE = ifelse(is.na(CONTACT_PHONE_EXT), CONTACT_PHONE, str_c(CONTACT_PHONE, CONTACT_PHONE_EXT, sep = " x"))) %>%
#   select(-CONTACT_PHONE_EXT)
# 
# CYards <- CYards %>%
#   mutate(CONTACT_PHONE = as.character(CONTACT_PHONE),
#          CONTACT_PHONE_EXT = as.character(CONTACT_PHONE_EXT)) %>%
#   mutate(CONTACT_PHONE = ifelse(is.na(CONTACT_PHONE_EXT), CONTACT_PHONE, str_c(CONTACT_PHONE, CONTACT_PHONE_EXT, sep = " x"))) %>%
#   select(-CONTACT_PHONE_EXT)

RL <- RL %>%
  mutate(MILL_LAT = as.numeric(MILL_LAT),
         MILL_LON = as.numeric(MILL_LON))

CYards <- CYards %>%
  mutate(MILL_LAT = as.numeric(MILL_LAT),
         MILL_LON = as.numeric(MILL_LON))

#Add columns for call times/notes
RL <- RL %>%
  cbind("NotesForMN" = NA, 
        "Call 1 Date/Time" = NA,
        "Call 1 Notes" = NA,
        "Call 2 Date/Time" = NA,
        "Call 2 Notes" = NA,
        "Call 3 Date/Time" = NA,
        "Call 3 Notes" = NA,
        "Completed?" = NA)

CYards <- CYards %>%
  cbind("NotesForMN" = NA, 
        "Call 1 Date/Time" = NA,
        "Call 1 Notes" = NA,
        "Call 2 Date/Time" = NA,
        "Call 2 Notes" = NA,
        "Call 3 Date/Time" = NA,
        "Call 3 Notes" = NA,
        "Completed?" = NA)

# #Function to convert state codes
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
# #Convert the State Codes to Abbreviations and drop the code
# RL <- State_Code_Conversion(RL)
# RL <- RL %>%
#   select(-MILL_STATECD)
# 
# CYards <- State_Code_Conversion(CYards)
# CYards <- CYards %>%
#   select(-MILL_STATECD)

#Enter 'C' in 'Completed?' column for mail responders
RL <- RL %>%
  mutate(`Completed?` = ifelse(!is.na(`Return Date`), "C", NA))



# Create data frames for each state and CYards
CT <- RL %>%
  filter(MAILING_STATE == "CT")
DE <- RL %>%
  filter(MAILING_STATE == "DE")
IL <- RL %>%
  filter(MAILING_STATE == "IL")
IN <- RL %>%
  filter(MAILING_STATE == "IN")
IA <- RL %>%
  filter(MAILING_STATE == "IA")
KS <- RL %>%
  filter(MAILING_STATE == "KS")
MA <- RL %>%
  filter(MAILING_STATE == "MA")
ME <- RL %>%
  filter(MAILING_STATE == "ME")
MD <- RL %>%
  filter(MAILING_STATE == "MD")
MO <- RL %>%
  filter(MAILING_STATE == "MO")
NE <- RL %>%
  filter(MAILING_STATE == "NE")
NH <- RL %>%
  filter(MAILING_STATE == "NH")
NJ <- RL %>%
  filter(MAILING_STATE == "NJ")
NY <- RL %>%
  filter(MAILING_STATE == "NY")
ND <- RL %>%
  filter(MAILING_STATE == "ND")
OH <- RL %>%
  filter(MAILING_STATE == "OH")
PA <- RL %>%
  filter(MAILING_STATE == "PA")
RI <- RL %>%
  filter(MAILING_STATE == "RI")
SD <- RL %>%
  filter(MAILING_STATE == "SD")
WI <- RL %>%
  filter(MAILING_STATE == "WI")
WV <- RL %>%
  filter(MAILING_STATE == "WV")
MI <- RL %>%
  filter(MAILING_STATE == "MI")
VT <- RL %>%
  filter(MAILING_STATE == "VT")
MN <- RL %>%
  filter(MAILING_STATE == "MN")


CallBacks <- data.frame('TPOID' = NA, 'CallBack Date/Time' = NA) 
UGLog <- data.frame('Name' = NA, 'Date' = NA, `Start_State&TPOID` = NA, `End_State&TPOID` = NA) 




#define sheet names for each data frame
dataset_names <- list('UGLog' = UGLog,
                      'SpecificCallBackRequests' = CallBacks,
                      'CT' = CT, 
                      'DE' = DE,
                      'IA' = IA,
                      'IL' = IL,
                      'IN' = IN,
                      'KS' = KS,
                      'MA' = MA,
                      'MD' = MD,
                      'ME' = ME,
                      'MO' = MO,
                      'NE' = NE,
                      'NH' = NH,
                      'NJ' = NJ,
                      'NY' = NY,
                      'ND' = ND,
                      'OH' = OH,
                      'PA' = PA,
                      'RI' = RI,
                      'SD' = SD,
                      'WI' = WI,
                      'WV' = WV,
                      'CYards' = CYards,
                      'MI' = MI,
                      'MN' = MN,
                      'VT' = VT)


#Export the 'Calls' Spreadsheet...You'll need to provide the file path.
write.xlsx(dataset_names, file = OutputPath)
