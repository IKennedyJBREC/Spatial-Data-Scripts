library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)

# THIS SCRIPT OUTPUTS THE RETURN LOG SPREADSHEET FROM THE ANNUAL SAMPLE DRAW
##Need to work in Contact/Physical Addresses!!!

# THE FIRST 4 LINES MUST BE RUN INDEPENDETLY TO ASK FOR FILE PATHS (No quotation marks!)
SamplePath <- readline(prompt = "Enter File Path to Original Sample Here: ")
RLPath <- readline(prompt = "Enter File Path to Return Log Here: ")

Sample <- read_xlsx(path = SamplePath)

# Coearce 'Other' mill types and create the Unique ID (TPOID)
Sample <- Sample %>%
  filter(sample == 1) %>%
  mutate(MILL_TYPE_CD = ifelse(MILL_TYPE_CD %in% c(111,100), 110, MILL_TYPE_CD)) %>%
  mutate(UNIQUE_ID = paste0(MILL_STATECD, "-2021-", MILL_NBR,"-", MILL_TYPE_CD)) %>%
  mutate(UNIQUE_ID = ifelse(MILL_STATECD == 9, paste0("0", UNIQUE_ID), UNIQUE_ID))

# Set Administrators for all states. Return Log will need to be amended if MO/WI implement portions of their state.
Sample <- Sample %>%
  mutate(Administrator = case_when(MILL_STATECD == 26 ~ "MI_DNR",
                                   MILL_STATECD == 27 ~ "MN State",
                                   MILL_STATECD == 50 ~ "VT State",
                                   !MILL_STATECD %in% c(26,27,50) ~ "FFRC"))

# Filter out Pulp/Composite mills...These are handled entirely by Ron and should not be included in FFRC's RRs
Sample <- Sample %>%
  filter(!MILL_TYPE_CD %in% c(30,40))

Sample <- Sample %>%
  select(-c(SURVEY_YEAR:MILL_NBR, MEANING:note))

Sample <- Sample %>%
  mutate("Qualified Respondent Mode" = NA,
        "used old data" = NA,
        `"Mill Status" Notes` = NA,
        `Source of "Mill Status" Notes` = NA, 
        "Return Date" = NA, 
        Comments = NA,
        UNIQUE_ID_CORRECTED = NA)

#Function to convert state codes
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
Sample <- Sample %>%
  State_Code_Conversion()

Sample <- Sample[, c(8:14, 1, 7, 15, 2:4, 16, 5:6)]


Sample <- Sample %>%
  rename('FULL_NAME' = 'MILL_NAME', 'MAILING_ADDRESS' = 'MILL_STREET1', 'MAILING_CITY' = 'MILL_CITY', 
         'MAILING_ZIP' = 'MILL_ZIP_CODE', 'MAILING_STATE' = 'MILL_STATE')

write.xlsx(Sample, file = RLPath)

