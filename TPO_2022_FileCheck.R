library(tidyverse)
library(readxl)
library(openxlsx)

RL <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2022 Mill Data (SY2023)\\TPO2022_ReturnLogs.xlsx", sheet = 2)
CYards <- read.xlsx("C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2022 Mill Data (SY2023)\\TPO2022_ReturnLogs.xlsx", sheet = 5)


RL <- RL %>%
  select(TPOID, FULL_NAME, Response, Response.Notes, Return_Date, MILL_STATECD, MILL_NBR)

RL <- RL %>%
  filter(Response %in% c("MAIL", "IDLE", "PHONE ONLY", "NIS-CHANGE", "PHONE_NR", "DISMANTLED", "OOB"))

RL %>%
  arrange(MILL_NBR) %>%
  filter(MILL_STATECD == 29) 

OOB_2021 <- RL %>%
  filter(Response %in% c("OOB", "DISMANTLED")) %>%
  filter(str_detect(Response.Notes, "in 2021"))

OOB_2021 <- OOB_2021$TPOID



MissingFiles <- c("10-2022-29-10", "10-2022-30-110", "17-2022-265-10", "17-2022-315-10", "17-2022-352-10",
                   "18-2022-463-10", "19-2022-14-10", "19-2022-102-10", "19-2022-103-10", "20-2022-74-10", 
                  "20-2022-104-10", "20-2022-136-10", "25-2022-12-110", "26-2022-181-10", "27-2022-204-10",
                  "31-2022-28-10", "31-2022-46-10", "31-2022-115-10", "33-2022-28-10", "33-2022-41-10", 
                  "34-2022-29-10", "36-2022-7-10", "39-2022-105-10", "39-2022-108-10", 
                  "39-2022-113-10", "39-2022-129-10", "39-2022-151-10", "39-2022-156-10", "39-2022-160-10",
                  "39-2022-170-10", "39-2022-184-10", "39-2022-201-10", "39-2022-212-10",
                  "39-2022-213-20", "39-2022-216-20", "42-2022-11-10", "42-2022-73-10", 
                  "42-2022-92-10", "44-2022-6-10", "46-2022-1302-80", "54-2022-19-10", "54-2022-52-10",
                  "54-2022-98-10", "54-2022-138-10", "54-2022-146-10", "54-2022-218-20", "55-2022-173-10",
                  "55-2022-833-10", "29-2022-563-10", "29-2022-970-10", "29-2022-975-10")


MissingFiles <- as.data.frame(MissingFiles)

MissingFiles <- MissingFiles %>%
  filter(!MissingFiles %in% OOB_2021)

MissingFiles <- MissingFiles$MissingFiles



FilesPresent_MissinginRL <- c("18-2022-549-10", "20-2022-42-10", "29-2022-127-10", "29-2022-3025-10",
                              "33-2022-29-10", "33-2022-36-10", "34-2022-4-10",
                              "36-2022-80-10", "36-2022-117-10", "36-2022-135-10", "36-2022-136-10", "42-2022-140-10",
                              "42-2022-14601-50", "55-2022-112-10", "55-2022-374-110", "55-2022-601-10", "55-2022-900-50",
                              "55-2022-917-10", "55-2022-926-10", "55-2022-940-50", "55-2022-81802-90")

MissingData <- data.frame(MissingFromRL_Scanned = FilesPresent_MissinginRL, 
                             MissingfromScanned_InRL = MissingFiles, 
                             OOB_2021 = OOB_2021)

dataset_names <- list('MissingFromRL_Scanned' = FilesPresent_MissinginRL, 
                      'MissingfromScanned_InRL' = MissingFiles, 
                      'OOB_2021' = OOB_2021)

write.xlsx(dataset_names, "C:\\Users\\ikenn\\Downloads\\FileCheck.xlsx")
