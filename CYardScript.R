library(tidyverse)
library(readxl)
library(magrittr)
library(ggthemes)
library(ggpubr)
library(writexl)
library(openxlsx)

CYards <- read_xlsx(path = 'C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\MAILING_PRINTING_LISTS\\Printing_Lists\\TPO_PrintList_2021_Unduplicated_VT_WI.xlsx', sheet = 2)

CYards <- CYards %>%
  select(-c(COLLECTION_AGENCY, STREET:ZIPCODE, MILL_PHYSICAL_SAME_AS_MAIL_CD, `Mill #`:MILL_CD, CONTACT_PHONE_EXT, NOTES, MILL_TYPE_DESC)) %>%
  cbind("Notes" = NA, 
        "Call 1_Date/Time" = NA,
        "Notes_Call1" = NA,
        "Call 2_Date/Time" = NA,
        "Notes_Call2" = NA,
        "Call 3_Date/Time" = NA,
        "Notes_Call3" = NA,
        "Completed?" = NA) 
  
CYards <-  CYards[, c(21,1:20, 22:29)]

length(names(CYards))

write.xlsx(CYards, file = "C:\\Users\\ikenn\\OneDrive - University of Massachusetts\\Documents - FFRC_TPO\\2021 TPO Survey\\SURVEY OUTPUT\\NR_CALLS\\ConcentrationYards.xlsx")

