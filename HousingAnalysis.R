library(tidyverse)
library(readxl)
library(magrittr)
library(ggthemes)
library(writexl)
library(car)
library(openxlsx)
library(grid)
library(mgcv)
library(tidymodels)
library(h2o)
library(kableExtra)
library(MLmetrics)
library(RColorBrewer)
library(xgboost)
library(ggpubr)

Housing <- read.csv("C:\\Users\\ikenn\\Downloads\\HousingValues.csv")
Rental <- read.csv("C:\\Users\\ikenn\\Downloads\\RentalCosts.csv")

Rental <- Rental %>%
  filter(State == "MA") %>%
  select(-State)

Housing <- Housing %>%
  filter(State == "MA") %>%
  select(-State)

Joined <- Rental %>%
  left_join(Housing, by = 'Zip')

Joined <- Joined %>%
  rename(RentalCost = Value.x, HousingCost = Value.y)

Joined <- Joined %>%
  mutate(Zip = paste0("0", Zip))

ggplot(Joined, aes(RentalCost, HousingCost)) +
  geom_point() 
  
write.xlsx(Joined, file = "C:\\Users\\ikenn\\Downloads\\JoinedData.xlsx")
