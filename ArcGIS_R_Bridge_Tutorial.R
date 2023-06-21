## Import R-ArcGIS Bridge
library(arcgisbinding)
## Import Raster library
library(raster)
## Import RGDAL for Raster Manipulation
library(rgdal)
## Import SF Packages
library(sf)
## Import Species Distribution Modeling library
library(maxlike)
library(tidyverse)
library(tmap)
## Run Check Product to initialize R-ArcGIS Bridge
arc.check_product()

csd <- arc.open("C:\\Users\\ikenn\\Downloads\\data\\census\\census2016.gdb\\CensusSubdivisions")

csd_df <- arc.select(csd, c("PRNAME", "CSDNAME", "CSDUID"))
class(csd_df)

csd_income <- arc.open("C:\\Users\\ikenn\\Downloads\\data\\census\\census2016.gdb\\income2016csd") %>%
  arc.select(c("geo_code", "gnr", "pop2016_t", "popdens_t", "income_median_t"), where_clause = "gnr is not null and gnr < 25") %>% 
  right_join(csd_df, by = c("geo_code" = "CSDUID"))

# Add columns back to the boundaries data frame:
csd_df$income_median_t <- csd_income$income_median_t
csd_df$pop2016_t <- csd_income$pop2016_t
csd_df$popdens_t <- csd_income$popdens_t
csd_df$gnr <- csd_income$gnr


class(csd_income)

csd_df$income_group <- ntile(csd_df$income_median_t, 5)

csd_group_ranges <- csd_df %>% 
  filter(!is.na(income_group)) %>% 
  group_by(income_group) %>%
  summarize_at(vars(pop2016_t,income_median_t), funs(min(., na.rm = TRUE), max(., na.rm = TRUE))) %>% 
  mutate(
    income_min = income_median_t_min,
    income_max = income_median_t_max,
    pop_min = pop2016_t_min,
    pop_max = pop2016_t_max
  ) %>%
  right_join(csd_df, by = income_group) %>% 
  select(income_min, income_max, pop_min, pop_max)


csd_df$income_min = csd_group_ranges$income_min
csd_df$income_max = csd_group_ranges$income_max
csd_df$pop_min = csd_group_ranges$pop_min
csd_df$pop_max = csd_group_ranges$pop_max


## ?arc.data2sp

csd_sp <- arc.data2sp(csd_df)

# Show the income for CSDs in New Brunwswick::
tm_shape(csd_sp) + 
  tm_polygons('income_median_t', 
              title = "Test") +
  tm_layout(legend.format= list(digits = 0), frame = F)


