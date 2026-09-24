setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/NZL")

# ******************************************************************************
#                                NEW ZEALAND
#
# This file provides the calculations performed on the input data for New Zealand,
# as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("NZL_InputData_21.07.2022.xlsx", sheet = "input data")
head(data)


# Data on live births only (1888-1970)..........................................
data %>% filter(Stillbirths == 99 | Stillbirths == 0) -> data_live

data_live %>%
  mutate(Stillbirths = ifelse(Stillbirths == 99, 0, Stillbirths),
         Twin_children = Twin_deliveries * 2,
         Triplet_deliveries = ifelse(is.na(Triplet_deliveries) & Year >= 1892, 
                                     0, 
                                     Triplet_deliveries),
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries) & Year >= 1892,  
                                             0, 
                                             Quadruplet_plus_deliveries),
         Quadruplet_plus_children = ifelse(Quadruplet_plus_deliveries == 0, 0,
                                           Quadruplet_plus_children),
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Singletons = Total_deliveries - Multiple_deliveries,
         Twinning_rate = ifelse(Year > 1891, 
                                round((Twin_deliveries / Total_deliveries) * 1000, 2),
                                Twinning_rate),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) %>%
  arrange(Year) -> data_live




# Data all births - live & still-births (1928-2021)..............................
data %>% filter(Stillbirths == 1) -> data_all

data_all %>%
  mutate(Twin_children = Twin_deliveries * 2,
         Triplet_deliveries = ifelse(is.na(Triplet_deliveries),0 , Triplet_deliveries),
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries) & Year < 1971,
                                             0, Quadruplet_plus_deliveries),
         Quadruplet_plus_children = ifelse(Quadruplet_plus_deliveries == 0, 0,
                                           Quadruplet_plus_children),
         Multiple_deliveries = ifelse(Year >= 1971,
                                      Twin_deliveries + Triplet_deliveries,
                                      Multiple_deliveries),
         Multiple_children = ifelse(Year >= 1971,
                                    Total_children - Singletons,
                                    Twin_children + Triplet_children + Quadruplet_plus_children),
         Singletons = ifelse(Year < 1971,
                             Total_children - Multiple_children,
                             Singletons),
         Total_deliveries = ifelse(is.na(Total_deliveries) &  Year < 1971,
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) %>%
  arrange(Year) -> data_all



# Identify outliers.............................................................
outliers_tr <- tsoutliers(data_live$Twinning_rate)
outliers_mr <- tsoutliers(data_live$Multiple_rate)

data_live %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(outlier = ifelse(row_number() %in% outliers_tr$index |
                            row_number() %in% outliers_mr$index,
                          1, 0)) -> check

subset(check, outlier == 1)
rm(check, outliers_tr, outliers_mr)


outliers_tr <- tsoutliers(data_all$Twinning_rate)
outliers_mr <- tsoutliers(data_all$Multiple_rate)

data_all %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(outlier = ifelse(row_number() %in% outliers_tr$index |
                            row_number() %in% outliers_mr$index,
                          1, 0)) -> check

subset(check, outlier == 1)
rm(check, outliers_tr, outliers_mr)



# Compile results...............................................................
data <- rbind(data_live, data_all)
data %>% arrange(Year) -> data
rm(data_live, data_all)


# Check discrepancies ...........................................................
data %>%
  mutate(check1 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check2 = round(Total_children - Singletons - Multiple_children, 2)) %>%
  as.data.frame() %>%
  filter(check1 != 0 | check2 != 0) -> check

subset(check, Year < 1971 & Stillbirths == 0)  
subset(check, Year < 1971 & Stillbirths == 1) 
# For data before 1971, some discrepancies are observed between the number of 
# births by plurality based on information on deliveries vs. the number of 
# births by plurality based on information on number of children. Those discrepancies
# are negligible for most years (<= 3 children for any given year), but they are 
# substantial for 1941, 1962 and 1963 (see fn. 2 in metadata). 

subset(check, Year >= 1971) # Discrepancies due to rounding (see fn. 3 in metadata)
rm(check)


# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/NZL_ALLDATA.txt",
            row.names = F)


