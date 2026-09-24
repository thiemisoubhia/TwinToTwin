setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/AUS")

# ******************************************************************************
#                                AUSTRALIA
#
# This file provides the calculations performed on the input data for 
# Australia, as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("AUS_InputData_27.06.2022.xlsx", sheet = "input data")


head(data)
data %>% arrange(Year) -> data


# Data from Bunle (1907-1936)...................................................
data %>%
  filter(Source == "Bunle") %>%
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Twin_deliveries = Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_Bunle


# Data from the Australian Bureau of Statistics (ABS):
# Yearbooks and Demographic Bulletins (1937-1974)................................
data %>%
  filter(Source == "ABS" & Year < 1975) %>%
  mutate(Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_ABS1                             
                                    

# Online data (1975-2020)........................................................
data %>%
  filter(Source == "ABS" & Year >= 1975) %>%
  mutate(Multiple_children = (Twin_deliveries*2) + (Triplet_deliveries*3),
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_ABS2 
                                      

# Compile results............................................................... 
data <- rbind(data_Bunle, data_ABS1, data_ABS2)
rm(data_Bunle, data_ABS1, data_ABS2)
data %>% arrange(Year) -> data


# Identify outliers.............................................................
outliers_tr <- tsoutliers(data$Twinning_rate)
outliers_mr <- tsoutliers(data$Multiple_rate)

data %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(outlier = ifelse(row_number() %in% outliers_tr$index |
                            row_number() %in% outliers_mr$index,
                          1, 0)) -> check

subset(check, outlier == 1)


# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/AUS_ALLDATA.txt",
            row.names = F)


