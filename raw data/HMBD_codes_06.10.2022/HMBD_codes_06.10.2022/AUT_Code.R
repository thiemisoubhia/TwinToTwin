setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/AUT")

# ******************************************************************************
#                                AUSTRIA
#
# This file provides the calculations performed on the input data for 
# Austria, as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("AUT_InputData_07.07.2022.xlsx", sheet = "input data")
head(data)


#...............................................................................
par <- 0.96 # Approx. proportion of triplets in the multiple births triplets +

data %>%
  filter(!is.na(Source)) %>%
  arrange(Year) -> data


# Data from Bunle (1906-1934)...................................................
# Data from 1906 to 1913 are removed because they refer to the Austro-Hungarian territory

data %>%
  filter(Source == "Bunle" & Year > 1913) %>% 
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Twin_deliveries = ifelse(Year < 1911,
                                  Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
                                  Multiple_deliveries - Triplet_deliveries)) -> data_Bunle



# Data from Statistics Austria (1938-1945)......................................
data %>%
  filter(Source == "Statistik Austria") %>%
  mutate(Singletons = ifelse(Year >= 1938 & Year <= 1945,
                             (Total_children - 
                               (Twin_deliveries * 2) - 
                               (par * (Triplet_deliveries * 3)) -
                               ((1 - par) * (Triplet_deliveries * 4))),
                             Singletons),
         Multiple_deliveries = ifelse(Year >= 1938 & Year <= 1945,
                                      Twin_deliveries + Triplet_deliveries,
                                      Multiple_deliveries),
         Total_deliveries = ifelse(Year >= 1938 & Year <= 1945,
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries)) -> data_StatistikAustria
  


# Data from Statistics Austria (1970-1990)......................................
data_StatistikAustria %>%
  mutate(Twin_deliveries = ifelse(Year >= 1970 & Year <= 1990,
                                  Twin_children / 2,
                                  Twin_deliveries),
         Triplet_deliveries = ifelse(Year >= 1970 & Year <= 1990,
                                     Triplet_children / 3,
                                     Triplet_deliveries),
         # For this period, there are zero births of quintuplets+ 
         # so the variable Quadruplet_plus_children comprises only quadruplets:
         Quadruplet_plus_deliveries = ifelse(Year >= 1970 & Year <= 1990,
                                           Quadruplet_plus_children / 4,
                                           Quadruplet_plus_deliveries),
         Multiple_deliveries = ifelse(Year >= 1970 & Year <= 1990,
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
                                      Multiple_deliveries),
         Multiple_children = ifelse(Year >= 1970 & Year <= 1990,
                                    Twin_children + Triplet_children + Quadruplet_plus_children,
                                    Multiple_children),
         Total_deliveries = ifelse(Year >= 1970 & Year <= 1990,
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Total_children = ifelse(Year >= 1970 & Year <= 1990,
                                 Singletons + Multiple_children,
                                 Total_children)) -> data_StatistikAustria



# Compile results...............................................................
data <- rbind(data_Bunle, data_StatistikAustria)
rm(data_Bunle, data_StatistikAustria)

data %>%
  mutate(Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data


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
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/AUT_ALLDATA.txt",
            row.names = F)


