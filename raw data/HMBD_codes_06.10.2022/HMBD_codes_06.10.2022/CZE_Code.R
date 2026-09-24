setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/CZE")

# ******************************************************************************
#                                CZECH REPUBLIC
#
# This file provides the calculations performed on the input data for the Czech
# Republic, as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("CZE_InputData_07.07.2022.xlsx", sheet = "input data")
head(data)


#...............................................................................
data %>%
  mutate(Singletons = ifelse(Year <= 1937, 
                             Total_children - Multiple_children,
                             ifelse(Year >= 1950, 
                                    Total_deliveries - Multiple_deliveries,
                                    Singletons)),
         Multiple_deliveries = ifelse(Year <= 1949, 
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
                                      Multiple_deliveries),
         Multiple_children = ifelse(Year >= 1950,
                                    Total_children - Singletons,
                                    Multiple_children),
         Total_deliveries = ifelse(Year <= 1937,
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data



# ..............................................................................
data %>%
  rowwise() %>%
  mutate(check1 = round(Multiple_deliveries - sum(Twin_deliveries, 
                                                  Triplet_deliveries, 
                                                  Quadruplet_plus_deliveries,
                                                  na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(check2 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check3 = round(Total_children - Singletons - Multiple_children, 2)) %>%
  as.data.frame() %>%
  filter(check1 != 0 | check2 != 0 | check3 != 0) -> check


# Identify outliers.............................................................
outliers_tr <- tsoutliers(data$Twinning_rate)
outliers_mr <- tsoutliers(data$Multiple_rate)

data %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(outlier = ifelse(row_number() %in% outliers_tr$index |
                            row_number() %in% outliers_mr$index,
                          1, 0)) -> check

subset(check, outlier == 1)

ggplot() +
  geom_point(data = data, 
             aes(x = Year, y = Twinning_rate)) +
  geom_point(data = subset(data, Year %in% unique(check$Year[check$outlier == 1])),
             aes(x = Year, y = Twinning_rate), colour = "red", size = 1.5)


ggplot() +
  geom_point(data = data, 
             aes(x = Year, y = Multiple_rate)) +
  geom_point(data = subset(data, Year %in% unique(check$Year[check$outlier == 1])),
             aes(x = Year, y = Multiple_rate), colour = "red", size = 1.5)


# Remove 1919 from pooled data file: unlikely results, dubious quality
data %>% filter(Year != 1919) -> data

# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/CZE_ALLDATA.txt",
            row.names = F)




  


