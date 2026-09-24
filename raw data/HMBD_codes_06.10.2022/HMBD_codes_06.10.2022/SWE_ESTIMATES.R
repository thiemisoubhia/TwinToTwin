library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                SWEDEN
# ******************************************************************************

data <- read.xlsx("SWE_InputData_Metadata_20.12.2021.xlsx", sheet = "input data")
head(data)



data %>%
  filter(Year >= 1815) %>%   # Drop data up to 1814 (dubious quality)
  mutate(Twin_deliveries = ifelse(is.na(Twin_deliveries), 
                                  Twin_children/2, 
                                  Twin_deliveries),
         Triplet_deliveries = ifelse(is.na(Triplet_deliveries), 
                                     Triplet_children/3, 
                                     Triplet_deliveries),
         Triplet_deliveries = ifelse(Year >= 1911 & Year <= 1920 & Stillbirths == 1, 
                                     Total_deliveries - Singletons - Twin_deliveries, 
                                     Triplet_deliveries),
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries), 
                                             Quadruplet_plus_children/4, 
                                             Quadruplet_plus_deliveries)) %>%
  rowwise() %>%
  mutate(Multiple_deliveries = sum(Twin_deliveries, 
                                   Triplet_deliveries, 
                                   Quadruplet_plus_deliveries, 
                                   na.rm = T),
         Multiple_children = sum(Twin_children, 
                                 Triplet_children, 
                                 Quadruplet_plus_children, 
                                   na.rm = T)) %>%
  ungroup() %>%
  mutate(Total_deliveries = ifelse(is.na(Total_deliveries),
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Singletons = ifelse(Year < 1861,
                             Total_deliveries - Multiple_deliveries,
                             Singletons),
         Multiple_children = ifelse(Year < 1861,
                                    Total_children - Singletons,
                                    Multiple_children),
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data 



data %>%
  rowwise() %>%
  mutate(check1 = round(Multiple_deliveries - sum(Twin_deliveries, 
                                            Triplet_deliveries, 
                                            Quadruplet_plus_deliveries,
                                            na.rm = T), 2),
         check2 = round(Multiple_children - sum(Twin_children, 
                                          Triplet_children, 
                                          Quadruplet_plus_children,
                                          na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(check3 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check4 = round(Total_children - Singletons - Multiple_children, 2)) %>%
  as.data.frame() %>%
  filter(check1 != 0 | check2 != 0 | check3 != 0 | check4 != 0) -> data_check



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/SWE_ALLDATA.txt",
            row.names = F)


