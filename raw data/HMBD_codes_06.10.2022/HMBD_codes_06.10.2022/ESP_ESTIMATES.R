library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   SPAIN
# ******************************************************************************

data <- read.xlsx("ESP_InputData_Metadata_20.12.2021.xlsx", sheet = "input data")
head(data)


data %>%
  filter(Year >= 1901) %>%
  rowwise()%>% 
  mutate(Multiple_deliveries = sum(Twin_deliveries, Triplet_deliveries, 
                                   Quadruplet_plus_deliveries, na.rm = T),
         Multiple_children = ifelse(is.na(Quadruplet_plus_children),
                                    sum(Twin_deliveries*2, Triplet_deliveries*3,
                                        Quadruplet_plus_deliveries*4, na.rm = T),
                                    sum(Twin_deliveries*2, Triplet_deliveries*3,
                                        Quadruplet_plus_children, na.rm = T))) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Total_deliveries = ifelse(is.na(Total_deliveries),
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data


data %>%
  mutate(check1 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check2 = round(Total_children - Singletons - Multiple_children, 2)) %>%
  as.data.frame() %>%
  filter(check1 != 0 | check2 != 0) %>%
  select(Year, check1, check2) -> data_check

# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/ESP_ALLDATA.txt",
            row.names = F)




  


