library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                DENMARK
# ******************************************************************************

data <- read.xlsx("DNK_InputData_Metadata_22.12.2021.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data

# Data from Statistics Denmark (1850-2018)......................................
data %>%
  mutate(Twin_children = Twin_deliveries * 2,
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_children = Total_children - Singletons - Twin_children - Triplet_children,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/DNK_ALLDATA.txt",
            row.names = F)