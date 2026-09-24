library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   GRECE
# ******************************************************************************

data <- read.xlsx("GRC_InputData_Metadata_23.09.2021.xlsx", sheet = "input data")
head(data)


data %>%
  mutate(Twin_deliveries = Twin_children / 2,
         Triplet_deliveries = Triplet_children / 3,
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries),
                                             Quadruplet_plus_children / 4,
                                             Quadruplet_plus_deliveries),
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/GRC_ALLDATA.txt",
            row.names = F)




  


