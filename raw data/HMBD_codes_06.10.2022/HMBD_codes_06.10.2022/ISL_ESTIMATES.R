library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   ICELAND
# ******************************************************************************

data <- read.xlsx("ISL_InputData_Metadata_22.10.2021.xlsx", sheet = "input data")
head(data)
sapply(data, class)

data %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(Year != 1853)-> data

# Data from Statistics Iceland (1850-2020)......................................
data %>%
  mutate(Multiple_children = ifelse(Year <= 1950,
                                    (Twin_deliveries * 2) + (Triplet_deliveries * 3) + (Quadruplet_plus_deliveries * 4),
                                    Twin_children + Triplet_children + Quadruplet_plus_children),
         Twin_deliveries = ifelse(Year >= 2017,
                                  Twin_children / 2,
                                  Twin_deliveries),
         Triplet_deliveries = ifelse(Year >= 2017,
                                     Triplet_children / 3,
                                     Triplet_deliveries),
         Quadruplet_plus_deliveries = ifelse(Year >= 2017,
                                             Quadruplet_plus_children / 4,
                                             Quadruplet_plus_deliveries),
         Multiple_deliveries = ifelse(Year >= 2017,
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
                                      Multiple_deliveries),
         Total_deliveries = ifelse(Year >= 1991,
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries),
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data


# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/ISL_ALLDATA.txt",
            row.names = F)




  


