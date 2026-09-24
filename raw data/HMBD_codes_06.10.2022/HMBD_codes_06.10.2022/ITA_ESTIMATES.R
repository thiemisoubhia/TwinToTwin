library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   ITALY
# ******************************************************************************

data <- read.xlsx("ITA_InputData_Metadata_15.09.2021.xlsx", sheet = "input data")
head(data)


# Data from ISTAT (1868-1998)...................................................
data %>%
  filter(Year <= 1998) %>%
  mutate(Quadruplet_plus_deliveries = ifelse(Year != 1985,
                                             Multiple_deliveries - Twin_deliveries - Triplet_deliveries,
                                             Quadruplet_plus_deliveries),
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_ISTAT



# Data from the Health Ministry CeDAP (2002-2019)...............................
data %>%
  filter(Year >= 2002) %>%
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Multiple_children = Total_children - Singletons,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_MinSalute



# Compile estimates and save data...............................................
data <- rbind(data_ISTAT, data_MinSalute)
data %>% arrange(Year) -> data

write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/ITA_ALLDATA.txt",
            row.names = F)




  


