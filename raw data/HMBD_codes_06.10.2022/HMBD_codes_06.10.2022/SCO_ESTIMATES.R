library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                SCOTLAND
# ******************************************************************************

data <- read.xlsx("SCO_InputData_Metadata_17.12.2021.xlsx", sheet = "input data")
head(data)



# Data from RGS (1856-1956)...................................................
# NOTE: All the deliveries of quadruplets or more children from 1856 to 1956
# involve quadruplets, hence the multiplication by 4 in the calculation of the 
# column Quadruplet_plus_children.
data %>%
  filter(Source == "RGS") %>% 
  mutate(Twin_children = Twin_deliveries * 2,
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_children = Quadruplet_plus_deliveries * 4,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Singletons = Total_children - Multiple_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_RGS

   

                          

# Data from NRS (1996-2019).....................................................
# The column Triplet_deliveries includes also the deliveries of quadruplets and 
# more children, as the annual number of deliveries of triplets or more children 
# is aggregated in the original data source. 
data %>%
  filter(Source == "NRS" & Year >= 1996) %>% 
  mutate(Twin_children = Twin_deliveries * 2,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries,
         Singletons = Total_deliveries - Multiple_deliveries,
         Multiple_children = Total_children - Singletons,
         Triplet_children = Multiple_children - Twin_children,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_NRS


# Compile data and estimates from all sources................................... 
data <- rbind(data_RGS, data_NRS)
data %>% arrange(Year) -> data


# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/SCO_ALLDATA.txt",
            row.names = F)


