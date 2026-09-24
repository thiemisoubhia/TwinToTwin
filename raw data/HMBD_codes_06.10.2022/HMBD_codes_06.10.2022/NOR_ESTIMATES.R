library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                NORWAY
# ******************************************************************************

data <- read.xlsx("NOR_InputData_Metadata_29.09.2021.xlsx", sheet = "input data")
head(data)



# Data from the historical yearbooks (1899-1945)................................
# NOTE: All the deliveries of quadruplets or more children from 1899 to 1945
# involve quadruplets, hence the multiplication by 4 in the calculation of the 
# column Quadruplet_plus_children.
data %>%
  filter(Year <= 1945) %>% 
  mutate(Twin_children = Twin_deliveries * 2,
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_children = Quadruplet_plus_deliveries * 4,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Singletons = Total_children - Multiple_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_yearbooks

   



# Data from Statistics Norway - online (1945-2020)..............................
# NOTE: The column Triplet_deliveries corresponds to the deliveries of triplets 
# and more children, as it is not possible to distinguish between the number of 
# deliveries of triplets and the number of deliveries of quadruplets and more 
# children in the original data source.
data %>%
  filter(Year > 1945) %>% 
  mutate(Twin_children = Twin_deliveries * 2,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries,
         Multiple_children = Total_children - Singletons,
         Triplet_children =  Multiple_children - Twin_children,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_online


# Compile data and estimates from all sources................................... 
data <- rbind(data_yearbooks, data_online)
data %>% arrange(Year) -> data


# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/NOR_ALLDATA.txt",
            row.names = F)


