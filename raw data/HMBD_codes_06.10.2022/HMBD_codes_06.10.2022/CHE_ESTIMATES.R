library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                SWITZERLAND
# ******************************************************************************

data <- read.xlsx("CHE_InputData_Metadata_14.12.2021.xlsx", sheet = "input data")
head(data)


# Data from Bunle (1906-1936)...................................................
data %>%
  filter(Source == "Bunle") %>%
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Twin_deliveries = Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
         Multiple_children = (Twin_deliveries * 2) + (Triplet_deliveries * 3) + (Quadruplet_plus_deliveries * 4)) -> data_Bunle





# Data from Statistics Switzerland (1960-2020)...................................
data %>%
  filter(Source == "OFS") %>%
  mutate(Multiple_children = ifelse(Year == 1960,
                                    (Twin_deliveries * 2) + (Triplet_deliveries * 3) + (Quadruplet_plus_deliveries * 4),
                                    Total_children - Singletons),
         Singletons = ifelse(Year == 1960, 
                             Total_children - Multiple_children,
                             Singletons),
         Total_deliveries = Singletons + Multiple_deliveries) -> data_OFS
  




# Common calculations to all sources (Bunle & OFS)..............................
data <- rbind(data_Bunle, data_OFS)

data %>%
  mutate(Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data



# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/CHE_ALLDATA.txt",
            row.names = F)


