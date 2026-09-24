library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                THE NETHERLANDS
# ******************************************************************************

data <- read.xlsx("NLD_InputData_Metadata_24.09.2021.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data

# Data from the Central Bureau of Statistics (1900-1949).........................
data %>%
  mutate(Twin_children = ifelse(Source == "CBS" & Year < 1950,
                                Twin_deliveries * 2,
                                Twin_children),
         Triplet_children = ifelse(Source == "CBS" & Year < 1950,
                                   Triplet_deliveries * 3,
                                   Triplet_children),
         Quadruplet_plus_children = ifelse(Source == "CBS" & Year < 1950 & Year != 1903,
                                           Quadruplet_plus_deliveries * 4,
                                           Quadruplet_plus_children),
         Quadruplet_plus_children = ifelse(Source == "CBS" & Year == 1903,
                                           4 + 5,
                                           Quadruplet_plus_children),
         Multiple_children = ifelse(Source == "CBS" & Year < 1950,
                                    Twin_children + Triplet_children + Quadruplet_plus_children,
                                    Multiple_children),
         Total_deliveries = ifelse(Source == "CBS" & Year < 1950,
                                   floor(Twin_deliveries / (Twinning_rate/1000)),
                                   Total_deliveries),
         Singletons = ifelse(Source == "CBS" & Year < 1950,
                             Total_deliveries - Multiple_deliveries,
                             Singletons)) -> data


# Data from the Central Bureau of Statistics (1950-2020)........................
data %>%
  mutate(Twin_children = ifelse(Source == "CBS" & Year >= 1950,
                                Twin_deliveries * 2,
                                Twin_children),
         Multiple_children = ifelse(Source == "CBS" & Year >= 1950,
                                    Total_children - Singletons,
                                    Multiple_children),
         # Number of children from Triplet, Quadruplet, etc. deliveries:
         Triplet_children = ifelse(Source == "CBS" & Year >= 1950,
                                   Multiple_children - Twin_children,
                                   Triplet_children),
         Twinning_rate = ifelse(Source == "CBS" & Year >= 1950,
                                (Twin_deliveries / Total_deliveries) * 1000,
                                Twinning_rate),
         Multiple_rate = ifelse(Source == "CBS" & Year >= 1950,
                                (Multiple_deliveries / Total_deliveries) * 1000,
                                Multiple_rate)) -> data



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/NLD_ALLDATA.txt",
            row.names = F)


