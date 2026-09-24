library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   FINLAND
# ******************************************************************************

data <- read.xlsx("FIN_InputData_Metadata_21.09.2021.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data


# Data from Bunle (1906-1936)...................................................
data %>%
  mutate(Singletons = ifelse(Source == "Bunle", 
                             Total_deliveries - Multiple_deliveries,
                             Singletons),
         Twin_deliveries = ifelse(Source == "Bunle",
                                  Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
                                  Twin_deliveries)) -> data


# Data from the Statistics Finland (1941-1987, Yearbooks).......................
data %>%
  mutate(Multiple_deliveries = ifelse(Source == "Tilastokeskus" & Year < 1988,
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
                                      Multiple_deliveries),
         Singletons = ifelse(Source == "Tilastokeskus" & Year < 1988,
                             Total_deliveries - Multiple_deliveries,
                             Singletons)) -> data


# Data from Statistics Finland (2000-2018, online data).........................
data %>%
  mutate(Twin_children = ifelse(Source == "Tilastokeskus" & Year >= 2000,
                                Twin_deliveries * 2,
                                Twin_children),
         Triplet_children = ifelse(Source == "Tilastokeskus" & Year >= 2000,
                                   Triplet_deliveries * 3,
                                   Triplet_children),
         Quadruplet_plus_children = ifelse(Source == "Tilastokeskus" & Year >= 2000,
                                           Quadruplet_plus_deliveries * 4,
                                           Quadruplet_plus_children),
         Multiple_deliveries = ifelse(Source == "Tilastokeskus" & Year >= 2000,
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
                                      Multiple_deliveries),
         Multiple_children = ifelse(Source == "Tilastokeskus" & Year >= 2000,
                                    Twin_children + Triplet_children + Quadruplet_plus_children,
                                    Multiple_children)) -> data


# Common calculations (online and yearbook data from Statistics Finland).........
data %>%
  mutate(Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/FIN_ALLDATA.txt",
            row.names = F)
