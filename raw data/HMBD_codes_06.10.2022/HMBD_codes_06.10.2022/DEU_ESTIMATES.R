library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   GERMANY
# ******************************************************************************

data <- read.xlsx("DEU_InputData_Metadata_23.09.2021.xlsx", sheet = "input data")
head(data)


# Data from Bunle (1906-1936)....................................................
data %>%
  mutate(Twin_deliveries = ifelse(Source == "Bunle",
                                  Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
                                  Twin_deliveries),
         Singletons = ifelse(Source == "Bunle",
                             Total_deliveries - Multiple_deliveries,
                             Singletons)) -> data



# Data from Statistics Germany (1950-2019)......................................
last.obs <- max(data$Year)

data %>%
  mutate(Twin_children = ifelse(Source == "Destatis" & Stillbirths == 1,
                                Twin_deliveries * 2,
                                Twin_children),
         Triplet_children = ifelse(Source == "Destatis" & Stillbirths == 1,
                                   Triplet_deliveries * 3,
                                   Triplet_children),
         Quadruplet_plus_children = ifelse(Source == "Destatis" & Stillbirths == 1 & Year <= 2015,
                                           Multiple_children - Twin_children - Triplet_children,
                                           Quadruplet_plus_children),
         Quadruplet_plus_children = ifelse(Source == "Destatis" & Stillbirths == 1 & Year %in% 2016:last.obs,
                                           Quadruplet_plus_deliveries * 4,
                                           Quadruplet_plus_children),
         Multiple_children = ifelse(Source == "Destatis" & Stillbirths == 1 & Year %in% 2016:last.obs,
                                    Twin_children + Triplet_children + Quadruplet_plus_children,
                                    Multiple_children),
         Singletons = ifelse(Source == "Destatis" & Stillbirths == 1,
                             Total_children - Multiple_children,
                             Singletons), 
         Total_deliveries = ifelse(Source == "Destatis",
                                   Singletons + Multiple_deliveries,
                                   Total_deliveries)) -> data



# Common calculations for both sources..........................................
data %>%
  filter(!is.na(Source)) %>% 
  mutate(Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data
         
         


# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/DEU_ALLDATA.txt",
            row.names = F)