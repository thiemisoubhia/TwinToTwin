library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                REPUBLIC OF KOREA
# ******************************************************************************

data <- read.xlsx("KOR_InputData_Metadata_08.03.2022.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data

data %>%
  filter(Year >= 1991) %>%
  mutate(Twin_deliveries = Twin_children / 2,
         Triplet_deliveries = Triplet_children / 3, # Includes deliveries of quadruplets or more
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries,
         Multiple_children = ifelse(Year < 1993,
                                    Twin_children + Triplet_children,
                                    Multiple_children),
         Unknown = Total_children - Singletons - Multiple_children) -> data


# Calculate total number of deliveries taking into account the births of unknown plurality:

data %>%
  mutate(p_singletons = Singletons / Total_children,
         Unknown_single_deliveries = Unknown * p_singletons,
         Unknown_multiple_deliveries = (Unknown * (1 - p_singletons)) / 2,
         # Assuming that most multiple deliveries are twin deliveries
         Unknown_total_deliveries = Unknown_single_deliveries + Unknown_multiple_deliveries) -> data


data %>%
  mutate(Total_deliveries = Singletons + Multiple_deliveries + Unknown_total_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) %>%
  select(-Unknown, -Unknown_single_deliveries, -Unknown_multiple_deliveries, 
         -Unknown_total_deliveries, -p_singletons)-> data
# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/KOR_ALLDATA.txt",
            row.names = F)

