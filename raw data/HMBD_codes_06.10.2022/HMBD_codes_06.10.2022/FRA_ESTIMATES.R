library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   FRANCE
# ******************************************************************************

data <- read.xlsx("FRA_InputData_Metadata_18.10.2021.xlsx", sheet = "input data")

data$Year <- as.numeric(data$Year)
head(data)
data %>% arrange(Year) -> data


# Data from SGF (1858-1901).....................................................
data %>%
  filter(Source == "SGF") %>%
  rowwise() %>%
  mutate(Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries),
                                             0, Quadruplet_plus_deliveries),
         Multiple_children = sum(Twin_children,
                                 Triplet_children,
                                 Quadruplet_plus_children,
                                 na.rm = T),
         Multiple_deliveries = sum(Twin_deliveries,
                                   Triplet_deliveries,
                                   Quadruplet_plus_deliveries,
                                   na.rm = T),
         Singletons = Total_children - Multiple_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) %>%
  ungroup() -> data_SGF



# Data from INSEE (1902-2020)...................................................
last_year <- 2020

data %>%
  filter(Source == "INSEE") %>% 
  mutate(Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Singletons = Total_deliveries - Multiple_deliveries,
         Twin_children = Twin_deliveries * 2,
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_children = Total_children - Singletons - Twin_children - Triplet_children,
         Multiple_children = Twin_children, Triplet_children, Quadruplet_plus_children, 
         Twinning_rate = ifelse(Year %in% c(1920:1938, 1946:last_year),
                                (Twin_deliveries / Total_deliveries) * 1000,
                                Twinning_rate),
         Multiple_rate = ifelse(Year %in% c(1920:1938,  1946:last_year),
                                (Multiple_deliveries / Total_deliveries) * 1000,
                                Multiple_rate))-> data_INSEE

# Verify that there are no negative numbers in the column 
# Quadruplet_plus_children and correct if necessary:
table(data_INSEE$Quadruplet_plus_deliveries, data_INSEE$Quadruplet_plus_children)



# Save data.....................................................................

data_FRA <- rbind(data_SGF, data_INSEE)

write.table(data_FRA, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/FRA_ALLDATA.txt",
            row.names = F)




  


