library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                CANADA
# ******************************************************************************

data <- read.xlsx("CAN_InputData_Metadata_13.09.2021.xlsx", sheet = "input data")
head(data)



# Data from Bunle (1921-1925)...................................................
data %>%
  filter(Source == "Bunle" & Year < 1926) %>%   # Data from 1926 are in the Year Books
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Twin_deliveries = Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_Bunle
                             
     


# Data from the Year Books of the Dominion Bureau of Statistics (1926-1965).....
data %>%
  filter(Source == "BureauStats") %>%   
  mutate(Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Total_deliveries = ifelse(Year <= 1940, Singletons + Multiple_deliveries, Total_deliveries),
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_BureauStats



# Data from the articles by Imaizumi (1972-1990)................................
data %>% filter(Source == "Imaizumi") -> data_Imaizumi
  



# Data from Statistics Canada: online data (1991-2019)..........................

unknown <- read.xlsx("CAN_InputData_Metadata_13.09.2021.xlsx", sheet = "supplement", 
                     rows = 3:32, cols = 1:2)
names(unknown)[2] <- "Unknown"


data %>% filter(Source == "StatCan") -> data_StatCan

data_StatCan <- left_join(data_StatCan, unknown)
head(data_StatCan)


data_StatCan %>%
  mutate(Twin_deliveries = round(Twin_children / 2, 2),
         Triplet_deliveries = round(Triplet_children / 3, 2),
         Quadruplet_plus_deliveries = round(Quadruplet_plus_children / 4, 2),
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Total_deliveries = Singletons + Multiple_deliveries + Unknown,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_StatCan

data_StatCan %>% select(-Unknown) -> data_StatCan


# Compile data and estimates from all sources................................... 
data <- rbind(data_Bunle, data_BureauStats, data_Imaizumi, data_StatCan)
data %>% arrange(Year) -> data


# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/CAN_ALLDATA.txt",
            row.names = F)


