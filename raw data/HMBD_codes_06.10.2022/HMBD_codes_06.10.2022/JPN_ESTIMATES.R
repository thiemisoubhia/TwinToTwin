library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                JAPAN
# ******************************************************************************

data <- read.xlsx("JPN_InputData_Metadata_24.09.2021.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data


# Data from Bunle (1923-1936)...................................................
data %>%
  filter(Source == "Bunle") %>%
  mutate(Singletons = Total_deliveries - Multiple_deliveries,
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries), 0, Quadruplet_plus_deliveries),
         Twin_deliveries = Multiple_deliveries - Triplet_deliveries - Quadruplet_plus_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_Bunle



# Data from Imaizumi (1951-1974)................................................
data %>%
  filter(Source == "Imaizumi" & Year >= 1951 & Year <= 1974) %>%
  mutate(Quadruplet_plus_deliveries = Quadruplet_plus_children / 4,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Singletons = Total_deliveries - Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_Imaizumi1



# Data from Imaizumi (1975-1992)................................................
data %>%
  filter(Source == "Imaizumi" & Year >= 1975 & Year <= 1992) %>%
  mutate(Twin_deliveries = Twin_children / 2,
         Triplet_deliveries = Triplet_children / 3,
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries,
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Singletons = Total_children - Multiple_children,
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_Imaizumi2



# Data from Imaizumi (1993-1994 & 1996).........................................
data %>%
  filter(Source == "Imaizumi" & (Year == 1993 | Year == 1994 | Year == 1996)) -> data_Imaizumi3
  
        
  
# Data from Statistics Japan (2000-2019)........................................
data %>%
  filter(Source == "StatisticsJapan" | Year == 1995) %>%
  mutate(Multiple_children = (Twin_deliveries * 2) + (Triplet_deliveries * 3) + Quadruplet_plus_children,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_StatsJapan



# Gather data...................................................................
data <- rbind(data_Bunle, data_Imaizumi1, data_Imaizumi2, data_Imaizumi3, data_StatsJapan)
data %>% arrange(Year) -> data
  

# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/JPN_ALLDATA.txt",
            row.names = F)