library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                   LITHUANIA
# ******************************************************************************

data <- read.xlsx("LTU_InputData_Metadata_18.02.2022.xlsx", sheet = "input data")
head(data)


data %>%
  filter(!(is.na(Source))) %>%
  rowwise()%>% 
  mutate(Multiple_deliveries = sum(Twin_deliveries, Triplet_deliveries, 
                                   Quadruplet_plus_deliveries, na.rm = T),
         Multiple_children = sum(Twin_deliveries*2, 
                                 Triplet_deliveries*3,
                                 Quadruplet_plus_deliveries*4, 
                                 na.rm = T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Singletons = Total_children - Multiple_children,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data



data %>%
  mutate(check1 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check2 = round(Total_children - Singletons - Multiple_children, 2)) %>%
  as.data.frame() %>%
  filter(check1 != 0 | check2 != 0) %>%
  select(Year, check1, check2) -> data_check



# Save data.....................................................................
write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/LTU_ALLDATA.txt",
            row.names = F)





  


