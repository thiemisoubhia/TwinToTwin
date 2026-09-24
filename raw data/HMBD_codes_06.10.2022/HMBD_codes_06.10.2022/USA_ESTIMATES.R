library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("E:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                UNITED STATES
# ******************************************************************************

data <- read.xlsx("USA_InputData_Metadata_20.12.2021.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data


# Data from the Vital Statistics - unit: deliveries (1915-1958).................
data %>%
  filter(Year <= 1958) %>%
  mutate(Twin_children = Twin_deliveries * 2,
         Triplet_children = Triplet_deliveries * 3,
         Quadruplet_plus_children = ifelse(is.na(Quadruplet_plus_children), 
                                           Quadruplet_plus_deliveries * 4, 
                                           Quadruplet_plus_children),
         Multiple_children = Twin_children + Triplet_children + Quadruplet_plus_children,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_VSUS_deliveries





# Data from the Vital Statistics - unit: children (1959-2020)...................

# From 1961 to 1988, information on triplets + is aggregated (i.e. no distinction 
# between triplets, quadruplets, etc.) in the sources. Use "par" to calculate the
# number of Triplet+ deliveries from 1961 to 1988:

par <- 0.9177 # This is the average proportion of Triplet children (among triplets +)
              # based on the data available from 1989 to 2002. For those years, 
              # that proportion fluctuates around similar values, meaning that it
              # does not follow an increasing or decreasing trend (it is flat). 

data %>%
  filter(Year >= 1959) %>%
  mutate(Twin_deliveries = Twin_children / 2,
         Triplet_deliveries = ifelse(Year >= 1961 & Year <= 1988,
                                     ((par*(Triplet_children / 3)) + ((1-par)*(Triplet_children/4))),
                                     Triplet_children / 3),
         Quadruplet_plus_deliveries = Quadruplet_plus_children / 4,
         Multiple_deliveries = ifelse(Year >= 1961 & Year <= 1988,
                                      Twin_deliveries + Triplet_deliveries, 
                                      Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries),
         Multiple_children = ifelse(Year >= 1961 & Year <= 1988,
                                    Twin_children + Triplet_children, 
                                    Twin_children + Triplet_children + Quadruplet_plus_children),
         Total_deliveries = Singletons + Multiple_deliveries,
         Twinning_rate = (Twin_deliveries / Total_deliveries) * 1000,
         Multiple_rate = (Multiple_deliveries / Total_deliveries) * 1000) -> data_VSUS_children




# Save data.....................................................................
data <- rbind(data_VSUS_deliveries, data_VSUS_children)
data %>% arrange(Year) -> data


write.table(data, 
            "E:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/USA_ALLDATA.txt",
            row.names = F)


