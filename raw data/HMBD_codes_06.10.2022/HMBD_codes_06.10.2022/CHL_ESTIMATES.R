library(dplyr)
library(tidyr)
library(openxlsx)

# Set working directory 
setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA")

# ******************************************************************************
#                                CHILE
# ******************************************************************************

data <- read.xlsx("CHL_InputData_Metadata_02.03.2022.xlsx", sheet = "input data")
head(data)

data %>% arrange(Year) -> data

drop.years <- c(1969, 1970) # Drop lines for these years because there are no data on multiple births

data %>%
  filter(!(Year %in% drop.years)) %>%
  rowwise() %>%
  mutate(Multiple_children = sum(Twin_children, Triplet_children, Quadruplet_plus_children, na.rm = T)) %>%
  ungroup() %>%
  mutate(Unknown = Total_children - Singletons - Multiple_children,
         Twin_deliveries = ifelse(is.na(Twin_deliveries),
                                  round(Twin_children / 2, 2),
                                  Twin_deliveries),
         Triplet_deliveries = ifelse(is.na(Triplet_deliveries),
                                     round(Triplet_children / 3, 2),
                                     Triplet_deliveries),
         Quadruplet_plus_deliveries = ifelse(is.na(Quadruplet_plus_deliveries),
                                             round(Quadruplet_plus_children / 4, 2),
                                             Quadruplet_plus_deliveries)) %>%
  rowwise() %>%
  mutate(Multiple_deliveries = sum(Twin_deliveries, Triplet_deliveries, Quadruplet_plus_deliveries, na.rm = T)) %>%
  ungroup() %>%
  as.data.frame() -> data
  
  
# Calculate total number of deliveries taking into account the births of unknown plurality:

data %>%
  mutate(p_singletons = Singletons / Total_children,
         Unknown_single_deliveries = Unknown * p_singletons,
         Unknown_multiple_deliveries = (Unknown * (1 - p_singletons)) / 2,
         # Assuming that most multiple deliveries are twin deliveries
         Unknown_total_deliveries = Unknown_single_deliveries + Unknown_multiple_deliveries) -> data


data %>%
  mutate(Total_deliveries = Singletons + Multiple_deliveries + Unknown_total_deliveries,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) %>%
  select(-Unknown, -Unknown_single_deliveries, -Unknown_multiple_deliveries, 
         -Unknown_total_deliveries, -p_singletons)-> data


# Identify outliers:
library(ggplot2)
library(forecast)

outliers_tr <- tsoutliers(data$Twinning_rate)
outliers_mr <- tsoutliers(data$Multiple_rate)

data %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(outlier = ifelse(row_number() %in% outliers_tr$index |
                            row_number() %in% outliers_mr$index,
                          1, 0)) -> check



# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/CHL_ALLDATA.txt",
            row.names = F)