setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/ENW")

# ******************************************************************************
#                                ENGLAND & WALES
#
# This file provides the calculations performed on the input data for England & Wales,
# as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("ENW_InputData_20.09.2021.xlsx", sheet = "input data")
head(data)




# Compare Dunn & Macfalane vs. ONS data.........................................
# multiple delivereis and total deliveries:

DM_multiple <- data$Multiple_deliveries[data$Source == "Dunn_Macfarlane"]
ONS_multiple <- data$Multiple_deliveries[data$Source == "ONS" & data$Year %in% 1975:1994]
DM_multiple - ONS_multiple # The NA is for 1981, as there are no data for that year 

DM_total <- data$Total_deliveries[data$Source == "Dunn_Macfarlane"]
ONS_total <- data$Total_deliveries[data$Source == "ONS" & data$Year %in% 1975:1994]
DM_total - ONS_total # Differnece of 419 deliveries in 1994. 
                     # Use updated number of total deliveries reported by ONS




# Dunn and Macfarlane (1975-1994) ...............................................
data %>% filter(Source == "Dunn_Macfarlane") -> data_DM
head(data_DM)


# Apply updated number of total deliveries in 1994 (ONS data):
data_DM$Total_deliveries[data_DM$Year == 1994] <- data$Total_deliveries[data$Source == "ONS" & data$Year == 1994]


# Correct number of Singletons in 1994:
data_DM %>%
  mutate(Singletons = ifelse(Year == 1994,
                             Total_deliveries - Multiple_deliveries,
                             Singletons)) -> data_DM


# Add total number of children reported by ONS:
data_DM$Total_children <- data$Total_children[data$Source == "ONS" & data$Year %in% 1975:1994]


# Calculate number of multiple children and twinning and multiple rates:
data_DM %>%
  mutate(Multiple_children = Total_children - Singletons,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_DM
  



# ONS data (1938-1997)...........................................................
data %>%
  filter(Source == "ONS" & Year < 1998) %>%
  filter(!(Year %in% (unique(data_DM$Year)))) %>%
  mutate(Singletons = ifelse(Year != 1940, 
                             Total_deliveries - Multiple_deliveries,
                             Singletons),
         Multiple_children = ifelse(Year != 1940,
                                    Total_children - Singletons,
                                    Multiple_children),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_ONS1




# ONS data (1998-2020)...........................................................
data %>%
  filter(Source == "ONS" & Year >= 1998) %>%
  mutate(Quadruplet_plus_children = ifelse(Year >= 2009,
                                           Total_children - Singletons - (Twin_deliveries * 2) - (Triplet_deliveries * 3),
                                           Quadruplet_plus_children),
         Multiple_children = (Twin_deliveries * 2) + (Triplet_deliveries * 3) + Quadruplet_plus_children,
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data_ONS2




# Compile estimates.............................................................
rm(data)
data <- rbind(data_DM, data_ONS1, data_ONS2)
data %>% arrange(Year) -> data
head(data)
rm(data_DM, data_ONS1, data_ONS2)



# ..............................................................................
data %>%
  rowwise() %>%
  mutate(check1 = round(Multiple_deliveries - sum(Twin_deliveries, 
                                                  Triplet_deliveries, 
                                                  Quadruplet_plus_deliveries,
                                                  na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(check2 = round(Total_deliveries - Singletons - Multiple_deliveries, 2),
         check3 = round(Total_children - Singletons - Multiple_children, 2),
         check4 = round(Multiple_children - ((Twin_deliveries * 2) + (Triplet_deliveries * 3) + Quadruplet_plus_children))) %>%
  as.data.frame() %>%
  filter((check1 != 0 & check1 != Multiple_deliveries) | check2 != 0 | check3 != 0 | check4 != 0) -> check
# Small discrepancies may be due to annual updates on the number of children and 
# the number of multiple maternities, whereas their distribution by multiplicity
# is not updated. Discrepancies <= 2 children (except in 2003, see metadata). 
rm(check)



# Identify outliers.............................................................
outliers_tr <- tsoutliers(data$Twinning_rate)
outliers_mr <- tsoutliers(data$Multiple_rate)
# Several outliers identified in the series of twinning rates, whereas none in the
# series of multiple rates. Maybe it is because the twinning rate series is short 
# and has a lot of NAs.
rm(outliers_mr)

outliers_tr <- data.frame(index = outliers_tr$index,
                          replacement = outliers_tr$replacements)


data %>% 
  select(Source, Year, Twinning_rate, Multiple_rate) %>%
  mutate(index = row_number()) -> check

check <- left_join(check, outliers_tr)

check %>%
  select(-index) %>%
  mutate(outlier = ifelse(is.na(replacement), 0, 1)) -> check


subset(check, outlier == 1)

ggplot(data = check) +
  geom_point(aes(x = Year, y = Multiple_rate)) +
  geom_line(aes(x = Year, y = Multiple_rate)) +
  geom_point(aes(x = Year, y = Twinning_rate), colour = "blue") +
  geom_line(aes(x = Year, y = Twinning_rate), colour = "blue") +
  geom_point(data = subset(check, outlier == 1),
             aes(x = Year, y = Twinning_rate), colour = "red") +
  geom_point(data = subset(check, outlier == 1),
             aes(x = Year, y = replacement), colour = "limegreen", shape = 8)

# The replacements suggested are far from the observed twinning and multiple rates.
# The twinning rate series is too short and has too many NAs, therefore there is not
# much information for outlier identification. Since the observed twinning rate
# is close to the multiple rate, and the latter does not contain outliers, we 
# assume there would not be outliers, if the series was complete. 
rm(outliers_tr, check)  


# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/ENW_ALLDATA.txt",
            row.names = F)




  


