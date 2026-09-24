setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/RAW DATA & METADATA/URY")

# ******************************************************************************
#                                URUGUAY
#
# This file provides the calculations performed on the input data for Uruguay,
# as used in the construction of the Human Multiple Births Database
#
# ******************************************************************************


library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)


data <- read.xlsx("URY_InputData_31.05.2022.xlsx", sheet = "input data", 
                  cols = 1:18)
head(data)


# .....................................................................
# Check the proportions of multiple births:

ggplot(data = data) +
  geom_point(aes(x = Year, y = Twin_children / Multiple_children)) +
  geom_line(aes(x = Year, y = Twin_children / Multiple_children, group = 1))

ggplot(data = data) +
  geom_point(aes(x = Year, y = Triplet_children / Multiple_children)) +
  geom_line(aes(x = Year, y = Triplet_children / Multiple_children, group = 1))

ggplot(data = data) +
  geom_point(aes(x = Year, y = Quadruplet_plus_children / Multiple_children)) +
  geom_line(aes(x = Year, y = Quadruplet_plus_children / Multiple_children, group = 1))

# since there are no visible trends over time, use the average by multiplicity
# as weight, for calculating other variables:

w_twins <- mean(data$Twin_children / data$Multiple_children, na.rm = T)
w_tripl <- mean(data$Triplet_children / data$Multiple_children, na.rm = T)
w_quadr <- 1 - w_twins - w_tripl

# .....................................................................

years <- c(1982, 1984:1988)

data %>% 
  mutate(Twin_deliveries = ifelse(Year %in% years,
                                  round((Multiple_children * w_twins) / 2, 2),
                                  ifelse(Year < 1996 & !(Year %in% years),
                                         round(Twin_children / 2, 2),
                                         Twin_deliveries)),
         Triplet_deliveries = ifelse(Year %in% years,
                                     round((Multiple_children * w_tripl) / 3, 2),
                                     ifelse(Year < 1996 & !(Year %in% years),
                                            round(Triplet_children / 3, 2),
                                            Triplet_deliveries)),
         Quadruplet_plus_deliveries = ifelse(Year %in% years,
                                             round((Multiple_children * w_quadr) / 4, 2),
                                             ifelse(Year < 1996 & !(Year %in% years),
                                                    round(Quadruplet_plus_children / 4, 2),
                                                    Quadruplet_plus_deliveries))) -> data

rm(years)

data %>%
  mutate(unknown_multiple = Multiple_children - (Twin_children + Triplet_children + Quadruplet_plus_children),
         Twin_deliveries = ifelse(Year >= 1996,
                                  round((Twin_children + (w_twins * unknown_multiple)) / 2, 2),
                                  Twin_deliveries),
         Triplet_deliveries = ifelse(Year >= 1996,
                                     round((Triplet_children + (w_tripl * unknown_multiple)) / 3, 2),
                                     Triplet_deliveries),
         Quadruplet_plus_deliveries = ifelse(Year >= 1996,
                                             round((Quadruplet_plus_children + (w_quadr * unknown_multiple)) / 4, 2),
                                             Quadruplet_plus_deliveries),
         Multiple_deliveries = Twin_deliveries + Triplet_deliveries + Quadruplet_plus_deliveries) -> data



data %>% 
  mutate(unknown_births = Total_children - (Singletons + Multiple_children),
         w_singl = Singletons / (Singletons + Multiple_children),
         unknown_deliveries = (unknown_births * w_singl) + ((unknown_births * (1 - w_singl)) / 2),
         Total_deliveries = round(Singletons + Multiple_deliveries + unknown_deliveries, 2),
         Twinning_rate = round((Twin_deliveries / Total_deliveries) * 1000, 2),
         Multiple_rate = round((Multiple_deliveries / Total_deliveries) * 1000, 2)) -> data


data %>% select(-unknown_births, -unknown_multiple, -unknown_deliveries, - w_singl) -> data




# Although data come from live birth certificates, twin children are counted as 
# such, even if the other twin was stillborn (hence the living twin represents 
# one twin delivery). Therefore, the variable Stillbirths is changed to :
# 2 = "Mixed treatment of stillbirths", 
# as estimates on deliveries  by plurality take into consideration the stillbirths,
# in those cases where there is at least one baby born alive.
data %>% mutate(Stillbirths = ifelse(!(is.na(Source)), 2, NA)) -> data


# Identify outliers.............................................................
outliers_tr <- tsoutliers(data$Twinning_rate)
outliers_mr <- tsoutliers(data$Multiple_rate)
# No outliers identified

rm(outliers_tr, outliers_mr)  


# Save data.....................................................................
write.table(data, 
            "F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES/URY_ALLDATA.txt",
            row.names = F)




  


