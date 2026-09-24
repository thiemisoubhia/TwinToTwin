library(dplyr)
library(tidyr)

# Set working directory 
setwd("F:/PROJET JUMEAUX INED-MNHM/DATABASE/ESTIMATES")

# Load data and remove the variables 'Flag' or 'Footnotes'. 'Flag' is being 
# replaced by 'Footnotes'. The latter will be included in the pooled data file 
# when it is available for all countries. 

AUS <- read.table("AUS_ALLDATA.txt", header = T) # Australia
AUS %>% select(-Footnotes) -> AUS

AUT <- read.table("AUT_ALLDATA.txt", header = T) # Austria
AUT %>% select(-Footnotes) -> AUT

CAN <- read.table("CAN_ALLDATA.txt", header = T) # Canada
CAN %>% select(-Flag) -> CAN

CHE <- read.table("CHE_ALLDATA.txt", header = T) # Switzerland
CHE %>% select(-Flag) -> CHE

CHL <- read.table("CHL_ALLDATA.txt", header = T) # Chile
CHL %>% select(-Flag) -> CHL

CZE <- read.table("CZE_ALLDATA.txt", header = T) # Czech Republic
CZE %>% select(-Footnotes) -> CZE

DEU <- read.table("DEU_ALLDATA.txt", header = T) # Germany
DEU %>% select(-Flag) -> DEU

DNK <- read.table("DNK_ALLDATA.txt", header = T) # Denmark
DNK %>% select(-Flag) -> DNK

ENW <- read.table("ENW_ALLDATA.txt", header = T) # England & Wales
ENW %>% select(-Footnotes) -> ENW

ESP <- read.table("ESP_ALLDATA.txt", header = T) # Spain
ESP %>% select(-Flag) -> ESP

FIN <- read.table("FIN_ALLDATA.txt", header = T) # Finland
FIN %>% select(-Flag) -> FIN

FRA <- read.table("FRA_ALLDATA.txt", header = T) # France
FRA %>% select(-Flag) -> FRA

GRC <- read.table("GRC_ALLDATA.txt", header = T) # Greece
GRC %>% select(-Flag) -> GRC

ISL <- read.table("ISL_ALLDATA.txt", header = T) # Iceland
ISL %>% select(-Flag) -> ISL

ITA <- read.table("ITA_ALLDATA.txt", header = T) # Italy
ITA %>% select(-Flag) -> ITA

JPN <- read.table("JPN_ALLDATA.txt", header = T) # Japan
JPN %>% select(-Flag) -> JPN

KOR <- read.table("KOR_ALLDATA.txt", header = T) # Republic of Korea
KOR %>% select(-Flag) -> KOR

LTU <- read.table("LTU_ALLDATA.txt", header = T) # Lithuania
LTU %>% select(-Flag) -> LTU

NLD <- read.table("NLD_ALLDATA.txt", header = T) # The Netherlands
NLD %>% select(-Flag) -> NLD

NZL <- read.table("NZL_ALLDATA.txt", header = T) # New Zealand
NZL %>% select(-Footnotes) -> NZL

NOR <- read.table("NOR_ALLDATA.txt", header = T) # Norway
NOR %>% select(-Flag) -> NOR

SCO <- read.table("SCO_ALLDATA.txt", header = T) # Scotland
SCO %>% select(-Flag) -> SCO

SWE <- read.table("SWE_ALLDATA.txt", header = T) # Sweden
SWE %>% select(-Flag) -> SWE

URY <- read.table("URY_ALLDATA.txt", header = T) # Uruguay
URY %>% select(-Footnotes) -> URY

USA <- read.table("USA_ALLDATA.txt", header = T) # United States
USA %>% select(-Flag) -> USA


# Merge data
DB <- rbind(AUS, AUT, CAN, CHE, CHL,
            CZE, DEU, DNK, ENW, ESP, 
            FIN, FRA, GRC, ISL, ITA, 
            JPN, KOR, LTU, NLD, NZL, 
            NOR, SCO, SWE, URY, USA)

head(DB)

UK <- c("England and Wales", "Scotland")

DB %>% 
  filter(!is.na(Source)) %>%
  mutate(Country = gsub("_", " ",Country))%>%
  mutate(Country = ifelse(Country %in% UK, 
                          paste("UK", Country, sep = "-"),
                          Country))%>%
  mutate(Singletons = round(Singletons, 2),
         Twin_deliveries = round(Twin_deliveries, 2),
         Triplet_deliveries = round(Triplet_deliveries, 2),
         Quadruplet_plus_deliveries = round(Quadruplet_plus_deliveries, 2),
         Multiple_deliveries = round(Multiple_deliveries, 2), 
         Multiple_children = round(Multiple_children, 2),
         Total_deliveries = round(Total_deliveries, 2), 
         Twinning_rate = round(Twinning_rate, 2), 
         Multiple_rate = round(Multiple_rate, 2)) %>%
  select(Country, Source, Year, Stillbirths, Singletons, 
         Twin_deliveries, Triplet_deliveries, Quadruplet_plus_deliveries,
         Multiple_deliveries, Multiple_children,
         Total_deliveries, Twinning_rate, Multiple_rate) %>%
  arrange(Country, Year) -> DB



DB %>%
  mutate(check1 = Total_deliveries - Singletons - Multiple_deliveries) %>%
  rowwise() %>%
  mutate(check2 = Multiple_deliveries - sum(Twin_deliveries, Triplet_deliveries, 
                                            Quadruplet_plus_deliveries, na.rm = T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(round(check1, 2) != 0 | round(check2, 2) != 0)-> check
# Discrepancies for some countries & years may be due to births of unknown plurality.
# These cases and other explanations are indicated in the country-specific metadata files.



name_version <- paste0("HMBD_pooled_data_", 
                       format(Sys.time(), "%d.%m.%Y"), 
                       ".csv")

# Run this line to write the entire dataset:
# write.csv(DB, file = name_version, row.names = F)


name_version <- paste0("HMBD_pooled_data_", 
                       format(Sys.time(), "%d.%m.%Y"), 
                       ".txt")

# Run this line to write the entire dataset:
# write.table(DB, file = name_version, row.names = F)





