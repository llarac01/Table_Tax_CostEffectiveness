####################################################################################################################################
#Title: Cost effectiveness of a Soda policy 
#By: Laura LC
#Date: Oct 19, 2023
#Last modified: 
####################################################################################################################################

rm(list = ls())
#rm(list = ls()[!(ls() %in% c('props.adults'))])

#-------------------------------------------
#PART 1. READ IN LIBRARIES, PATHS AND FILES
#-------------------------------------------

library(dplyr) # select, left_join
library(readxl)

path1 = "~/Library/CloudStorage/Box-Box/05_THESIS/03_THESIS/03_TAX_SSBs/03_OUTPUTS/"
path2 = "~/Library/CloudStorage/Box-Box/05_THESIS/03_THESIS/03_TAX_SSBs/01_INPUTS/10_Costs/WHO_Costing_Tool/WHO_Workbook/"
path3 = "~/Library/CloudStorage/Box-Box/GDD/12 MASTER FILES/01 COUNTRY ISO3/"
path4 = "~/Library/CloudStorage/Box-Box/05_THESIS/03_THESIS/00_RESOURCES/01_MASTER_FILES/ISO3/"

# WHO NCD COSTING TOOL COSTS PER COUNTRY

costs <- read_excel(paste0(path2, "NCD_Costing_Tool_Part1_20231005.xlsm"), sheet= "Export_Country_Costs", range = "A1:H194", col_names = T)
costs <- subset(costs, !is.na(Training)) # keep only countries with costs

# COUNTRY NAMES MATCH
master<-read.csv(paste0(path3, "iso3.masterlist.CURRENT.csv")) # for correct country name - relevant for tables w all countries, not so much for the 30most pop
gddcountry <-read.csv(paste0(path3,"iso3.GDD185only.csv")) # for countries included in GDD
iso3.countryorder<-read.csv(paste0(path4,"ISO3_Country_Ranking.csv")) # to match with CRA files cause those only have country_order


#----------------------------------------------------------
# PART 2: MAKE COUNTRY NAMES MATCH 
#-----------------------------------------------------------

# Merge country related files
gddcountry <- subset(gddcountry, select =-c(countryname))
master <- merge(master, gddcountry, by = c("iso3"), all.x = FALSE, all.y = TRUE)

#Add country order
names(iso3.countryorder)[names(iso3.countryorder)=="ISO3"]<-"iso3"
iso3.countryorder<-iso3.countryorder[order(iso3.countryorder$country_order),]
iso3.countryorder <- subset(iso3.countryorder, select = -c(location_name))
iso3.countryorder.master <-merge(master, iso3.countryorder, by = ("iso3"),  all.x = TRUE, all.y = FALSE)
iso3.countryorder.master <- iso3.countryorder.master[, c("iso3", "country", "country_order")]

rm(master, gddcountry, iso3.countryorder)

# Drop countries for which estimates are not available 
iso3.countryorder.master <-subset(iso3.countryorder.master, iso3!="SSD") # does not have GBD information
iso3.countryorder.master <-subset(iso3.countryorder.master, iso3!= "CUB" & iso3!= "PSE" & iso3!= "SYR" & iso3!="VEN") # countries without PPP for 2018 
iso3.countryorder.master <-subset(iso3.countryorder.master, iso3!= "DJI" & iso3!= "ERI" & iso3!= "TWN" ) # countries without complete GDP to calculate the adjusted GDP for 2018
# 177 countries left

# RENAME COUNTRIES IN FROM WHO COSTING TOOL TO GDD NAMES

# Identify names that don't match first
costs <- costs %>%
  dplyr::rename( country = "who_country" )

who <- costs[, c("country")]
no.merge <- anti_join(iso3.countryorder.master, who, by='country') # 25 obsv

# Rename
costs$country[costs$country == "Bahamas"] <-"Bahamas, The"
costs$country[costs$country == "Brunei Darussalam"] <- "Brunei"
costs$country[costs$country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
costs$country[costs$country == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
costs$country[costs$country == "Congo"] <- "Congo, Rep."
costs$country[costs$country == "Egypt"] <- "Egypt, Arab Rep."
costs$country[costs$country == "Ethiopia"] <- "Ethiopia (excludes Eritrea)"
costs$country[costs$country == "Micronesia, Federated States of"] <- "Micronesia, Fed. Sts."
costs$country[costs$country == "Gambia"] <- "Gambia, The"
costs$country[costs$country == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
costs$country[costs$country == "Kyrgyzstan"] <- "Kyrgyz Republic"
costs$country[costs$country == "Republic of Korea"] <- "Korea, Rep."
costs$country[costs$country == "Lao People's  Democratic Republic"] <- "Lao PDR"
costs$country[costs$country == "Libyan Arab Jamahiriya"] <- "Libya"
costs$country[costs$country == "Saint Lucia"] <- "St. Lucia"
costs$country[costs$country == "Republic of Moldova"] <- "Moldova"
costs$country[costs$country == "North Macedonia"] <- "Macedonia, FYR"
costs$country[costs$country == "Slovakia"] <- "Slovak Republic"
costs$country[costs$country == "Eswatini"] <- "Swaziland"
costs$country[costs$country == "Timor Leste"] <- "Timor-Leste"
costs$country[costs$country == "Turkiye"] <- "Turkey"
costs$country[costs$country == "United Republic of Tanzania"] <- "Tanzania"
costs$country[costs$country == "United States of America"] <- "United States"
costs$country[costs$country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
costs$country[costs$country == "Viet Nam"] <- "Vietnam"
costs$country[costs$country == "Yemen"] <- "Yemen, Rep."

who <- costs[, c("country")]
no.merge <- anti_join(iso3.countryorder.master, who, by='country') # 0 obsv
rm(who, no.merge)

# ADD COUNTRY NAME TO BURDEN FILES

# GLOBAL ATTR BURDEN

# CVD
globe.cvd.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.CVD.Incidence.Tax20perc.csv"))[-1]
globe.cvd.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.CVD.Deaths.Tax20perc.csv"))[-1]
globe.cvd.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.CVD.Dalys.Tax20perc.csv"))[-1]
# T2D
globe.t2d.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.T2D.Incidence.Tax20perc.csv"))[-1]
globe.t2d.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.T2D.Deaths.Tax20perc.csv"))[-1]
globe.t2d.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.overall.T2D.Dalys.Tax20perc.csv"))[-1]

# REGIONAL  ATTR BURDEN

# CVD
sr2.cvd.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.CVD.Incidence.Tax20perc.csv"))[-1]
sr2.cvd.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.CVD.Deaths.Tax20perc.csv"))[-1]
sr2.cvd.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.CVD.Dalys.Tax20perc.csv"))[-1]
# T2D
sr2.t2d.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.T2D.Incidence.Tax20perc.csv"))[-1]
sr2.t2d.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.T2D.Deaths.Tax20perc.csv"))[-1]
sr2.t2d.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.superregion2.T2D.Dalys.Tax20perc.csv"))[-1]

# COUNRTY LEVEL  ATTR BURDEN

# CVD
cnty.cvd.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.CVD.Incidence.Tax20perc.csv"))[-1]
cnty.cvd.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.CVD.Deaths.Tax20perc.csv"))[-1]
cnty.cvd.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.CVD.Dalys.Tax20perc.csv"))[-1]
# T2D
cnty.t2d.inci.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.T2D.Incidence.Tax20perc.csv"))[-1]
cnty.t2d.deaths.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.T2D.Deaths.Tax20perc.csv"))[-1]
cnty.t2d.dalys.attr <- read.csv(paste0(path1, "Joint.attr.burden.ss.by.country_order.T2D.Dalys.Tax20perc.csv"))[-1]

# MERGE COUNTRY ATTRB FILES WITH COUNTRY NAMES

list.all.cnty = list(cnty.cvd.inci.attr=cnty.cvd.inci.attr, cnty.cvd.deaths.attr=cnty.cvd.deaths.attr, cnty.cvd.dalys.attr=cnty.cvd.dalys.attr
                     , cnty.t2d.inci.attr=cnty.t2d.inci.attr, cnty.t2d.deaths.attr=cnty.t2d.deaths.attr, cnty.t2d.dalys.attr=cnty.t2d.dalys.attr)

fx_names_cnty <- function(df){
  df <- merge(df, iso3.countryorder.master, by = c("country_order"), all.x = F, all.y = T)
  return(df)
}

list.all.cnty <- lapply(list.all.cnty, fx_names_cnty)
list2env(list.all.cnty, globalenv())

# ADD SUPERREGION NAME TO BURDEN FILES

list.all.sr2 = list( sr2.cvd.inci.attr=sr2.cvd.inci.attr, sr2.cvd.deaths.attr=sr2.cvd.deaths.attr, sr2.cvd.dalys.attr=sr2.cvd.dalys.attr
                    , sr2.t2d.inci.attr=sr2.t2d.inci.attr, sr2.t2d.deaths.attr=sr2.t2d.deaths.attr, sr2.t2d.dalys.attr=sr2.t2d.dalys.attr)

fx_names_sr2 <- function(df){
  
  # Add region labels
  df$region<-"Southeast and East Asia"
  df$region[df$superregion2=="2"]<-"Centr/Eastern Europe and Centr Asia"
  df$region[df$superregion2=="3"]<-"High-Income Countries"
  df$region[df$superregion2=="4"]<-"Latin Amer/Caribbean"
  df$region[df$superregion2=="5"]<-"Mid. East/North Africa"
  df$region[df$superregion2=="6"]<-"South Asia"
  df$region[df$superregion2=="7"]<-"Sub-Saharan Africa"
  
  return(df)
}

list.all.sr2 <- lapply(list.all.sr2, fx_names_sr2)
list2env(list.all.sr2, globalenv())

#----------------------------------------------------------
# PART 3: CALCULATE COST-EFFECTIVENESS
#-----------------------------------------------------------

# SUM COSTS ACROSS ALL CATEGORIES

totalcost <- costs %>%
  mutate(totalcost = rowSums(.[2:8]))

totalcost <- totalcost[, c("country", "totalcost")]

# CALCULATE DISCOUNT RATE

discount.10 <- 0.1*(0.97)^0+0.2*(0.97)^1+0.3*(0.97)^2+0.4*(0.97)^3+0.5*(0.97)^4+0.6*(0.97)^5+0.7*(0.97)^6+0.8*(0.97)^7+0.9*(0.97)^8+1*(0.97)^9
discount.15 <- 0.06666667*(0.97)^0+0.06666667*2*(0.97)^1+0.06666667*3*(0.97)^2+0.06666667*4*(0.97)^3+
               0.06666667*5*(0.97)^4+0.06666667*6*(0.97)^5+0.06666667*7*(0.97)^6+0.06666667*8*(0.97)^7+
               0.06666667*9*(0.97)^8+0.06666667*10*(0.97)^9+0.06666667*11*(0.97)^10+0.06666667*12*(0.97)^11+
               0.06666667*13*(0.97)^12+0.06666667*14*(0.97)^13+0.06666667*15*(0.97)^14


# MERGE COSTS AND BURDENS AND CALCULATE COST-EFFECTIVENESS

list.all.cnty = list(cnty.cvd.inci.attr=cnty.cvd.inci.attr, cnty.cvd.deaths.attr=cnty.cvd.deaths.attr, cnty.cvd.dalys.attr=cnty.cvd.dalys.attr
                     , cnty.t2d.inci.attr=cnty.t2d.inci.attr, cnty.t2d.deaths.attr=cnty.t2d.deaths.attr, cnty.t2d.dalys.attr=cnty.t2d.dalys.attr)

fx_names_cnty <- function(df){
  df <- merge(df, totalcost, by = c("country"), all = T)
  
  df$medians <- df$medians*discount.15
  df$LB <- df$LB*discount.15
  df$UB <- df$UB*discount.15
  
  df$ce_sodatax <- df$totalcost / df$medians## Dividing intervention effect by cost = lives/$

  return(df)
}

list.all.cnty <- lapply(list.all.cnty, fx_names_cnty)
list2env(list.all.cnty, globalenv())


