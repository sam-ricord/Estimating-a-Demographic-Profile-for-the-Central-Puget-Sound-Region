rm(list = ls())
setwd("~/UW/Research Project/Transportation Big Data Equity/Second Study - Freeway Netowrk")
library(ecoreg)
library(tidyverse)
library(lme4)
library(leaflet)
library(sf)

#import data

Individual_Trips <- read.table(file = '_trip.tsv', sep = '\t', header = TRUE)
Individual_Trips.df <- data.frame(Individual_Trips)

OD_raw <- read.delim("PSRC_OD Demand.txt", header = TRUE, sep = "")
OD_raw.df <- data.frame(OD_raw)
colnames(OD_raw) <- c('TAZ_O', 'TAZ_D', 'trips')
colnames(OD_raw.df) <- c('TAZ_O', 'TAZ_D', 'trips')

HHTS_Household <- read.csv(file = 'HHTS_Household.csv', header = TRUE, stringsAsFactors = FALSE)
HHTS_Person <- read.csv(file = 'HHTS_Person.csv', header = TRUE, stringsAsFactors = FALSE)
HHTS_Trip <- read.csv(file = 'HHTS_Trip.csv', header = TRUE, stringsAsFactors = FALSE)

TAZ_HHDemographics <- read.csv(file = 'luv2-lodes_TAZ.csv', header = TRUE, stringsAsFactors = FALSE)
names(TAZ_HHDemographics)[1] <- "County"

#Origin Only (non-individual data)

TAZ_O <- 1
Trips_O <- 0
Trips_all <- c()

for(i in 1:length(OD_raw.df$TAZ_O)) {
  if (OD_raw.df$TAZ_O[i] == TAZ_O) { 
    Trips_O <- Trips_O + OD_raw.df$trips[i]
  } else {
    Trips_all <- c(Trips_all, Trips_O)
    TAZ_O <- TAZ_O + 1
    Trips_O <- 0
  }
}

OriginOnly.df <- data.frame(Trips_all)

#Full O/D Matrix (non-individual data)

OD_Matrix <- matrix(nrow = max(OD_raw.df$TAZ_O), ncol = max(OD_raw.df$TAZ_D))

for(i in 1:length(OD_raw.df$TAZ_O)) {
  OD_Matrix[OD_raw.df$TAZ_O[i],OD_raw.df$TAZ_D[i]] <- OD_raw.df$trips[i]
}

for(i in 1:length(OD_Matrix[1,])) {
  for(j in 1:length(OD_Matrix[,1])){
    if(is.na(OD_Matrix[i,j])) {
      OD_Matrix[i,j] <- 0
    }
  }
}

#OD Matrices that use the individual trip data

HHTS_Trip$speed_mph[which(is.na(HHTS_Trip$speed_mph))] <- 0
HHTS_Trip$speed_mph[which(HHTS_Trip$speed_mph > 80)] <- 80
HHTS_speed_sd <- sd(HHTS_Trip$speed_mph[which(HHTS_Trip$mode_simple == "Drive")])
HHTS_speed_av <- quantile(HHTS_Trip$speed_mph, c(.4, .45, .6))
HHTS_speed_margin <- qt(0.975, df = length(HHTS_Trip$speed_mph) - 1) * (HHTS_speed_sd / sqrt(length(HHTS_Trip$speed_mph)))
speed_conf <- HHTS_speed_av - HHTS_speed_margin

Individual_OD_Matrix <- matrix(0, nrow = max(Individual_Trips.df$otaz), ncol = max(Individual_Trips.df$dtaz))

for(i in 1:length(Individual_Trips.df$otaz)) {
  Individual_OD_Matrix[Individual_Trips.df$otaz[i], Individual_Trips.df$dtaz[i]] <- Individual_OD_Matrix[Individual_Trips.df$otaz[i], Individual_Trips.df$dtaz[i]] + 1
}

Individual_Freeway_OD_Matrix <- matrix(0, nrow = max(Individual_Trips.df$otaz), ncol = max(Individual_Trips.df$dtaz))

for(i in 1:length(Individual_Trips.df$otaz)) {
  if(Individual_Trips.df$mode[i] == 3 | Individual_Trips.df$mode[i] == 4 | Individual_Trips.df$mode[i] == 5) { #don't know the code for mode ## Individual_Trips.df$mode[i] == 1 | Individual_Trips.df$mode[i] == 2
  #if(Individual_Trips.df$mode[i] == 1 | Individual_Trips.df$mode[i] == 2) {  
    if(Individual_Trips.df$travdist[i] != 0 && Individual_Trips.df$travtime[i] != 0) {
      if(((Individual_Trips.df$travdist[i]/Individual_Trips.df$travtime[i])*60) >= min(speed_conf)) { #average speed estimate
        Individual_Freeway_OD_Matrix[Individual_Trips.df$otaz[i], Individual_Trips.df$dtaz[i]] <- Individual_Freeway_OD_Matrix[Individual_Trips.df$otaz[i], Individual_Trips.df$dtaz[i]] + 1
      }
    }
  }
}

#County Incomes

income_king <- read.csv(file = 'king_income.csv', header = TRUE, stringsAsFactors = FALSE)
income_kitsap <- read.csv(file = 'kitsap_income.csv', header = TRUE, stringsAsFactors = FALSE)
income_pierce <- read.csv(file = 'pierce_income.csv', header = TRUE, stringsAsFactors = FALSE)
income_snohomish <- read.csv(file = 'snohomish_income.csv', header = TRUE, stringsAsFactors = FALSE)

names(income_king)[1] <- "variable"
names(income_kitsap)[1] <- "variable"
names(income_pierce)[1] <- "variable"
names(income_snohomish)[1] <- "variable"

income_brackets_king <- income_king %>% slice(51:61)
income_brackets_kitsap <- income_kitsap %>% slice(51:61)
income_brackets_pierce <- income_pierce %>% slice(51:61)
income_brackets_snohomish <- income_snohomish %>% slice(51:61)

income_brackets_king$Estimate <- c(882028, 37556, 22534, 44052, 48493, 76593, 124915, 107203, 169168, 98984, 152530)
income_brackets_kitsap$Estimate <- c(103913, 4464, 2996, 6611, 7029, 11462, 19143, 15972, 20377, 7993, 7866)
income_brackets_pierce$Estimate <- c(323296, 14681, 9278, 20706, 24208, 38513, 61120, 47617, 61191, 24913, 21069)
income_brackets_snohomish$Estimate <- c(293823, 9883, 7109, 15742, 16222, 27686, 48604, 44712, 62537, 31983, 29345)

percent_king <- c()
percent_kitsap <- c()
percent_pierce <- c()
percent_snohomish <- c()
for(i in 1:length(income_brackets_king$Estimate)) {
  percent_king <- c(percent_king, income_brackets_king$Estimate[i]/income_brackets_king$Estimate[1])
  percent_kitsap <- c(percent_kitsap, income_brackets_kitsap$Estimate[i]/income_brackets_kitsap$Estimate[1])
  percent_pierce <- c(percent_pierce, income_brackets_pierce$Estimate[i]/income_brackets_pierce$Estimate[1])
  percent_snohomish <- c(percent_snohomish, income_brackets_snohomish$Estimate[i]/income_brackets_snohomish$Estimate[1])
}

income_brackets_king$percentage <- percent_king
income_brackets_kitsap$percentage <- percent_kitsap
income_brackets_pierce$percentage <- percent_pierce
income_brackets_snohomish$percentage <- percent_snohomish

income_brackets_all <- data.frame(income_brackets_king$variable)
names(income_brackets_all)[1] <- "variable"

income_brackets_all$king_pop <- income_brackets_king$Estimate
income_brackets_all$kitsap_pop <- income_brackets_kitsap$Estimate
income_brackets_all$pierce_pop <- income_brackets_pierce$Estimate
income_brackets_all$snohomish_pop <- income_brackets_snohomish$Estimate

income_brackets_all$king_percentage <- income_brackets_king$percentage
income_brackets_all$kitsap_percentage <- income_brackets_kitsap$percentage
income_brackets_all$pierce_percentage <- income_brackets_pierce$percentage
income_brackets_all$snohomish_percentage <- income_brackets_snohomish$percentage

income_brackets_all$all_pop <- numeric(length(income_brackets_all$variable))
for (i in 1:length(income_brackets_all$variable)) {
  income_brackets_all$all_pop[i] <- income_brackets_all$king_pop[i] + income_brackets_all$kitsap_pop[i] + income_brackets_all$pierce_pop[i] + income_brackets_all$snohomish_pop[i]
}
income_brackets_all$all_percentage <- numeric(length(income_brackets_all$variable))
for (i in 1:length(income_brackets_all$variable)) {
  income_brackets_all$all_percentage[i] <- income_brackets_all$all_pop[i]/income_brackets_all$all_pop[1]
}

#initialize and fill ecoreg dataframe

income_all <- data.frame(matrix(0, nrow = length(Individual_OD_Matrix[,1]), ncol = 12))
names(income_all) <- c("Trips", "HW_Trips", "k10", "k15", "k25", "k35", "k50", "k75", "k100", "k150", "k200", "k201")

for(i in 1:length(Individual_OD_Matrix[,1])) {
  income_all$Trips[i] <- sum(Individual_OD_Matrix[i,])
  income_all$HW_Trips[i] <- sum(Individual_Freeway_OD_Matrix[i,])
  if (i <= length(TAZ_HHDemographics$County)) {
    if(TAZ_HHDemographics$County[i] == "King") { 
      for(j in 2:length(income_brackets_king$Estimate)) {
        income_all[i, j+1] <- income_brackets_king$percentage[j]
      }
    } else if(TAZ_HHDemographics$County[i] == "Kitsap") { 
      for(j in 2:length(income_brackets_kitsap$Estimate)) {
        income_all[i, j+1] <- income_brackets_kitsap$percentage[j]
      }
    } else if(TAZ_HHDemographics$County[i] == "Pierce") { 
      for(j in 2:length(income_brackets_pierce$Estimate)) {
        income_all[i, j+1] <- income_brackets_pierce$percentage[j]
      }
    } else if(TAZ_HHDemographics$County[i] == "Snohomish") { 
      for(j in 2:length(income_brackets_snohomish$Estimate)) {
        income_all[i, j+1] <- income_brackets_snohomish$percentage[j]
      }
    } else {
      print(i)
    }
  } else {
    for(j in 2:length(income_brackets_kitsap$Estimate)) { #pick county
      income_all[i, j+1] <- income_brackets_kitsap$percentage[j]
    }
  }
}

income_all <- income_all %>% add_column(households = 0)
for(i in 1:length(income_all$households)) {
  if(i <= length(TAZ_HHDemographics$households_2014)) {
    income_all$households[i] <- TAZ_HHDemographics$households_2014[i]
  } else {
    income_all$households[i] <- income_all$Trips[i] * (sum(TAZ_HHDemographics$households_2014)/sum(income_all$Trips))
  }
}

income_all <- income_all %>% add_column(k10_households = 0)
income_all <- income_all %>% add_column(k15_households = 0)
income_all <- income_all %>% add_column(k25_households = 0)
income_all <- income_all %>% add_column(k35_households = 0)
income_all <- income_all %>% add_column(k50_households = 0)
income_all <- income_all %>% add_column(k75_households = 0)
income_all <- income_all %>% add_column(k100_households = 0)
income_all <- income_all %>% add_column(k150_households = 0)
income_all <- income_all %>% add_column(k200_households = 0)
income_all <- income_all %>% add_column(k201_households = 0)
income_all <- income_all %>% add_column(k10_trips = 0)
income_all <- income_all %>% add_column(k15_trips = 0)
income_all <- income_all %>% add_column(k25_trips = 0)
income_all <- income_all %>% add_column(k35_trips = 0)
income_all <- income_all %>% add_column(k50_trips = 0)
income_all <- income_all %>% add_column(k75_trips = 0)
income_all <- income_all %>% add_column(k100_trips = 0)
income_all <- income_all %>% add_column(k150_trips = 0)
income_all <- income_all %>% add_column(k200_trips = 0)
income_all <- income_all %>% add_column(k201_trips = 0)
for(i in 1:length(income_all$Trips)) {
  for(j in 3:12) {
    paste(i,j)
    income_all[i,j+11] <- income_all[i,j]*income_all$households[i]
    income_all[i,j+21] <- income_all[i,j]*income_all$Trips[i]
  }
}

zerotrip_TAZ <- c()
for(i in 1:length(income_all$Trips)) {
  if(income_all$Trips[i] == 0) {
    zerotrip_TAZ <- c(zerotrip_TAZ, i)
  }
}
income_all_nonzero <- income_all[-c(zerotrip_TAZ),]

#Create Ecological Regression

#income.eco.1 <- eco(cbind(HW_Trips, Trips) ~ k10 + k15 + k25 + k35 + k50 + k75 + k100 + k150 + k200 + k201, data = income_all)
#income.eco.1

##income.eco.2 <- eco(cbind(HW_Trips, Trips) ~ k10_households + k15_households + k25_households + k35_households + k50_households + k75_households + k100_households + k150_households + k200_households + k201_households, data = income_all)
#income.eco.2

#income.eco.3 <- eco(cbind(HW_Trips, Trips) ~ k10_trips + k15_trips + k25_trips + k35_trips + k50_trips + k75_trips + k100_trips + k150_trips + k200_trips + k201_trips, data = income_all)
#income.eco.3

income.eco.4 <- eco(cbind(HW_Trips, Trips) ~ k10 + k15 + k25 + k35 + k50 + k75 + k100 + k150 + k200 + k201, data = income_all_nonzero)
income.eco.4

income.eco.5 <- eco(cbind(HW_Trips, Trips) ~ k10_households + k15_households + k25_households + k35_households + k50_households + k75_households + k100_households + k150_households + k200_households + k201_households, data = income_all_nonzero)
income.eco.5

income.eco.6 <- eco(cbind(HW_Trips, Trips) ~ k10_trips + k15_trips + k25_trips + k35_trips + k50_trips + k75_trips + k100_trips + k150_trips + k200_trips + k201_trips, data = income_all_nonzero)
income.eco.6

#Sensitivity Analysis
income_brackets_all$eco_5 <- income.eco.5$ors.ctx * income_brackets_all$all_pop #eco_5 and eco_6 are total populations
income_brackets_all$eco_6 <- income.eco.6$ors.ctx * income_brackets_all$all_pop #eco_5 and eco_6 are total populations

totalpercent_5 <- c(0)
totalpercent_6 <- c(0)
for (i in 2:length(income_brackets_all$variable)) {
  totalpercent_5 <- c(totalpercent_5, income_brackets_all$eco_5[i]/sum(income_brackets_all$eco_5[2:length(income_brackets_all$eco_5)]))
  totalpercent_6 <- c(totalpercent_6, income_brackets_all$eco_6[i]/sum(income_brackets_all$eco_6[2:length(income_brackets_all$eco_6)]))
}

income_brackets_all$totalpercent_5 <- totalpercent_5
income_brackets_all$totalpercent_6 <- totalpercent_6

#weighted percent A based on population, B based on percents
income_brackets_all$percentchange_A_5 <- (income_brackets_all$all_pop - income_brackets_all$eco_5) / income_brackets_all$all_pop
income_brackets_all$percentchange_A_6 <- (income_brackets_all$all_pop - income_brackets_all$eco_6) / income_brackets_all$all_pop

income_brackets_all$percentchange_A_5 <- abs(income_brackets_all$percentchange_A_5)
income_brackets_all$percentchange_A_6 <- abs(income_brackets_all$percentchange_A_6)

income_brackets_all$percentchange_B_5 <- (income_brackets_all$all_percentage - income_brackets_all$totalpercent_5) / income_brackets_all$all_percentage
income_brackets_all$percentchange_B_6 <- (income_brackets_all$all_percentage - income_brackets_all$totalpercent_6) / income_brackets_all$all_percentage

income_brackets_all$percentchange_B_5 <- abs(income_brackets_all$percentchange_B_5)
income_brackets_all$percentchange_B_6 <- abs(income_brackets_all$percentchange_B_6)

weighted_percentchange_A_5 <- 0
weighted_percentchange_A_6 <- 0
weighted_percentchange_B_5 <- 0
weighted_percentchange_B_6 <- 0
for (i in 2:length(income_brackets_all$variable)) {
  weighted_percentchange_A_5 <- weighted_percentchange_A_5 + (income_brackets_all$all_pop[i] * income_brackets_all$percentchange_A_5[i])
  weighted_percentchange_A_6 <- weighted_percentchange_A_6 + (income_brackets_all$all_pop[i] * income_brackets_all$percentchange_A_6[i])
  weighted_percentchange_B_5 <- weighted_percentchange_B_5 + (income_brackets_all$all_pop[i] * income_brackets_all$percentchange_B_5[i])
  weighted_percentchange_B_6 <- weighted_percentchange_B_6 + (income_brackets_all$all_pop[i] * income_brackets_all$percentchange_B_6[i])
}

weighted_percentchange_A_5 <- weighted_percentchange_A_5/income_brackets_all$all_pop[1]
weighted_percentchange_A_6 <- weighted_percentchange_A_6/income_brackets_all$all_pop[1]
weighted_percentchange_B_5 <- weighted_percentchange_B_5/income_brackets_all$all_pop[1]
weighted_percentchange_B_6 <- weighted_percentchange_B_6/income_brackets_all$all_pop[1]

weighted_percentchange_A_5
weighted_percentchange_A_6
weighted_percentchange_B_5
weighted_percentchange_B_6

#sum(Individual_Freeway_OD_Matrix)
#sum(Individual_OD_Matrix)
income_brackets_all$expectedtrips_5 <- income_brackets_all$totalpercent_5 * sum(Individual_Freeway_OD_Matrix)
income_brackets_all$expectedtrips_6 <- income_brackets_all$totalpercent_6 * sum(Individual_Freeway_OD_Matrix)
















