#Format WildTrax report for occupancy modelling using the unmarked package 
library(dplyr)
library(unmarked)
library(tidyverse)
library(lubridate)

data <- read.csv("0_data/Raw/1_RUGR_LAB_PROJECT_basic_summary.csv")

# Getting site covariates
stand_covariates <- read.csv("0_data/Raw/DATA/ABMIpoints_Beaudoin.hardbuffers.150.565-sf-terra.csv")
# Filter for only the columns i want 
siteCovs <- select(stand_covariates, c('location', 'YEAR', 'longitude', 'latitude',
                                       'LandCover_NonVeg_v1.565m', 'LandCover_Veg_v1.565m',
                                       'LandCover_VegNonTreed_v1.565m', 'LandCover_VegTreed_v1.565m'))

# Only keeping the rows with sites that are used in analysis
siteCovs <- stand_covars %>% 
  filter(!duplicated(location))
siteCovs <- na.omit(siteCovs)

# Scale/standardize your variables before you add site cover data to unmarked-data-frame
# Stand age within 565m buffer radius of point 
#siteCovs$Structure_Stand_Age_v1.565m <-ifelse(siteCovs$Structure_Stand_Age_v1.565m==0,1,siteCovs$Structure_Stand_Age_v1.565m)
#siteCovs$Structure_Stand_Age_v1.565m <-siteCovs$Structure_Stand_Age_v1.565m/max(siteCovs$Structure_Stand_Age_v1.565m)
#range(siteCovs$Structure_Stand_Age_v1.565m) # just check to make sure range is 0-1

# NonVeg Land Cover 
siteCovs$LandCover_NonVeg_v1.565m <- ifelse(siteCovs$LandCover_NonVeg_v1.565m==0,1,siteCovs$LandCover_NonVeg_v1.565m)
siteCovs$LandCover_NonVeg_v1.565m <- siteCovs$LandCover_NonVeg_v1.565m/max(siteCovs$LandCover_NonVeg_v1.565m)
range(siteCovs$LandCover_NonVeg_v1.565m)

# Veg Land Cover 
siteCovs$LandCover_Veg_v1.565m <- ifelse(siteCovs$LandCover_Veg_v1.565m==0,1,siteCovs$LandCover_Veg_v1.565m)
siteCovs$LandCover_Veg_v1.565m <- siteCovs$LandCover_Veg_v1.565m/max(siteCovs$LandCover_Veg_v1.565m)
range(siteCovs$LandCover_Veg_v1.565m)

# Land cover veg non treed 
siteCovs$LandCover_VegNonTreed_v1.565m <- ifelse(siteCovs$LandCover_VegNonTreed_v1.565m==0,1, siteCovs$LandCover_VegNonTreed_v1.565m)
siteCovs$LandCover_VegNonTreed_v1.565m <- siteCovs$LandCover_VegNonTreed_v1.565m/max(siteCovs$LandCover_VegNonTreed_v1.565m)
range(siteCovs$LandCover_VegNonTreed_v1.565m)

# Land cover veg treed 
siteCovs$LandCover_VegTreed_v1.565m <- ifelse(siteCovs$LandCover_VegTreed_v1.565m==0,1, siteCovs$LandCover_VegTreed_v1.565m)
siteCovs$LandCover_VegTreed_v1.565m <- siteCovs$LandCover_VegTreed_v1.565m/max(siteCovs$LandCover_VegTreed_v1.565m)
range(siteCovs$LandCover_VegTreed_v1.565m)

# remove locatiosn from data that there are no site covariates for by merging based on location names 
#data <- semi_join(data, siteCovs, by = "location")
data <- merge(data, siteCovs, by = "location")
siteCovs <- select(data, c('location', 'longitude.x', 'latitude.x',
                                       'LandCover_NonVeg_v1.565m', 'LandCover_Veg_v1.565m',
                                       'LandCover_VegNonTreed_v1.565m', 'LandCover_VegTreed_v1.565m'))
siteCovs <- siteCovs %>% filter(!duplicated(location))
# Do the thing!
#wt_format_occupancy <- function(data,
#                                species,
#                                siteCovs)
  
#Wrangle observations and observation covariates for the species of interest
#this is chats code 
visits <- data %>%
  mutate(occur = ifelse(species_code == "RUGR", 1, 0)) %>%
  dplyr::select(location, recording_date, observer, method, occur) %>%
 unique() %>%
  mutate(recording_date = as.POSIXct(recording_date),
         year = format(recording_date, format = "%Y"),
         doy = as.numeric(format(recording_date, format = "%j")),
         hr = as.numeric(format(recording_date, format = "%H"))) %>%
  group_by(location) %>%
  arrange(recording_date) %>%
  mutate(visit = row_number()) %>%
  ungroup()


#Create location X recording dataframe of observations (1 for detected, 0 for undetected)
visits$occur <- as.integer(visits$occur)
str(visits$occur)
   y <- visits %>%
    dplyr::select(location, visit, occur) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = occur) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  
#Create location X recording dataframes for observation covariates (doy = day of year, hr = hour of day, method = processing method, observer = observer ID)
   visits$doy <- as.integer(visits$doy)
   str(visits$doy)
   doy <- visits %>%
    dplyr::select(location, visit, doy) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = doy) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
  doy2 <- visits %>%
    mutate(doy2 = doy^2) %>%
    dplyr::select(location, visit, doy2) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = doy2) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
  visits$hr <- as.integer(visits$hr)
  hr <- visits %>%
    dplyr::select(location, visit, hr) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = hr) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
  hr2 <- visits %>%
    mutate(hr2 = hr^2) %>%
    dplyr::select(location, visit, hr2) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = hr2) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
  method <- visits %>%
    dplyr::select(location, visit, method) %>%
    mutate(method = as.factor(method)) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = method) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
  observer <- visits %>%
    dplyr::select(location, visit, observer) %>%
    mutate(observer = as.factor(observer)) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = observer) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()
  
#Create a list of the observation covariates
obsCovs <- list(doy=doy, doy2=doy2, hr=hr, hr2 = hr2, method=method, observer=observer)
 
  
#Put together as an unmarked object for single species occupancy models
# Run this line if you dont have site covariates 
umf <- unmarkedFrameOccu(y=y, siteCovs=NULL, obsCovs=obsCovs)

# This one if you have covs 
umf <- unmarkedFrameOccu(y=y, siteCovs=siteCovs, obsCovs=obsCovs)


#Prepare the data to run the occupancy models using only one visit to each site 
one_visit_data <- visits %>%
  group_by(location) %>%
  sample_n(size = 1) %>%
  ungroup()

#Make new site covs data for just the random subset of visits to each location 
one_vis <- merge(one_visit_data, siteCovs, by = "location")
siteCovs1visit <- select(one_vis, c('location', 'longitude.x', 'latitude.x',
                           'LandCover_NonVeg_v1.565m', 'LandCover_Veg_v1.565m',
                           'LandCover_VegNonTreed_v1.565m', 'LandCover_VegTreed_v1.565m'))

#Create a list of the observation covariates for the one visit 
obsCovs1visit <- list(doy=doy, doy2=doy2, hr=hr, hr2 = hr2, method=method, observer=observer)

#Put together as an unmarked object for single species occupancy models

# This one if you have covs 
umf1visit <- unmarkedFrameOccu(y=y, siteCovs=siteCovs1visit, obsCovs=obsCovs1visit)

