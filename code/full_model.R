# Author: Kate Burrows
# Date: Sep. 2, 2022
# Purpose: model used in analysis for 
#       Burrows et al. (submitted to Nature Communications)

# NOTE: this code is a guide for transparency and 
#       reproducability and is not able to be run

#packages used in this model
library("dlnm")
library("dplyr")
library("lubridate")
library("purrr")
library("lme4")

#1. Full model formula
mixed_effect_model <- function(data_to_mod){
        m<- lme4::glmer(cases ~
                  # exposure slopes by lag day (lag0 is day of storm, lag_neg1 is day before, etc.)
                  lag_neg2 + lag_neg1 + lag0 + lag1 + lag2 + lag3 + lag4 + lag5 + lag6 + lag7 +
                  
                  # random intercept to account for variation at the ZCTA level
                  (1|zcta) +
                          
                  # indicator to adjust for year and day of week
                  year + dow,
                  
                  # log offset to account for changes in Medicare population
                  offset = log(denom), data=data_to_mod,
          # poisson model
          family=poisson(link = "log"))
        return(m)
}

full_model<-mixed_effect_model(df="data_to_model.rds")


# 2. ZCTA-level stratification 
        # variable list:
        # hd.binary: Houshold density
        # owned.binary: Owner occupied units
        # value.binary: Housing value
        # pov.binary: Poverty
        # household.binary: Income
        # BA.binary: Bachelors degree
        # ne.binary: Non-English speakers
        # density.binary: Urban density
        # percent.urban.binary: Percent Urban
        # proportion_binary: Percent Black

# identify whether zips fall above or below median for a given variable
stratifyZCTA<-function(var, strata){
        list<-readRDS(file="census_data/census_data.rds") %>% 
                filter({{var}} == strata) 
        return(list$zcta)
}

zcta_list<-stratifyZCTA(BA.binary, 1)

data_to_mod<- full_data %>% filter(zcta %in% zcta_list)

stratified_model<-mixed_effect_model(data_to_mod)

