#### Preamble ####
# Purpose: POL304 Week 9 R Script
# Author: Talia Fabregas
# Date: 11 March 2024
# Contact: talia.fabregas@mail.utoronto.ca
# License: n/a
# Pre-requisites: 
# - download and save GTD_data_small.csv and save it to week9
# - add it to gitignore to prevent it from being pushed onto github
# Any other information needed? n/a

#### Workplace Setup ####
library(tidyverse)
library(dplyr)
library(data.table)

#### Code ####
mydata <- fread("week9/GTD_data_small.csv", header=TRUE)
cor(mydata$nkill,mydata$iyear, use="complete.obs")

data1 <- mydata %>%
  select_if(is.numeric) %>%
  select(nkill,)

correlation_matrix <- round(abs(cor(data1, use = "pairwise.complete.obs")), 2)
correlation_matrix <- as.data.frame(correlation_matrix) %>%
  arrange(desc(nkill))
row.names(correlation_matrix)[1:11]

  
# get a correlation matrix
correlation_matrix <- cor(mydata)

# find the top 10 correlations with nkill
correlation_with_nkill <- sort(correlation_matrix["nkill", ], decreasing = TRUE)
top_10_correlated <- head(correlation_with_nkill, 10)

# get the variables
data2 <- mydata %>%
  select(nkill,
         nwound,
         nkillus,
         nkillter,
         nhostkid,
         nreleased,
         nwoundte,
         suicide)

# regression model for nkill with the highest correlated variables
model <- lm(nkill  ~ nwound + nkillus + nkillter + nhostkid + nreleased +
               nwoundte + suicide, 
             data = data2)

# find the R^2
summary(model)
r_squared <- summary(model)$r.squared
# the R^2 is 0.8190756
# this means that 81.9% of the variance in nkill is explained by the variables 
# included in the regression model