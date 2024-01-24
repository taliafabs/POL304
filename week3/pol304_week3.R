## load packages
## can use dplyr and janitor to clean data as well
library(tidyverse)
library(dplyr)
library(janitor)
library(stats)

## load the data
vig <- read.csv("week3/vignettes.csv")

# Categorize China and Mexico
vig_chi <- subset(vig, china == 1)
vig_mex <- subset(vig, china == 0)

# China
chi_table <- prop.table(table(vig_chi$self))
barplot(chi_table, main = "China")

# Our first look at the data indicates potential response bias in China.


# Mexico
mex_table <- prop.table(table(vig_mex$self))
barplot(mex_table, main = "Mexico")

# Compute the proportion of respondents from China and Mexico who rank
# themselves as having less say in the government's decisions than Moses
# create a binary variable
# mean of the binary variable will tell us the proportion of respondents who 
# think they have less say than moses.

# China
vig_chi$less_than_moses <- ifelse(vig_chi$self < vig_chi$moses, 1, 0)
mean(vig_chi$less_than_moses)
# 56% of respondents in China think they have less power than Moses

# Mexico
vig_mex$less_than_moses <- ifelse(vig_mex$self < vig_mex$moses, 1, 0)
mean(vig_mex$less_than_moses)
# 24.9% of respondents in Mexico think they have less power than Moses
# Inititally, many thought they had little power, but less than a quarter
# of respondents from Mexico thought they had less power than Moses

# restrict the data to people who ranked these three vignettes in order:
# Alison ≥ Jane ≥ Moses

# China
vig_chi <- subset(vig_chi, vig_chi$alison >= vig_chi$jane &
                          vig_chi$jane >= vig_chi$moses)

# Mexico
vig_mex <- subset(vig_mex, vig_mex$alison >= vig_mex$jane &
                          vig_mex$jane >= vig_mex$moses)

# rank variable

# China
vig_chi$rank <- NA
# 1
vig_chi$rank <- ifelse(vig_chi$self < vig_chi$moses, 1, vig_chi$rank)
# 2
vig_chi$rank <- ifelse(vig_chi$self >= vig_chi$moses & 
                         vig_chi$self < vig_chi$jane, 2, vig_chi$rank)
# 3
vig_chi$rank <- ifelse(vig_chi$self >= vig_chi$jane & 
                         vig_chi$self < vig_chi$alison, 
                       3, vig_chi$rank)
# 4
vig_chi$rank <- ifelse(vig_chi$self >= vig_chi$alison, 4, vig_chi$rank) 

# Mexico
vig_mex$rank <- NA
# 1
vig_mex$rank <- ifelse(vig_mex$self < vig_mex$moses, 1, vig_mex$rank)
# 2
vig_mex$rank <- ifelse(vig_mex$self >= vig_mex$moses & 
                         vig_mex$self < vig_mex$jane, 2, vig_mex$rank)
# 3
vig_mex$rank <- ifelse(vig_mex$self >= vig_mex$jane & 
                         vig_mex$self < vig_mex$alison, 
                       3, vig_mex$rank)
# 4
vig_mex$rank <- ifelse(vig_mex$self >= vig_mex$alison, 4, vig_mex$rank) 

## Create the barplots of this new variable 

# China
chi_table2 <- prop.table(table(vig_chi$rank))
barplot(chi_table2, main = "China")

# Mexico
mex_table2 <- prop.table(table(vig_mex$rank))
barplot(mex_table2, main = "Mexico")

