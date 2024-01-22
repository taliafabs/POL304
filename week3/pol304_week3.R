## load packages
library(tidyverse)
library(dplyr)
library(janitor)

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