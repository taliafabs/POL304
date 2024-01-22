## pol304 week 1 r script
## talia fabregas

## open the file
rosca <- read.csv('rosca.csv', header=TRUE)

## access or create a new variable
rosca$bg_female
rosca$new_var<- NA
## see how many participants that saved at least 100 dollars
hundred_plus <- sum(rosca$fol2_amtinvest >= 100)

## subset rows (observations) by some value value
rosca$fol2_amtinvest[rosca$bg_female==1]

## or create a copy of the dataset that only includes women:
rosca1<- rosca$fol2_amtinvest[rosca$bg_female==1]

## calculate the mean
mean(rosca$fol2_amtinvest, na.rm=TRUE)
mean(rosca$fol2_amtinvest[rosca$bg_female==1], na.rm=TRUE)
tapply(rosca$fol2_amtinvest, rosca$bg_female,mean, na.rm=TRUE)

## table observations by category
table(rosca$bg_female)

## exercises
rosca$treatment <- ifelse(rosca$encouragement == 1, "control",
                          ifelse(rosca$safe_box == 1, "safebox",
                                 ifelse(rosca$locked_box == 1, "lockbox", NA)))

## the number of individuals in each group
control_count <- sum(rosca$treatment == "control", na.rm = TRUE)
safebox_count <- sum(rosca$treatment == "safebox", na.rm = TRUE)
lockbox_count <- sum(rosca$treatment == "lockbox", na.rm = TRUE)

## extra practice

## see how many participants that saved at least 100 dollars
hundred_plus <- sum(rosca$fol2_amtinvest >= 100)


