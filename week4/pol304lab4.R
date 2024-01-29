## Load the data
social <- read_csv("week4/social.csv")
social$messages<-as.factor(social$messages)
levels(social$messages)

## Fit a regression model
fit <- lm(primary2008 ~ messages, data = social)
coef(fit)
round(coef(fit),3)

## Create binary indicator variable for each of the 4 categories
social$control <- ifelse(social$messages=="Control", 1, 0)
social$civic <- ifelse(social$messages=="Civic Duty", 1, 0)
social$hawthorne <- ifelse(social$messages=="Hawthorne", 1, 0)
social$neighbors <- ifelse(social$messages=="Neighbors", 1, 0)
head(social)
fit1<-lm(primary2008 ~ civic + neighbors + hawthorne, data = social)
coef(fit1)
round(coef(fit1),3)
## here we omit the control category
## control group (no message) turnout rate was 29.7%
## went up by the slope for each of the intercepts
## civic associated w 1.8 percentage point increase
## neighbors associated w 8.1 percent point increase
## hawthorne associated w 2.6 percentage point increase

## Question 1
social$age <- 2008 - social$yearofbirth

## Question 2



## Question 3

## Question 4

## Question 5
