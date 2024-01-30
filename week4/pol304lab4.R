## Load the data
social <- read_csv("week4/social.csv")
social$messages<-as.factor(social$messages)
levels(social$messages)

## Fit a regression model
fit <- lm(primary2008 ~ messages, data = social)
coef(fit)
round(coef(fit),3)

## Create binary indicator variable for each of the 4 categories
## alternatively, as.numeric can be used instead of ifelse
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
## create variable for age in 2008
social$age <- 2008 - social$yearofbirth

## Question 2
fit_age <- lm(primary2008 ~ civic + neighbors + hawthorne + age, data=social)
coef(fit_age)
round(coef(fit_age),3)


## Question 3
## no, the effect of each treatment did not change

## Question 4
## a one-year increase in age is associated w a 0.4 percentage point increase
## in turnout

## Question 5
## age 18
new_data18 =data.frame(civic=0, 
                    neighbors=0, 
                    hawthorne=0, 
                    age=18)
predict(fit_age, new_data18)
## 15.6% predicted turnout

## age 40
new_data40 =data.frame(civic=0, 
                       neighbors=0, 
                       hawthorne=0, 
                       age=40)
predict(fit_age, new_data40)
## 24.76% predicted turnout