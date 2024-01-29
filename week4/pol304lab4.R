## Load the data
social <- read_csv("week4/social.csv")
social$messages<-as.factor(social$messages)
levels(social$messages)

## Fit a regression model
fit <- lm(primary2008 ~ messages, data = social)
round(coef(fit),3)

## Create binary indicator variable for each of the 4 categories
social$control<-as.numeric(social$messages=="Control")
social$civic<-as.numeric(social$messages=="Civic Duty")
social$hawthorne<-as.numeric(social$messages=="Hawthorne")
social$neighbors<-as.numeric(social$messages=="Neighbors")
fit1<-lm(primary2008 ~ civic+ hawthorne+ neighbors, data = social)
round(coef(fit1),3)

## Predicted values given the avg outcome under each condition
predict(fit, newdata =  data.frame(messages = 
                                     unique(social$messages)))
tapply(social$primary2008, social$messages, mean)

## Question 1
social$age <- 2008 - social$yearofbirth

## Question 2



## Question 3

## Question 4

## Question 5
