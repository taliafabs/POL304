## POL304 Week 5 Tutorial

## load packages
## install.packages("ggplot2")
library(ggplot2)

MP_data <- read_csv("week5/MPs.csv")
MPs_labour <- subset(MP_data, subset = (party == "labour"))
MPs_tory <- subset(MP_data, subset = (party == "tory"))

## regressions for Labour
## two regressions for Labour: negative and positive margin
labour_fit1 <- lm(ln.net ~ margin,
                  data = MPs_labour[MPs_labour$margin < 0, ])
labour_fit2 <- lm(ln.net ~ margin,
                  data = MPs_labour[MPs_labour$margin > 0, ])

## two regressions for Tory: negative and positive margin
tory_fit1 <- lm(ln.net ~ margin, data = MPs_tory[MPs_tory$margin < 0, ])
tory_fit2 <- lm(ln.net ~ margin, data = MPs_tory[MPs_tory$margin > 0, ])

## Scatterplot with regression lines for tory
plot(MPs_tory$margin, MPs_tory$ln.net, main = "Tory", xlim = c(-0.5, 0.5),
     ylim = c(6, 18), xlab = "Margin of victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")
## add regression lines
## Tory: range of predictions
y1t.range <- c(min(MPs_tory$margin), 0) # min to 0
y2t.range <- c(0, max(MPs_tory$margin)) # 0 to max
## predict outcome
y1.tory <- predict(tory_fit1, newdata = data.frame(margin = y1t.range))
y2.tory <- predict(tory_fit2, newdata = data.frame(margin = y2t.range))
lines(y1t.range, y1.tory, col = "blue")
lines(y2t.range, y2.tory, col = "blue")

## Government Transfers and Literacy

## load the data
data <- read.csv("week5/transfer.csv")
mid1 <- 10188 + (13584 - 10188) / 2
mid2 <- 13584 + (16980 - 13584) / 2

## Create normalized percent score variable
data$pscore <-
  ifelse(data$pop82 <= mid1, (data$pop82 - 10188)/10188,
         ifelse(data$pop82 <= mid2, (data$pop82 - 13584)/13584,
                (data$pop82 - 16980)/16980))*100

## figure out the nearest midpoint for each city in terms of percentage points
## how many percentage points away from the nearest threshold is a particular
## city
## construct midpoints to do that

## calculating the causal effect
dta.below <- subset(data, (data$pscore >= -3) & (data$pscore < 0))
dta.above <- subset(data, (data$pscore >= 0) & (data$pscore <= 3))
## effect on literacy rate
## below threshold
lm(literate91 ~ pscore, data = dta.below)
## above threshold
lm(literate91 ~ pscore, data = dta.above)

## Lab questions

## Question 1: calulate causal effect of government funding on poverty rate
## (poverty91)
lm(poverty91 ~ pscore, data = dta.below)
lm(poverty91 ~ pscore, data = dta.above)
## There is a 6 percentage decrease at the intercept

## Question 2: Scatter plot
## use ggplot to visualize this regression discontinuity
ggplot() + 
  geom_point(aes(x=pscore, y=poverty91),
             (data=subset(data, (data$pscore >= -3 & data$pscore <= 3))), color="darkgrey") +
  geom_smooth(aes(x=pscore, y=poverty91), data=dta.below, method=lm, se=FALSE, color="purple") +
  geom_smooth(aes(x=pscore, y=poverty91), data=dta.above, method=lm, se=FALSE, color="purple") +
  theme_minimal()

## Shows that funding has a negative effect on poverty rates
## the plot shows a negative jump at the intercept

## Question 3: Assumptions
## In order to interpret this effect as causal, the assumption that the poverty 
## rates were comparable before the treatment. We do not want a situation
## where there was a significant difference in the poverty rates above and
## below the threshold in the pre-treatment data.
## Check the variable poverty81 before and after the treatment
## Can also check the same plot for 1981

poverty1980 <- ggplot() + 
    geom_point(aes(x=pscore, y=poverty80),
               (data=subset(data, (data$pscore >= -3 & data$pscore <= 3))), color="pink") +
    geom_smooth(aes(x=pscore, y=poverty80), data=dta.below, method=lm, se=FALSE, color="purple") +
    geom_smooth(aes(x=pscore, y=poverty80), data=dta.above, method=lm, se=FALSE, color="purple")
print(poverty1980)

## Running the regression lines for the two groups before the treatment
lm(poverty80 ~ pscore, data = dta.below)
lm(poverty80 ~ pscore, data = dta.above)

## There is a three percentage point difference in the two groups' poverty rates
## in 1980, before the treatment was administered
