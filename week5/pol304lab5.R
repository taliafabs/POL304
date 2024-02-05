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

## Question 2

## Question 3
