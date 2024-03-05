# install.packages("stylo")

## part 2 documentary hypothesis
library(stylo)
stylo::classify
genesis<-classify()
summary(genesis)
genesis$distance.table
# calculates distance in unknown units between each piece of text in the training
# set and each piece of text in the testing set
# smaller distance means more similar, larger distance means less similar

# e is more likely to be the author of passage x


