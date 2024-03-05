# install.packages("stylo")
library(stylo)
myanalysis <- stylo()

mydata<-read.table("./data/example1/table_with_frequencies.txt", header=TRUE)
mydata

# the
mean(as.numeric(mydata['the',1:9])) #Bush
# bush uses 'the' more frequently than obama
mean(as.numeric(mydata['the',10:19])) #Obama

# and
mean(as.numeric(mydata['and',1:9])) #Bush
mean(as.numeric(mydata['and',10:19])) #Obama


# 1. test whether Bush uses the word "economy" more or less frequently than obama
mean(as.numeric(mydata['economy',1:9])) #Bush
mean(as.numeric(mydata['economy',10:19])) #Obama
# Obama used the word economy a mean of 4.14 times, compared to 0.17 for bush
# 2009 economic recession? Obamacare?
# Could compare Obama during 2008 campaign vs after 2009

# 2. test whether Bush uses the word "security" more or less frequently than obama
mean(as.numeric(mydata['security',1:9])) #Bush
mean(as.numeric(mydata['security',10:19])) #Obama
# Bush uses security a mean of 0.23 compared to 0.15 for Obama
# this makes sense because 9/11 happened during Bush's term
# to isolate the effects of 9/11 on security emphasis in speeches, more would
# need to be done