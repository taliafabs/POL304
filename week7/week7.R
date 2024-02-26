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

# 2. test whether Bush uses the word "security" more or less frequently than obama
mean(as.numeric(mydata['security',1:9])) #Bush
mean(as.numeric(mydata['security',10:19])) #Obama