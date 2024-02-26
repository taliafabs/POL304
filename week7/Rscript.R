
#install.packages("stylo")
library(stylo)

stylo()


mydata<-read.table("table_with_frequencies.txt", header=TRUE)

library(tidyverse)
library(magrittr)

mydata<- mydata %>% mutate(word=row.names(.)) %>%
  pivot_longer(1:19, names_to="pres", values_to="prop") %>%
  mutate(pres=str_extract(pres, "[^_]+"))

the<-filter(mydata, word=="the")
t.test(prop~pres, data=the)

security<-filter(mydata, word=="security")
t.test(prop~pres, data=security)


