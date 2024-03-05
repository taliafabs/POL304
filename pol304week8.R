# POL304 week 8 lecture R Script
# workplace setup
library(tidyverse)
library(magrittr)
library(rvest)

myurl<-"https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
myhtml <- read_html(myurl)
myhtml

mytable<-html_nodes(myhtml, "table") %>%  #Gets everything in the element
  html_table(fill=TRUE) #Convert to an R table, fill=TRUE is necessary when the website has multiple tables
# get the third table
mytable<-mytable %>% extract2(3) #since the website has multiple tables, we need to extract the 3rd one.

mytable<-read_html(myurl) %>% 
  html_nodes("table") %>% 
  html_table(fill=TRUE)  %>% 
  extract2(3) %>% #our table is actually nested within a list element [[]]
  select(Country=1, Year=4, GDP=3) %>% 
  slice(3:214) %>% 
  mutate( Year=str_remove(Year, ".*\\]"), #remove everything before the ]
          GDP=str_remove(GDP, ".*\\]"),GDP=parse_number(GDP), Year=parse_number(Year))

## Follow the same steps to scrape the Wikipedia table of foreign direct investments
myurl1<-"https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
myhtml1 <- read_html(myurl1)
table1 <- read_html(myurl1) %>%
  mutate(Year = str_remove(Year, ".*\\"),
         FDI = str_remove(),
         FDI = parse_number(FDI), Year=parse_number(Year))

maryland2020 <-"https://elections.maryland.gov/elections/2020/results/general/gen_detail_results_2020_4_BOT001-.html"
pres<-read_html(maryland2020) %>% html_nodes("table") %>% html_table(fill=TRUE) %>% extract2(2) %>% 
  select(County=Jurisdiction, Biden20=contains("Biden"), Trump20=contains("Trump")) %>% 
  filter(str_detect(County, "Total", negate=TRUE)) %>% 
  mutate(Biden20=parse_number(Biden20), Trump20=parse_number(Trump20))

maryland2016 <- "https://elections.maryland.gov/elections/2016/results/General/gen_results_2016_4_001-.html"
pres <- read_html(maryland2016) %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>% extract2(1) %>%
  select(County=Jurisdiction, Clinton16=contains("Clinton"),
         Trump16=contains("Trump")) %>%
  filter(str_detect(County, "Total", negate=TRUE)) %>%
  mutate(Clinton16=parse_number(Clinton16), Trump16=parse_number(Trump16))


## Current MPP's
## extract names, links to websites, names, parties
parliament43 <- "https://www.ourcommons.ca/members/en/search?parliament=43"
parl43_html <- read_html(parliament43)

mpps <- parl43_html %>%
  html_nodes("table") |> html_table(fill=TRUE) %>%
  extract(1)

mpp_links <- read_html(parl43_html) %>%
  html_nodes("table") |> html_nodes("a") %>%
  html_attr("href")

mpp_links <- mpp_links[3:132]

mpp1 <- read_html(paste0("https://www,ola.org/", mpp_links[1])) %>%
  html_nodes(".views-field") %>%
  html_nodes("a") %>%
  html_attr("href")

mpp1[5]

