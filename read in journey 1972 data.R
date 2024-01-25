# read file 

library(dplyr)
library(foreign)

#### individual details ##############
# 
# jou <- read.table("./NTS 1972-3/UKDA-2852-tab/tab/jou7273.tab",
#                   header = TRUE) %>%
#   janitor::clean_names()
# line 213996 did not have 32 elements

jou <- read.spss("./NTS 1972-3/UKDA-2852-spss/spss/jou7273.por")

jou_clean <- jou