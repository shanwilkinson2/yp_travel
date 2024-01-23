# read file 

library(dplyr)
library(haven)

ind <- read.table("./NTS 1972-3/UKDA-2852-tab/tab/ind7273.tab",
                  header = TRUE) %>%
  janitor::clean_names()

ind_clean = ind %>%
  mutate(sex = factor(i11, labels = c("male", "female", "NA"))) %>%
  relocate(sex, .after = i11) %>%
  
  # not working yet
    mutate(age = factor(i9, 
                        labels = c("1 year and under", 
"2 years",
"1 year and under",
"2 years",
"3 years",
"4 years",
"5 years",
"6 years",
"7 years",
"8 years",
"10 years",
"11 years",
"12 years",
"13 years",
"14 years",
"15 years",
"16 years",
"17 years",
"18 years",
"19 years",
"20 years",
"21 years",
"22 years",
"23 years",
"24 years",
"25 years",
"26 to 29",
"30 to 39",
"40 to 49",
"50 to 59",
"60 to 64",
"65 to 69",
"70+",
"<3",
"3-15",
"15+"))) %>%
relocate(age, .after = i9)

++109. AGE OF PERSON
Total
1 year and under
2 2 yeara
3 3 yeare
4 4 years
5 5 yeara
6 6 years
7 7 years
8 8 years
9 9 yeara
10 10 years
11 11 yeare
12 12 years
13 13 yeara
14 14 yeare
15 15 years
16 16 years
17 17 yeare
18 18 years
19 19 yeara
20 20 years
21 21 years
22 22 yeare
23 23 yeara
24 24 years
25 25 yeare
26 26 to 29
27 30 to 39
28 40 to 49
29 50 to 59
30 60 to 64
31 65 to 69
32 70 or over
33 NA <3
34 NA 3-15
35 NA 15+
