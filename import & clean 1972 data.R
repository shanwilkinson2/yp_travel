# read file 

library(dplyr)
library(forcats)
# library(haven) # for reading spss files

ind <- read.table("./NTS 1972-3/UKDA-2852-tab/tab/ind7273.tab",
                  header = TRUE) %>%
  janitor::clean_names()

ind_clean = ind %>%
  mutate(sex = case_when(i11 == 1 ~"male", i11 == 2 ~"female", 
                         i11 ==3 ~"NA"),
         sex = factor(sex)) %>%
  relocate(sex, .after = i11) %>%

      mutate(age = case_when(
        i9 == 1 ~"1 year and under",
        i9 == 2 ~"2 years",
        i9 == 3 ~"3 years",
        i9 == 4 ~"4 years",
        i9 == 5 ~"5 years",
        i9 == 6 ~"6 years",
        i9 == 7 ~"7 years",
        i9 == 8 ~"8 years",
        i9 == 9 ~"9 years",
        i9 == 10 ~"10 years",
        i9 == 11 ~"11 years",
        i9 == 12 ~"12 years",
        i9 == 13 ~"13 years",
        i9 == 14 ~"14 years",
        i9 == 15 ~"15 years",
        i9 == 16 ~"16 years",
        i9 == 17 ~"17 years",
        i9 == 18 ~"18 years",
        i9 == 19 ~"19 years",
        i9 == 20 ~"20 years",
        i9 == 21 ~"21 years",
        i9 == 22 ~"22 years",
        i9 == 23 ~"23 years",
        i9 == 24 ~"24 years",
        i9 == 25 ~"25 years",
        i9 == 26 ~"26 to 29",
        i9 == 27 ~"30 to 39",
        i9 == 28 ~"40 to 49",
        i9 == 29 ~"50 to 59",
        i9 == 30 ~"60 to 64",
        i9 == 31 ~"65 to 69",
        i9 == 32 ~"70+",
        i9 == 33 ~"<3",
        i9 == 34 ~"3-15",
        i9 == 35 ~"15+"),
         age = factor(age),
        age = fct_relevel(age, c("2 years", "3 years", "4 years", "5 years",
                           "6 years", "7 years", "8 years", "9 years"),
                    after = 1),
        age = fct_relevel(age, "3-15", after = 3)
        ) %>%
relocate(age, .after = i9)
