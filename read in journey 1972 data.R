# read file 

library(dplyr)
library(foreign)

#### journey details ##############
# 
# jou <- read.table("./NTS 1972-3/UKDA-2852-tab/tab/jou7273.tab",
#                   header = TRUE) %>%
#   janitor::clean_names()
# line 213996 did not have 32 elements

jou <- read.spss("./NTS 1972-3/UKDA-2852-spss/spss/jou7273.por") %>%
  janitor::clean_names()

jou_clean <- jou %>%
  as.data.frame() %>%
  rename(
    "i1_person_id" = "i1", 
    "j1_journey_num" = "j1", 
    "j2_land_use_origin" = "j2", 
    "j3_area_type_origin" = "j3", 
    "j4_land_use_dest" = "j4",      
    "j5_area_type_dest" = "j5",
    "j6_plan_region_origin" = "j6",
    "j7_plan_region_dest" = "j7",
    "j8_purpose" = "j8",
    "j9_purpose_to" = "j9",
    "j10_purpose_from" = "j10",
    "j11_main_transport" = "j11",
    "j12_shortwalk" = "j12",
    "j13_num_stages_excl_shortwalk" = "j13",
    "j14_length_excl_shortwalk" = "j14",
    "j16_freq_journey" = "j16",
    "j17_day_of_week" = "j17",
    "j18_day_of_recording_period" = "j18",
    "j19_journey_duration" = "j19",
    "j21_time_start" = "j21",
    "j22_time_end" = "j22",
    "j24_wait_time" = "j24",
    "j25_pct_time_wait" = "j25",
    "j26_length_inc_shortwalk" = "j26",
    "j28_num_stages_inc_shortwalk" = "j28",
    "j30_why_not_vehicle" = "j30",
    "j31_mean_overall_speed" = "j31"
  ) %>%
  mutate(across(c(j2_land_use_origin, j4_land_use_dest), 
                ~case_when(
      .x == 1 ~"Home base",
      .x == 2 ~"Temp residence",
      .x == 3 ~"Industry + public utility",
      .x == 4 ~"Building site",
      .x == 5 ~"Retai1 trade",
      .x == 6 ~"wholesale/warehouse depot",
      .x == 7 ~"Office + public building",
      .x == 8 ~"Services",
      .x == 9 ~"Hospitals",
      .x == 10 ~"Education",
      .x == 11 ~"Traneport interchange",
      .x == 12 ~"Recreation",
      .x == 13 ~"Open space - develop",
      .x == 14 ~"0pen epace - natural",
      .x == 15 ~"Military",
      .x == 16 ~"Prisons + borstals",
      .x == 17 ~"Mining + quarrying",
      .x == 18 ~"Other",
      .x == 19 ~"DK/NA",
      .x == 20 ~"DNA"
    )),
    
    j8_purpose = case_when(
     j8_purpose == 1 ~ "To/ from work",
      j8_purpose ==  2 ~ "In, course work",
     j8_purpose == 3 ~ "Educational",
     j8_purpose == 4 ~ "Shopping",
     j8_purpose == 5 ~ "Personal business",
     j8_purpose == 6 ~ "Eating/drinking",
     j8_purpose == 7 ~ "Sport (ADril)",
     j8_purpose == 8 ~ "sport (Watch)",
     j8_purpose == 9 ~ "sport (Part)",
     j8_purpose == 10 ~"Entertainment",
     j8_purpose ==  11 ~"Social",
     j8_purpose ==  12 ~"Hols April",
     j8_purpose ==  13 ~"Hols",
     j8_purpose ==  14 ~"Day-trip",
     j8_purpose ==  15 ~"Escort",
     j8_purpose ==  16 ~"Other",
     j8_purpose ==  17 ~NA
    ),
    j8_purpose = factor(j8_purpose),
    
    across(c(j9_purpose_to, j10_purpose_from), 
           ~case_when(
            .x == 1 ~"Work",
            .x == 2 ~"In course work",
            .x == 3 ~"Educational",
            .x == 4 ~"Shopping",
            .x == 5 ~"Personal business",
            .x == 6 ~"Eating/drinking",
            .x == 7 ~"Sport (April)",
            .x == 8 ~"Sport (Watch)",
            .x == 9 ~"Sport (Part)",
            .x == 10 ~"Entertainment",
            .x == 11 ~"Social",
            .x == 12 ~"Hols April",
            .x == 13 ~"Hols",
            .x == 14 ~"Day-trip",
            .x == 15 ~"Escort",
            .x == 16 ~"Other",
            .x == 17 ~"Home",
            .x == 18 ~"NA",
            .x == 19 ~"DNA"
           )
    ),
    
   j11_main_transport = case_when(
     j11_main_transport == 2 ~"LT underground",
     j11_main_transport == 3 ~ "LT stage bus",
     j11_main_transport == 4 ~"Other stage bus",
     j11_main_transport == 5 ~"Long distance bus",
     j11_main_transport == 6 ~"Othar public transport",
     j11_main_transport == 7 ~"Private car/van/1orry",
     j11_main_transport == 8 ~ "Motorcycle/scooter/moped",
     j11_main_transport == 9 ~"Bike",
    j11_main_transport ==  10 ~"Walk", 
    j11_main_transport == 11 ~"Other private tranaport",
    j11_main_transport ==  12 ~NA
  ),
  j11_main_transport = factor(j11_main_transport),
  
  j12_shortwalk = case_when(
    j12_shortwalk == 1 ~TRUE,
    j12_shortwalk ==2 ~FALSE,
    j12_shortwalk ==3 ~NA
  ),
  
  j14_length_excl_shortwalk = case_when(
    j14_length_excl_shortwalk == 1 ~"Under 1 mile",
    j14_length_excl_shortwalk == 2 ~"1 to 1.9 miles",
    j14_length_excl_shortwalk == 3 ~"2 to 2.9 miles",
    j14_length_excl_shortwalk == 4 ~"3 to 4.9 miles",
    j14_length_excl_shortwalk == 5 ~"5 to 9.9 miles",
    j14_length_excl_shortwalk == 6 ~"10 to 14.9 miles",
    j14_length_excl_shortwalk == 7 ~"15 to 24.9 miles",
    j14_length_excl_shortwalk == 8 ~"25 to 29.9 miles",
    j14_length_excl_shortwalk == 9 ~"30 to 49.9 miles",
    j14_length_excl_shortwalk == 10 ~"50 to 99.9 miles",
    j14_length_excl_shortwalk == 11 ~"100 to 199 9 miles",
    j14_length_excl_shortwalk == 12 ~"200 miles +",
    j14_length_excl_shortwalk == 13 ~"DK/ NA",
    j14_length_excl_shortwalk == 14 ~"DNA"
  ),
  j14_length_excl_shortwalk = factor(j14_length_excl_shortwalk),
  
  j16_freq_journey = case_when(
    j16_freq_journey == 1 ~"5+ times a week",
    j16_freq_journey == 2 ~"2-4 times a week",
    j16_freq_journey == 3 ~"Weekly",
    j16_freq_journey == 4 ~"Monthly but less than weekly",
    j16_freq_journey == 5 ~"Less frequent, Irregular",
    j16_freq_journey == 6 ~NA,
    j16_freq_journey == 7 ~"DNA"
  ),
  j16_freq_journey = factor(j16_freq_journey),
  
  j19_journey_duration = case_when(
    j19_journey_duration == 1 ~"Under 15 mins",
    j19_journey_duration == 2 ~"15 to 29 mins",
    j19_journey_duration == 3 ~"30 to 44 mins",
    j19_journey_duration == 4 ~"45 to 59 mins",
    j19_journey_duration == 5 ~"60 to <90 mins",
    j19_journey_duration == 6 ~"90 to <120 mins",
    j19_journey_duration == 7 ~"120 <150 mins",
    j19_journey_duration == 8 ~"150 <3hrs",
    j19_journey_duration == 9 ~"3 <4hrs",
    j19_journey_duration == 10 ~"4 <5hrs",
    j19_journey_duration == 11 ~"5 <6hrs",
    j19_journey_duration == 12 ~"6hrs+",
    j19_journey_duration == 13 ~"NA/ DK",
    j19_journey_duration == 14 ~"DNA"
  ),
  j19_journey_duration = factor(j19_journey_duration),
  
  across(c(j21_time_start, j22_time_end),
         ~case_when(
         .x == 1 ~"Midnight - 0059",
         .x == 2 ~"0100 - 0159",
         .x == 3 ~"0200 - 0259",
         .x == 4 ~"0300 - 0359",
         .x == 5 ~"0400 - 0459",
         .x == 6 ~"0500 - 0559",
         .x == 7 ~"0600 - 0629",
         .x == 8 ~"0630 - 0659",
         .x == 9 ~"0700 - 0714",
         .x == 10 ~"0715 - 0729",
         .x == 11 ~"0730 - 0744",
         .x == 12 ~"0745 - 0759",
         .x == 13 ~"0800 - 0814",
         .x == 14 ~"0815 - 0829",
         .x == 15 ~"0830 - 0844",
         .x == 16 ~"0845 - 0859",
         .x == 17 ~"0900 - 0914",
         .x == 18 ~"0915 - 0929",
         .x == 19 ~"0930 - 0959",
         .x == 20 ~"1000 - 1029",
         .x == 21 ~"1030 - 1059",
         .x == 22 ~"1100 - 1129",
         .x == 23 ~"1130 - 1159",
         .x == 24 ~"1200 - 1229",
         .x == 25 ~"1230 - 1259",
         .x == 26 ~"1300 - 1329",
         .x == 27 ~"1330 - 1359",
         .x == 28 ~"1400 - 1429",
         .x == 29 ~"1430 - 1459",
         .x == 30 ~"1500 - 1529",
         .x == 31 ~"1530 - 1559",
         .x == 32 ~"1600 - 1629",
         .x == 33 ~"1630 - 1644",
         .x == 34 ~"1645 - 1659",
         .x == 35 ~"1700 - 1714",
         .x == 36 ~"1715 - 1729",
         .x == 37 ~"1730 - 1744",
         .x == 38 ~"1745 - 1759",
         .x == 39 ~"1S00 - 1814",
         .x == 40 ~"1815 - 1829",
         .x == 41 ~"1830 - 1859",
         .x == 42 ~"1900 - 1929",
         .x == 43 ~"1930 - 1959",
         .x == 44 ~"2000 - 2029",
         .x == 45 ~"2030 - 2059",
         .x == 46 ~"2100 - 2129",
         .x == 47 ~"2130 - 2159",
         .x == 48 ~"2200 - 2229",
         .x == 49 ~"2230 - 2259",
         .x == 50 ~"2300 - 2329",
         .x == 51 ~"2330 - 2359",
         .x == 52 ~"NA"
         )
         ),
  
  j26_length_inc_shortwalk = case_when(
    j26_length_inc_shortwalk == 1 ~"Under 1 mile",
    j26_length_inc_shortwalk == 2 ~"1 to 1.9 miles",
    j26_length_inc_shortwalk == 3 ~"2 to 2.9 miles",
    j26_length_inc_shortwalk == 4 ~"3 to 4.9 miles",
    j26_length_inc_shortwalk == 5 ~"5 to 9.9 miles",
    j26_length_inc_shortwalk == 6 ~"10 to 14.9 miles",
    j26_length_inc_shortwalk == 7 ~"15 to 24.9 miles",
    j26_length_inc_shortwalk == 8 ~"25 to 29.9 miles",
    j26_length_inc_shortwalk == 9 ~"30 to 49.9 miles",
    j26_length_inc_shortwalk == 10 ~"50 to 99.9 miles",
    j26_length_inc_shortwalk == 11 ~"100 to 199 9 miles",
    j26_length_inc_shortwalk == 12 ~"200 miles +",
    j26_length_inc_shortwalk == 13 ~"DK/ NA",
    j26_length_inc_shortwalk == 14 ~"DNA"
  ),
  j26_length_inc_shortwalk = factor(j26_length_inc_shortwalk)
  ) %>%
  rename(j27_mi_inc_shortwalk = j27,
         j15_mi_exc_shortwalk = j15,
         j20_mins = j20
         ) %>%
  mutate(j27_mi_inc_shortwalk = j27_mi_inc_shortwalk/10,
         j15_mi_exc_shortwalk =j15_mi_exc_shortwalk/10
         )

###
saveRDS(jou_clean, "jou_clean.RDS")

