# read file 

library(dplyr)
library(foreign)

#### individual details ##############
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
    "j11_main_trasnport" = "j11",
    "j12_short_walk" = "j12",
    "j13_num_stages_excl_shortwalk" = "j13",
    "j14_length_excl_shortwalk" = "j14",
    "j16_freq_journey" = "j16",
    "j17_day_of_week" = "j17",
    "j18_day_of_recording_period" = "j18",
    "j19_tot_time" = "j19",
    "j21_time_start" = "j21",
    "j22_time_end" = "j22",
    "j24_wait_time" = "j24",
    "j25_pct_time_wait" = "j25",
    "j26_num_stages_inc_shortwalk" = "j28",
    "j30_why_not_vehicle" = "j30",
    "j31_mean_overall_speed" = "j31"
  )
    