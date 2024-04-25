# read file 

library(dplyr)
library(forcats)

#### individual details ##############

ind <- read.table("./NTS 1972-3/UKDA-2852-tab/tab/ind7273.tab",
                  header = TRUE) %>%
  janitor::clean_names()

ind_clean = ind %>%
  rename(
    i1_person_id = i1,
    i2_travel_rec_complete = i2,
    i3_ind_sched_complete = i3,
    i4_hh_status = i4,
    i5_marital_status = i5,
    i6_work_status = i6,
    i7_socioeconomic_group = i7,
    i8_gross_income = i8,
    i9_age = i9,
    i10_age_sex = i10,
    i11_sex = i11,
    i12_housewife = i12,
    i13_car_license = i13,
    i14_motorbike_license = i14,
    i15_other_license = i15,
    i16_driving_experience = i16,
    i17_license_ever_held = i17,
    i18_licence_givenup_when = i18,
    i19_licence_givenup_yrs = i19,
    i20_license_type = i20,
    i21_pubtrans_can_use = i21,
    i22_access_to_bus = i22,
    i23_access_to_rail = i23,
    i24_access_to_all_pubtrans = i24,
    i25_bus_quality = i25,
    i27_num_criticisms_bus = i27,
    i28_quality_train = i28,
    i30_num_criticisms_train = i30,
    i31_tot_individ_journeys = i31,
    i32_tot_individ_stages = i32,
    i33_tot_individ_miles = i33,
    i34_tot_individ_spend = i34,
    i35_tot_individ_transport_day7 = i35
    ) %>%
  mutate(i11_sex = case_when(i11_sex == 1 ~"male", i11_sex == 2 ~"female", 
                         i11_sex ==3 ~"NA"),
         i11_sex = factor(i11_sex)) %>%

      mutate(i9_age = case_when(
        i9_age == 1 ~"1 year and under",
        i9_age == 2 ~"2 years",
        i9_age == 3 ~"3 years",
        i9_age == 4 ~"4 years",
        i9_age == 5 ~"5 years",
        i9_age == 6 ~"6 years",
        i9_age == 7 ~"7 years",
        i9_age == 8 ~"8 years",
        i9_age == 9 ~"9 years",
        i9_age == 10 ~"10 years",
        i9_age == 11 ~"11 years",
        i9_age == 12 ~"12 years",
        i9_age == 13 ~"13 years",
        i9_age == 14 ~"14 years",
        i9_age == 15 ~"15 years",
        i9_age == 16 ~"16 years",
        i9_age == 17 ~"17 years",
        i9_age == 18 ~"18 years",
        i9_age == 19 ~"19 years",
        i9_age == 20 ~"20 years",
        i9_age == 21 ~"21 years",
        i9_age == 22 ~"22 years",
        i9_age == 23 ~"23 years",
        i9_age == 24 ~"24 years",
        i9_age == 25 ~"25 years",
        i9_age == 26 ~"26 to 29",
        i9_age == 27 ~"30 to 39",
        i9_age == 28 ~"40 to 49",
        i9_age == 29 ~"50 to 59",
        i9_age == 30 ~"60 to 64",
        i9_age == 31 ~"65 to 69",
        i9_age == 32 ~"70+",
        i9_age == 33 ~"<3",
        i9_age == 34 ~"3-15",
        i9_age == 35 ~"15+"),
         i9_age = factor(i9_age),
        i9_age = fct_relevel(i9_age, c("2 years", "3 years", "4 years", "5 years",
                           "6 years", "7 years", "8 years", "9 years"),
                    after = 1),
        i9_age = fct_relevel(i9_age)
        ) %>%
  mutate(child = ifelse(i9_age %in% c(
    "1 year and under", "2 years", "3 years", "4 years", "5 years",         
    "6 years" , "7 years", "8 years" , "9 years", "10 years",        
    "11 years", "12 years", "13 years", "14 years", "15 years",        
    "16 years", "17 years")
    , TRUE, FALSE)
  )

###

saveRDS(ind_clean, "ind_clean.RDS")
