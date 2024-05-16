# 1972 children's travel

library(dplyr)
library(data.table)

# read in files

###

  set.seed(42)
  hh_sample <- sample(unique(ind_clean$hholdid), 20)
  jou_clean_sample <- jou_clean %>%
    filter(hholdid %in% hh_sample)
  
  ind_clean_sample <- ind_clean %>%
    filter(hholdid %in% hh_sample)
  
  saveRDS(jou_clean_sample, "jou_clean_sample.RDS")
  write.csv(jou_clean_sample, "jou_clean_sample.csv")

###

# lots of times are missing ~80%
  
  # mostly working but not for escort trips #################

jou_clean_2 <- jou_clean_sample %>%
  # add age & gender
  left_join(ind_clean_sample %>% 
              select(hholdid, i1_person_id, i9_age, i10_age_sex, i11_sex, child),
            by = c("hholdid", "i1_person_id")
            ) %>%
  relocate(i10_age_sex, .after = i1_person_id) %>%
  # currently not requiring multiple people 
  # so 2 journeys close together in time similar distance showing travellign with self
  mutate(
    row_id = row_number()
    ) %>%
  relocate(row_id, .before = recid) %>%
  arrange(hholdid, j18_day_of_recording_period, j21_time_start, i1_person_id) %>%
  # group by trip info plus person info
  # add a person trip number, to identify similar trips
  group_by(i1_person_id, 
           hholdid, j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, j8_purpose, j9_purpose_to, j10_purpose_from,
           j2_land_use_origin, j4_land_use_dest) %>%
  mutate(duplicate_trip = ave(i1_person_id, FUN = seq_along)) %>%
  relocate(duplicate_trip, .after = i1_person_id) %>%
  ungroup() %>%
  # add a travelling togeether id 
  group_by(duplicate_trip,
           hholdid, j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, 
           j2_land_use_origin, j4_land_use_dest
           # j8_purpose, j9_purpose_to, j10_purpose_from
           ) %>%
  mutate(
    trip_together = min(row_id),
    num_people = n_distinct(i1_person_id),
    num_rows = n(),
    right_num_people = ifelse(num_people == num_rows, TRUE, FALSE)
  ) %>%
    ungroup() %>%
  relocate(
    c(trip_together, num_people, right_num_people), .after = i1_person_id) %>%
  arrange(trip_together) %>%
    # get escort trips
  group_by(hholdid, j18_day_of_recording_period) %>% 
    mutate(
      escort_day = ifelse(j8_purpose == "Escort" | 
                            j9_purpose_to == "Escort" | 
                            j10_purpose_from == "Escort"
                          , 
        TRUE, 
        FALSE
      ),
      escort_day = max(escort_day)
      ) %>%
    ungroup()

escort_days <- jou_clean_2 %>%
  filter(escort_day ==1 ) %>%
  group_by(hholdid, duplicate_trip,
           j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, j8_purpose, j9_purpose_to, j10_purpose_from
           )
  
jou_clean_2 %>% 
  group_by(trip_together) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(mean(right_num_people))

