# 1972 children's travel

library(dplyr)

# read in files

jou_clean <- readRDS("jou_clean.RDS")
ind_clean <- readRDS("ind_clean.RDS")

###

jou_clean_2 <- jou_clean %>%
  # currently not requiring multiple people 
  # so 2 journeys close together in time similar distance showing travellign with self
  mutate(
    row_id = row_number()) %>%
  relocate(row_id, .before = recid) %>%
  arrange(hholdid, j18_day_of_recording_period, j21_time_start, i1_person_id) %>%
  group_by(hholdid, j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, j9_purpose_to, j10_purpose_from) %>%
  mutate(
    trip_together = min(row_id),
    num_people = n_distinct(i1_person_id),
    num_rows = n(),
    right_num_people = ifelse(num_people == num_rows, TRUE, FALSE)
  ) %>%
  relocate(
    c(trip_together, num_people, right_num_people), .after = i1_person_id)

jou_clean_2 %>% 
  group_by(trip_together) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(mean(right_num_people))

