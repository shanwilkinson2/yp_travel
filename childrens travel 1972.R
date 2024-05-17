# 1972 children's travel

library(dplyr)
library(data.table)

# read in files

ind_clean <- readRDS("ind_clean.RDS")
jou_clean <- readRDS("jou_clean.RDS")

###

# make a sample of 20 households

  set.seed(42)
  hh_sample <- sample(unique(ind_clean$hholdid), 20)
  jou_clean_sample <- jou_clean %>%
    filter(hholdid %in% hh_sample)
  
  ind_clean_sample <- ind_clean %>%
    filter(hholdid %in% hh_sample)
  
  # saveRDS(jou_clean_sample, "jou_clean_sample.RDS")
  # write.csv(jou_clean_sample, "jou_clean_sample.csv")

###

# lots of times are missing ~80%
  
  # mostly working but not for escort trips #################
  # remove escorter who's not currently escorting from trip_together
  # not workign if escorter doing non-escort is the first person in the trip togehter

jou_clean_2 <- jou_clean_sample %>%
    mutate(unique_person_id = hholdid * 1000 + i1_person_id) %>%
    relocate(unique_person_id) %>%
  # add age & gender
  left_join(ind_clean_sample %>% 
              select(hholdid, i1_person_id, i9_age, i10_age_sex, i11_sex, child),
            by = c("hholdid", "i1_person_id")
            ) %>%
  relocate(i10_age_sex, .after = i1_person_id) %>%
  # make a row number so can use this to indicate people travelling togetehr
  mutate(
    row_id = row_number()
    ) %>%
  relocate(row_id, .before = recid) %>%
  # arrange(hholdid, j18_day_of_recording_period, j21_time_start, i1_person_id) %>%
  # group by trip info plus person info
  group_by(i1_person_id, 
           hholdid, j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, j8_purpose, j9_purpose_to, j10_purpose_from,
           j2_land_use_origin, j4_land_use_dest) %>%
  # add a person trip number, to identify similar trips 
    # so doesn't make the same person travel with themselves 
    # as similar trips will have different number so include this in the group_by
  mutate(duplicate_trip = ave(i1_person_id, FUN = seq_along)) %>%
  relocate(duplicate_trip, .after = i1_person_id) %>%
  ungroup() %>%
  # add a travelling together id from minimum row_id of the group
  group_by(duplicate_trip,
           hholdid, j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, 
           j2_land_use_origin, j4_land_use_dest #,
          # escort trips have different purpose for those being escorted
           # j8_purpose, j9_purpose_to, j10_purpose_from 
           ) %>%
  mutate(
    trip_together = min(row_id),
    ) %>%
    ungroup() %>%
  arrange(trip_together) %>%
    # get escort trips
  group_by(trip_together) %>% 
    mutate(
      escort = ifelse(j8_purpose == "Escort" | 
                            j9_purpose_to == "Escort" | 
                            j10_purpose_from == "Escort"
                          , 
        TRUE, 
        FALSE
      ),
      escort_trip = max(escort)
      ) %>%
    ungroup() %>%
    
    group_by(trip_together, unique_person_id) %>%
      mutate(
        escorter = ifelse(j8_purpose == "Escort" | 
                          j9_purpose_to == "Escort" | 
                          j10_purpose_from == "Escort"
                        , 
                        TRUE, 
                        FALSE
        )
      ) %>%
    ungroup() %>%
    
    # find duplicate people
    group_by(trip_together, unique_person_id) %>%
      mutate(duplicate_person = n()) %>%
    ungroup() %>%
    
    # remove escorter who's not currently escorting from trip_together
    # not workign if escorter doing non-escort is the first person in the trip togehter
    mutate(trip_together = ifelse(escort_trip == TRUE & escorter == TRUE & escort == FALSE, 
                  row_id, trip_together
                  ),
           ) %>%

    
    # check if right number of people
    group_by(trip_together) %>%
    mutate(
      num_people = n_distinct(i1_person_id),
      num_rows = n(),
      right_num_people = ifelse(num_people == num_rows, TRUE, FALSE)
      ) %>%
    relocate(
      c(trip_together, num_people, right_num_people, duplicate_person), .after = i1_person_id) %>%
      ungroup()

  # escort days - ie days on which an escort trip was made
  # so can check if people being escorted looks right
escort_days <- jou_clean_2 %>%
  filter(escort_trip ==1 & right_num_people == FALSE) %>%
  group_by(hholdid, duplicate_trip,
           j17_day_of_week, j21_time_start, j22_time_end, j11_main_transport, 
           j27_mi_inc_shortwalk, j8_purpose, j9_purpose_to, j10_purpose_from
           )
  
jou_clean_2 %>% 
  group_by(trip_together) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(mean(right_num_people))

