



bfa_dates <- c(as.Date(c("2019-02-05","2019-03-14","2019-04-02","2019-05-11",
                       "2019-07-23","2019-08-14","2019-09-06","2019-10-02",
                       "2019-12-31","2020-01-27","2020-02-17","2020-03-25",
                       "2020-04-22","2020-06-07","2020-07-09","2020-08-08",
                       "2020-09-08","2020-11-10","2020-12-31","2021-01-31",
                       "2021-02-28","2021-03-31","2021-04-30","2021-05-31",
                       "2021-06-30","2021-07-31","2021-08-31","2021-09-30",
                       "2021-10-31","2021-11-30","2021-12-31","2022-01-31",
                       "2022-02-28","2022-04-30","2022-09-30","2022-10-31",
                       "2022-11-30","2022-12-31","2023-01-31","2023-02-28"), format = "%Y-%m-%d"),last_acled_date)


model_bfa <-bfa_df %>% mutate(survey_date = as.Date(Date, format = "%d-%m-%Y")) %>% filter(survey_date %in% bfa_dates)

# Add in target0 dates - latest ACLED date

target0_date <- model_bfa %>% filter(survey_date=="2023-02-28") %>% mutate(PDI=NA, survey_date = last_acled_date, Date = "27-01-2023")

model_bfa <- rbind(model_bfa, target0_date)

model_bfa %<>% mutate(Region = ifelse(Region == "Plateau Central", "Plateau-Central", Region))

date_ranges <- tibble(dates = bfa_dates, time_code = seq(1,length(bfa_dates),1))

all_dates <- left_join(tibble(dates = seq(as.Date("2019-02-05", format = "%Y-%m-%d"), 
                                          last_acled_date,1)), 
                       date_ranges, by = "dates") %>% 
  fill(time_code, .direction = "up")

recent_bfa <- acled_burkina %>% mutate(Date = as_date(EVENT_DATE)) %>% filter(Date %in% all_dates$dates)
recent_bfa <- left_join(recent_bfa %>% mutate(cha_date = as.character(Date)), all_dates %>% mutate(cha_date = as.character(dates)), by = ("cha_date"))
recent_sum_bfa <- recent_bfa %>% mutate(FATALITIES_civil = ifelse(EVENT_TYPE=="Violence against civilians", FATALITIES, 0),
                                        EVENTS_civil = ifelse(EVENT_TYPE=="Violence against civilians", 1, 0),
                                        EVENTS_battles = ifelse(EVENT_TYPE=="Battles", 1, 0),
                                        EVENTS_explo = ifelse(EVENT_TYPE=="Explosions/Remote violence", 1, 0),
                                        EVENTS_kidnap = ifelse(SUB_EVENT_TYPE=="Abduction/forced disappearance", 1, 0),
                                        EVENTS_looting = ifelse(SUB_EVENT_TYPE=="Looting/property destruction", 1, 0),
                                        ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                  (grepl("Islam",ACTOR1) | grepl("JNIM",ACTOR1) | ACTOR1 == "Katiba Macina" | ACTOR1 == "MUJAO: Movement for Unity and Jihad in West Africa"), 1, 0),
                                        ACTOR1_ethnic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                 (grepl("Ethnic",ACTOR1) | grepl("Communal",ACTOR1) | ACTOR1=="Dan Na Ambassagou"), 1, 0),
                                        ACTOR1_military = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                   (grepl("Military Forces",ACTOR1)), 1, 0)) %>% 
  group_by(ADMIN1, time_code) %>% summarise(fatal = sum(FATALITIES, na.rm = T), 
                                            max_fatal = max(FATALITIES),
                                            fatal_civil = sum(FATALITIES_civil),
                                            events_civil = sum(EVENTS_civil),
                                            events_battle = sum(EVENTS_battles),
                                            events_explo = sum(EVENTS_explo),
                                            events_kidnap = sum(EVENTS_kidnap),
                                            events_looting = sum(EVENTS_looting),
                                            events_islamic = sum(ACTOR1_islamic),
                                            events_ethnic = sum(ACTOR1_ethnic),
                                            events_military = sum(ACTOR1_military),
                                            .groups = "drop")

model_bfa <- left_join(left_join(model_bfa, date_ranges, by = c("survey_date" = "dates")), recent_sum_bfa, by = c("Region" = "ADMIN1", "time_code")) %>% arrange(Region, time_code)


model_bfa %<>% mutate(fatal = replace_na(fatal, 0), 
                      max_fatal = replace_na(max_fatal, 0), 
                      fatal_civil = replace_na(fatal_civil, 0), 
                      events_civil = replace_na(events_civil, 0), 
                      events_battle = replace_na(events_battle, 0), 
                      events_explo = replace_na(events_explo, 0), 
                      events_kidnap = replace_na(events_kidnap, 0),   
                      events_looting = replace_na(events_looting, 0),   
                      events_islamic = replace_na(events_islamic, 0), 
                      events_ethnic = replace_na(events_ethnic, 0), 
                      events_military = replace_na(events_military, 0))


model_bfa <- model_bfa %>% group_by(Region) %>% 
  mutate(lag_idp = lag(PDI), 
         diff = ifelse(PDI>lag(PDI),PDI - lag(PDI), 0),
         previous_survey = lag(survey_date))%>%
  filter(time_code != 1)





model_idp <- rbind(mali_data %>% select(survey_date, admin1, idp),
                   mali_march22)

target0_date <- model_idp %>% filter(survey_date=="2022-12-31") %>% mutate(idp=NA, survey_date = last_acled_date)
model_idp <- rbind(model_idp, target0_date)

distinct_dates <- model_idp %>% distinct(survey_date)
mli_dates <- as_date(distinct_dates$survey_date)
date_ranges <- tibble(dates = mli_dates, time_code = seq(1,length(mli_dates),1))

all_dates <- left_join(tibble(dates = seq(as.Date("2014-02-28", format = "%Y-%m-%d"), 
                                          last_acled_date,1)), 
                       date_ranges, by = "dates") %>% 
  fill(time_code, .direction = "up")

acled_mali %<>% mutate(ADMIN1 = ifelse(ADMIN1=="Segou", "Ségou", ADMIN1))

recent_mli <- acled_mali %>% mutate(Date = as_date(EVENT_DATE)) %>% filter(Date %in% all_dates$dates)
recent_mli <- left_join(recent_mli %>% mutate(cha_date = as.character(Date)), all_dates %>% mutate(cha_date = as.character(dates)), by = ("cha_date"))
recent_sum_mli <- recent_mli %>% mutate(FATALITIES_civil = ifelse(EVENT_TYPE=="Violence against civilians", FATALITIES, 0),
                                        EVENTS_civil = ifelse(EVENT_TYPE=="Violence against civilians", 1, 0),
                                        EVENTS_battles = ifelse(EVENT_TYPE=="Battles", 1, 0),
                                        EVENTS_explo = ifelse(EVENT_TYPE=="Explosions/Remote violence", 1, 0),
                                        EVENTS_kidnap = ifelse(SUB_EVENT_TYPE=="Abduction/forced disappearance", 1, 0),
                                        EVENTS_looting = ifelse(SUB_EVENT_TYPE=="Looting/property destruction", 1, 0),
                                        ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                  (grepl("Islam",ACTOR1) | grepl("JNIM",ACTOR1) | ACTOR1 == "Katiba Macina" | ACTOR1 == "MUJAO: Movement for Unity and Jihad in West Africa"), 1, 0),
                                        ACTOR1_ethnic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                 (grepl("Ethnic",ACTOR1) | grepl("Communal",ACTOR1) | ACTOR1=="Dan Na Ambassagou"), 1, 0),
                                        ACTOR1_military = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                   (grepl("Military Forces",ACTOR1)), 1, 0)) %>% 
  group_by(ADMIN1, time_code) %>% summarise(fatal = sum(FATALITIES, na.rm = T), 
                                            max_fatal = max(FATALITIES),
                                            fatal_civil = sum(FATALITIES_civil),
                                            events_civil = sum(EVENTS_civil),
                                            events_battle = sum(EVENTS_battles),
                                            events_explo = sum(EVENTS_explo),
                                            events_kidnap = sum(EVENTS_kidnap),
                                            events_looting = sum(EVENTS_looting),
                                            events_islamic = sum(ACTOR1_islamic),
                                            events_ethnic = sum(ACTOR1_ethnic),
                                            events_military = sum(ACTOR1_military),
                                            .groups = "drop")

model_mli <- left_join(left_join(model_idp, date_ranges, by = c("survey_date" = "dates")), recent_sum_mli, by = c("admin1" = "ADMIN1", "time_code")) %>% arrange(admin1, time_code)


model_mli %<>% mutate(fatal = replace_na(fatal, 0), 
                      max_fatal = replace_na(max_fatal, 0), 
                      fatal_civil = replace_na(fatal_civil, 0), 
                      events_civil = replace_na(events_civil, 0), 
                      events_battle = replace_na(events_battle, 0),
                      events_explo = replace_na(events_explo, 0), 
                      events_kidnap = replace_na(events_kidnap, 0), 
                      events_looting = replace_na(events_looting, 0), 
                      events_islamic = replace_na(events_islamic, 0), 
                      events_ethnic = replace_na(events_ethnic, 0), 
                      events_military = replace_na(events_military, 0))


model_mli <- model_mli %>% group_by(admin1) %>% 
  mutate(lag_idp = lag(idp), 
         diff = ifelse(idp>lag(idp),idp - lag(idp), 0),
         previous_survey = lag(survey_date)) %>%
  filter(time_code != 1)




# Niger
model_ner <- niger_df %>% mutate(survey_date = as_date(Date)) %>% select(-Date) %>%
  pivot_longer(-survey_date, names_to = "Region", values_to = "idp")

target0_date <- model_ner %>% filter(survey_date=="2023-03-31") %>% mutate(idp=NA, survey_date = last_acled_date)
model_ner <- rbind(model_ner, target0_date)

niger_dates <- as_date(c(niger_df$Date, last_acled_date))

date_ranges <- tibble(dates = niger_dates, time_code = seq(1,length(niger_dates),1))

all_dates <- left_join(tibble(dates = seq(as.Date("2018-09-30", format = "%Y-%m-%d"), 
                                          last_acled_date,1)), 
                       date_ranges, by = "dates") %>% 
  fill(time_code, .direction = "up")

recent_ner <- acled_niger %>% mutate(Date = as_date(EVENT_DATE)) %>% filter(Date %in% all_dates$dates)
recent_ner <- left_join(recent_ner %>% mutate(cha_date = as.character(Date)), all_dates %>% mutate(cha_date = as.character(dates)), by = ("cha_date"))
recent_sum_ner <- recent_ner %>% mutate(FATALITIES_civil = ifelse(EVENT_TYPE=="Violence against civilians", FATALITIES, 0),
                                        EVENTS_civil = ifelse(EVENT_TYPE=="Violence against civilians", 1, 0),
                                        EVENTS_battles = ifelse(EVENT_TYPE=="Battles", 1, 0),
                                        EVENTS_explo = ifelse(EVENT_TYPE=="Explosions/Remote violence", 1, 0),
                                        EVENTS_kidnap = ifelse(SUB_EVENT_TYPE=="Abduction/forced disappearance", 1, 0),
                                        EVENTS_looting = ifelse(SUB_EVENT_TYPE=="Looting/property destruction", 1, 0),
                                        ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                  (grepl("Islam",ACTOR1) | grepl("JNIM",ACTOR1) | ACTOR1 == "Katiba Macina" | ACTOR1 == "MUJAO: Movement for Unity and Jihad in West Africa"), 1, 0),
                                        ACTOR1_ethnic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                 (grepl("Ethnic",ACTOR1) | grepl("Communal",ACTOR1) | ACTOR1=="Dan Na Ambassagou"), 1, 0),
                                        ACTOR1_military = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                                                   (grepl("Military Forces",ACTOR1)), 1, 0)) %>% 
  group_by(ADMIN1, time_code) %>% summarise(fatal = sum(FATALITIES, na.rm = T), 
                                            max_fatal = max(FATALITIES),
                                            fatal_civil = sum(FATALITIES_civil),
                                            events_civil = sum(EVENTS_civil),
                                            events_battle = sum(EVENTS_battles),
                                            events_explo = sum(EVENTS_explo),
                                            events_kidnap = sum(EVENTS_kidnap),
                                            events_looting = sum(EVENTS_looting),
                                            events_islamic = sum(ACTOR1_islamic),
                                            events_ethnic = sum(ACTOR1_ethnic),
                                            events_military = sum(ACTOR1_military),
                                            .groups = "drop")

model_ner <- left_join(left_join(model_ner, date_ranges, by = c("survey_date" = "dates")), recent_sum_ner, by = c("Region" = "ADMIN1", "time_code")) %>% arrange(Region, time_code)



model_ner %<>% mutate(fatal = replace_na(fatal, 0), 
                      max_fatal = replace_na(max_fatal, 0), 
                      fatal_civil = replace_na(fatal_civil, 0), 
                      events_civil = replace_na(events_civil, 0), 
                      events_battle = replace_na(events_battle, 0),
                      events_explo = replace_na(events_explo, 0),
                      events_kidnap = replace_na(events_kidnap, 0),  
                      events_looting = replace_na(events_looting, 0),  
                      events_islamic = replace_na(events_islamic, 0), 
                      events_ethnic = replace_na(events_ethnic, 0), 
                      events_military = replace_na(events_military, 0))


model_ner <- model_ner %>% group_by(Region) %>% 
  mutate(lag_idp = lag(idp), 
         diff = ifelse(idp>lag(idp),idp - lag(idp), 0),
         previous_survey = lag(survey_date)) %>%
  filter(time_code != 1)



model_lga <- rbind(model_bfa %>% ungroup() %>% select(Region, survey_date, previous_survey, idp = PDI, lag_idp, diff, fatal, max_fatal, fatal_civil, events_civil, events_battle, events_explo, events_kidnap, events_looting, events_islamic, events_ethnic, events_military) %>% mutate(Country = "Burkina Faso"),
                   model_mli %>% select(Region = admin1, survey_date, previous_survey, idp, lag_idp, diff, fatal,  max_fatal, fatal_civil, events_civil, events_battle, events_explo, events_kidnap, events_looting, events_islamic, events_ethnic, events_military) %>% mutate(Country = "Mali"),
                   model_ner %>% select(Region, survey_date, previous_survey, idp, lag_idp, diff, fatal,  max_fatal, fatal_civil, events_civil, events_battle, events_kidnap, events_looting, events_explo, events_islamic, events_ethnic, events_military) %>% mutate(Country = "Niger"))


model_lga %<>% mutate(days_between = as.numeric(survey_date - previous_survey))


lga_pop_df <- rbind(mli_pop_df,bfa_pop_df, ner_pop_df)
lga_pop_df %<>% mutate(Region = ifelse(Region == "Plateau Central", "Plateau-Central", Region))


model_lga <- left_join(model_lga, lga_pop_df %>% select(Region, Population), by = "Region")

print("Constructed: conflict data")

acled_lga <- acled_africa %>% filter(COUNTRY %in% c("Mali","Niger","Burkina Faso"), YEAR>2012)


year_past_fatal <- acled_lga %>% 
  mutate(FATALITIES_civil = ifelse(EVENT_TYPE=="Violence against civilians", FATALITIES, 0),
         EVENTS_civil = ifelse(EVENT_TYPE=="Violence against civilians", 1, 0),
         EVENTS_conflict = ifelse(EVENT_TYPE %in% c("Explosions/Remote violence","Battles","Violence against civilians"), 1, 0),
         EVENTS_kidnap = ifelse(SUB_EVENT_TYPE=="Abduction/forced disappearance", 1, 0),
         EVENTS_looting = ifelse(SUB_EVENT_TYPE=="Looting/property destruction", 1, 0),
         ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                   (grepl("Islam",ACTOR1) | grepl("JNIM",ACTOR1) | ACTOR1 == "Katiba Macina" | ACTOR1 == "MUJAO: Movement for Unity and Jihad in West Africa"), 1, 0)) %>%
  group_by(COUNTRY, ADMIN1, EVENT_DATE) %>% 
  summarise(fatal = sum(FATALITIES),
            fatal_civil = sum(FATALITIES_civil),
            events_civil = sum(EVENTS_civil),
            events_conflict = sum(EVENTS_conflict),
            events_kidnap = sum(EVENTS_kidnap),
            events_looting = sum(EVENTS_looting),
            events_islamic = sum(ACTOR1_islamic), .groups = "drop") %>% 
  mutate(date = ymd(EVENT_DATE)) %>% select(-EVENT_DATE) %>% 
  complete(date = full_seq(date, period = 1), nesting(COUNTRY, ADMIN1) , fill = list(fatal = 0, fatal_civil = 0, events_civil =0, events_conflict = 0, events_kidnap = 0, events_looting = 0, events_islamic = 0)) %>% 
  group_by(ADMIN1) %>%
  mutate(fatal_rolling365 = zoo::rollapplyr(fatal, width = 365, FUN = sum, partial = TRUE),
         fatal_rolling182 = zoo::rollapplyr(fatal, width = 182, FUN = sum, partial = TRUE),
         fatal_rolling90 = zoo::rollapplyr(fatal, width = 90, FUN = sum, partial = TRUE),
         fatal_rolling45 = zoo::rollapplyr(fatal, width = 45, FUN = sum, partial = TRUE),
         fatal_civil_rolling365 = zoo::rollapplyr(fatal_civil, width = 365, FUN = sum, partial = TRUE),
         fatal_civil_rolling182 = zoo::rollapplyr(fatal_civil, width = 182, FUN = sum, partial = TRUE),
         fatal_civil_rolling90 = zoo::rollapplyr(fatal_civil, width = 90, FUN = sum, partial = TRUE),
         fatal_civil_rolling45 = zoo::rollapplyr(fatal_civil, width = 45, FUN = sum, partial = TRUE),
         events_civil_rolling365 = zoo::rollapplyr(events_civil, width = 365, FUN = sum, partial = TRUE),
         events_civil_rolling182 = zoo::rollapplyr(events_civil, width = 182, FUN = sum, partial = TRUE),
         events_civil_rolling90 = zoo::rollapplyr(events_civil, width = 90, FUN = sum, partial = TRUE),
         events_civil_rolling45 = zoo::rollapplyr(events_civil, width = 45, FUN = sum, partial = TRUE),
         events_conflict_rolling365 = zoo::rollapplyr(events_conflict, width = 365, FUN = sum, partial = TRUE),
         events_conflict_rolling182 = zoo::rollapplyr(events_conflict, width = 182, FUN = sum, partial = TRUE),
         events_conflict_rolling90 = zoo::rollapplyr(events_conflict, width = 90, FUN = sum, partial = TRUE),
         events_conflict_rolling45 = zoo::rollapplyr(events_conflict, width = 45, FUN = sum, partial = TRUE),
         events_kidnap182 = zoo::rollapplyr(events_kidnap, width = 182, FUN = sum, partial = TRUE),
         events_kidnap90 = zoo::rollapplyr(events_kidnap, width = 90, FUN = sum, partial = TRUE),
         events_looting182 = zoo::rollapplyr(events_looting, width = 182, FUN = sum, partial = TRUE),
         events_looting90 = zoo::rollapplyr(events_looting, width = 90, FUN = sum, partial = TRUE),
         events_islamic182 = zoo::rollapplyr(events_islamic, width = 182, FUN = sum, partial = TRUE),
         events_islamic90 = zoo::rollapplyr(events_islamic, width = 90, FUN = sum, partial = TRUE))  %>% mutate(year = year(date))



year_past_fatal %<>% mutate(ADMIN1 = ifelse(ADMIN1=="Segou", "Ségou", ADMIN1),
                            ADMIN1 = ifelse(ADMIN1 == "Plateau Central", "Plateau-Central", ADMIN1))


model_lga_ext <- left_join(model_lga, year_past_fatal %>% dplyr::select(-COUNTRY, -fatal,-fatal_civil,-events_civil, -events_conflict, -events_kidnap, -events_looting, -events_islamic), by = c("survey_date" = "date", "Region" = "ADMIN1"))

model_lga_ext %<>% mutate(fatal_rolling365_pop1 = (fatal_rolling365+1)/Population, fatal_rolling182_pop1 = (fatal_rolling182+1)/Population)


model_lga_ext <- left_join(model_lga_ext, lga_area, by = "Region") %>% mutate(Area = as.numeric(Area))

model_lga_ext %<>% mutate(conflict_intensity = (events_civil + events_battle + events_explo)/Area)

print("Constructed: rolling conflict data")



model_lga_ext %<>% mutate(month = month(survey_date), year = year(survey_date))

model_lga_more <- left_join(model_lga_ext, cadre_lga_full %>% select(Region = adm1_name, year = reference_year, month, share_phase35, pop_phase35), by = c("Region", "month", "year"))

model_lga_ext <- model_lga_more





coords <- st_coordinates(st_centroid(st_geometry(bfa_shp1)))
xx <- poly2nb(bfa_shp1)


xx_mat <- nb2mat(xx)


all_list <- list()

#bfa_map_dates <- c(bfa_dates, future_date)

N <- length(bfa_dates)

bfa_part <- model_lga_ext %>% filter(Country=="Burkina Faso") %>% arrange(survey_date, Region)

# bfa_dates fra n=2 og frem

for(i in 2:N){
  
  old_df <- bfa_part %>% filter(survey_date == bfa_dates[i]) 
  
  test_mat <- old_df %>% select(fatal) %>% as.matrix()
  new_df <- xx_mat %*% test_mat %>% as.data.frame() %>% rename(geo_fatal = fatal)
  
  test_mat2 <- old_df %>% select(fatal_civil) %>% as.matrix()
  new_df2 <- xx_mat %*% test_mat2 %>% as.data.frame() %>% rename(geo_fatal_civil = fatal_civil)
  
  test_mat3 <- old_df %>% select(conflict_intensity) %>% as.matrix()
  new_df3 <- xx_mat %*% test_mat3 %>% as.data.frame() %>% rename(geo_conflict_intensity = conflict_intensity)
  
  test_mat4 <- old_df %>% select(fatal_civil_rolling90) %>% as.matrix()
  new_df4 <- xx_mat %*% test_mat4 %>% as.data.frame() %>% rename(geo_fatal_civil_rolling90 = fatal_civil_rolling90)
  
  test_mat5 <- old_df %>% select(events_civil_rolling90) %>% as.matrix()
  new_df5 <- xx_mat %*% test_mat5 %>% as.data.frame() %>% rename(geo_events_civil_rolling90 = events_civil_rolling90)
  
  test_mat6 <- old_df %>% select(fatal_rolling90) %>% as.matrix()
  new_df6 <- xx_mat %*% test_mat6 %>% as.data.frame() %>% rename(geo_fatal_rolling90 = fatal_rolling90)
  
  test_mat7 <- old_df %>% select(events_islamic90) %>% as.matrix()
  new_df7 <- xx_mat %*% test_mat7 %>% as.data.frame() %>% rename(geo_events_islamic90 = events_islamic90)
  
  all_list[[i-1]] <- cbind(old_df, new_df, new_df2, new_df3, new_df4, new_df5, new_df6, new_df7)
}

more_df <- bind_rows(all_list)
row.names(more_df) <- NULL





coords_mli <- st_coordinates(st_centroid(st_geometry(mli_shp1)))
xx_mli <- poly2nb(mli_shp1)

xx_mat_mli <- nb2mat(xx_mli)



all_list <- list()

N <- length(mli_dates)

mli_part <- model_lga_ext %>% filter(Country=="Mali") %>% arrange(survey_date, Region)

# bfa_dates fra n=2 og frem

for(i in 2:N){
  
  old_df <- mli_part %>% filter(survey_date == mli_dates[i]) 
  
  test_mat <- old_df %>% select(fatal) %>% as.matrix()
  
  new_df <- xx_mat_mli %*% test_mat %>% as.data.frame() %>% rename(geo_fatal = fatal)
  
  test_mat2 <- old_df %>% select(fatal_civil) %>% as.matrix()
  
  new_df2 <- xx_mat_mli %*% test_mat2 %>% as.data.frame() %>% rename(geo_fatal_civil = fatal_civil)
  
  test_mat3 <- old_df %>% select(conflict_intensity) %>% as.matrix()
  
  new_df3 <- xx_mat_mli %*% test_mat3 %>% as.data.frame() %>% rename(geo_conflict_intensity = conflict_intensity)
  
  test_mat4 <- old_df %>% select(fatal_civil_rolling90) %>% as.matrix()
  new_df4 <- xx_mat_mli %*% test_mat4 %>% as.data.frame() %>% rename(geo_fatal_civil_rolling90 = fatal_civil_rolling90)
  
  test_mat5 <- old_df %>% select(events_civil_rolling90) %>% as.matrix()
  new_df5 <- xx_mat_mli %*% test_mat5 %>% as.data.frame() %>% rename(geo_events_civil_rolling90 = events_civil_rolling90)
  
  test_mat6 <- old_df %>% select(fatal_rolling90) %>% as.matrix()
  new_df6 <- xx_mat_mli %*% test_mat6 %>% as.data.frame() %>% rename(geo_fatal_rolling90 = fatal_rolling90)
  
  test_mat7 <- old_df %>% select(events_islamic90) %>% as.matrix()
  new_df7 <- xx_mat_mli %*% test_mat7 %>% as.data.frame() %>% rename(geo_events_islamic90 = events_islamic90)
  
  all_list[[i-1]] <- cbind(old_df, new_df, new_df2, new_df3, new_df4, new_df5, new_df6, new_df7)

}

more_mli <- bind_rows(all_list)


row.names(more_mli) <- NULL


ner_part <- model_lga_ext %>% filter(Country=="Niger") %>% arrange(Region, survey_date)

ner_part_tilla <- ner_part %>% filter(Region == "Tillaberi")
ner_part_taho <- ner_part %>% filter(Region == "Tahoua")

ner_part_tilla <- cbind(ner_part_tilla, ner_part_taho %>% select(geo_fatal = fatal, geo_fatal_civil = fatal_civil, geo_conflict_intensity = conflict_intensity, geo_fatal_civil_rolling90 = fatal_civil_rolling90, geo_events_civil_rolling90 = events_civil_rolling90, geo_fatal_rolling90 = fatal_rolling90, geo_events_islamic90 = events_islamic90))
ner_part_taho <- cbind(ner_part_taho, ner_part_tilla %>% select(geo_fatal = fatal, geo_fatal_civil = fatal_civil, geo_conflict_intensity = conflict_intensity, geo_fatal_civil_rolling90 = fatal_civil_rolling90, geo_events_civil_rolling90 = events_civil_rolling90, geo_fatal_rolling90 = fatal_rolling90, geo_events_islamic90 = events_islamic90))


full_lga_data <- rbind(more_df, more_mli, ner_part_tilla, ner_part_taho) %>% mutate(label = ifelse(diff>0, 1, 0))

print("Constructed: neighbour data")


full_lga_data <- left_join(full_lga_data, extra_sahel, by = "Region")

full_lga_data<- left_join(full_lga_data, vhi_monthly %>% select(year = Year, month = Month, Region = Province, three_month_mean), by = c("year","month","Region"))

full_lga_data %<>% mutate(log_days = log(days_between), log_lag_idp = log(lag_idp + 1), log_phase35 = log(pop_phase35+1),  log_fatal365 = log(fatal_rolling365 + 1))


# log_addi_phase35 is the logarithm of the number of food insecure people additional to the idp population.
full_lga_data %<>% mutate(log_addi_phase35 = log(ifelse(pop_phase35>lag_idp, pop_phase35-lag_idp, 1000)),
                          addi_phase35 = ifelse(pop_phase35>lag_idp, pop_phase35-lag_idp, 0))





price_data <- left_join(full_lga_data %>% filter(year>2016),
                        left_join(changed_price, map_names, by = "adm1_name") %>% 
                          mutate(month_ahead = (as.numeric(Month) + 1) %% 12) %>% 
                          mutate(month_ahead = ifelse(month_ahead==0,12, month_ahead),
                                 year_ahead = ifelse(month_ahead==1, Year+1,Year)) %>%
                          select(-Country,-Month),
                        by = c("month" = "month_ahead", "Region" = "Region", "year" = "year_ahead"))


print("Constructed: additional indicator data")
