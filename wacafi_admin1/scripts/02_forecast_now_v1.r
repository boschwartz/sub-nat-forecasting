


# This script produces the data for the forecasts and runs first the forecast to the end of the dataset and then the forecast into the future



first_date_bfa <- "2019-03-14"
first_date_mli <- "2017-01-31"
first_date_ner <- "2018-10-31"

first_data <- price_data %>% filter((survey_date == first_date_bfa & Country=="Burkina Faso") | (survey_date == first_date_mli & Country == "Mali") | (survey_date == first_date_ner & Country == "Niger")) %>% select(Region, first_date = survey_date, first.idp = idp)



all_data <- left_join(all_dates_sheet %>% select(previous_survey, first_survey, Country),
                      price_data %>% select(Country, Region, previous_survey, lag_idp), 
                      by = c("first_survey" = "previous_survey", "Country" = "Country")) %>% 
  rename(first.idp = lag_idp) %>% mutate(previous_survey = as_date(previous_survey), first_survey = as_date(first_survey))


forecast_data <- price_data %>% mutate(real_diff = idp-lag_idp)  %>% arrange(Region, survey_date) %>%  
  group_by(Region) %>% 
  mutate(lag2_date = lag(previous_survey, 1),
         days_between0_1 = as.numeric(survey_date - previous_survey),
         days_between1_2 = as.numeric(previous_survey - lag2_date)) %>%
  ungroup()


forecast_data <- left_join(forecast_data, all_data %>% select(-Country), by = c("Region","previous_survey")) %>%
  mutate(three_months_rate = (lag_idp - first.idp)/(as.numeric(previous_survey) - as.numeric(first_survey))*90,
         rate_average = three_months_rate/90*days_between)

forecast_data %<>% mutate(change_over_average = real_diff-rate_average)
forecast_data <- forecast_data %>% arrange(Region, survey_date) %>% group_by(Region) %>% 
  mutate(lag_change_over_average = lag(change_over_average, default = 0)) %>% ungroup()


forecast_data <- forecast_data %>% group_by(Region) %>%
  mutate(rn = row_number(),
         ave_events_civil_rolling90 = zoo::rollapplyr(events_civil_rolling90, width = 100, FUN = sum, partial = TRUE)/rn, change_events_civil_rolling90 = events_civil_rolling90 - ave_events_civil_rolling90,
         ave_events_civil_rolling45 = zoo::rollapplyr(events_civil_rolling45, width = 100, FUN = sum, partial = TRUE)/rn, change_events_civil_rolling45 = events_civil_rolling45 - ave_events_civil_rolling45,
         ave_events_islamic90 = zoo::rollapplyr(events_islamic90, width = 100, FUN = sum, partial = TRUE)/rn, change_events_islamic90 = events_islamic90 - ave_events_islamic90,
         ave_fatal_civil_rolling90 = zoo::rollapplyr(fatal_civil_rolling90, width = 100, FUN = sum, partial = TRUE)/rn, change_fatal_civil_rolling90 = fatal_civil_rolling90 - ave_fatal_civil_rolling90,
         ave_fatal_civil_rolling45 = zoo::rollapplyr(fatal_civil_rolling45, width = 100, FUN = sum, partial = TRUE)/rn, change_fatal_civil_rolling45 = fatal_civil_rolling45 - ave_fatal_civil_rolling45,
         ave_geo_fatal_civil_rolling90 = zoo::rollapplyr(geo_fatal_civil_rolling90, width = 100, FUN = sum, partial = TRUE)/rn, change_geo_fatal_civil_rolling90 = geo_fatal_civil_rolling90 - ave_geo_fatal_civil_rolling90,
         ave_geo_events_civil_rolling90 = zoo::rollapplyr(geo_events_civil_rolling90, width = 100, FUN = sum, partial = TRUE)/rn, change_geo_events_civil_rolling90 = geo_events_civil_rolling90 - ave_geo_events_civil_rolling90) %>%
  ungroup()


forecast_data$Region <- factor(forecast_data$Region, levels = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord",
                                                                "Centre-Ouest","Centre-Sud","Est","Hauts-Bassins","Nord","Plateau-Central",
                                                                "Sahel","Sud-Ouest","Bamako","Gao","Kayes","Kidal","Koulikoro",
                                                                "Menaka","Mopti","Ségou","Sikasso","Tombouctou",
                                                                "Tillaberi","Tahoua"))

forecast_data$Country <- factor(forecast_data$Country, levels = c("Burkina Faso","Mali","Niger"))



bfa_forecast_dates <- forecast_data %>% filter(Country=="Burkina Faso") %>% distinct(survey_date) %>% filter(survey_date != "2022-04-30", !(survey_date %in% c(last_acled_date, future_date)))
mli_forecast_dates <- forecast_data %>% filter(Country=="Mali") %>% distinct(survey_date) %>% filter(survey_date != "2021-12-30", !(survey_date %in% c(last_acled_date, future_date)))
ner_forecast_dates <- forecast_data %>% filter(Country=="Niger") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date, future_date)))


# create hyperparameter grid
hyper_grid <- expand.grid(
  learning_rate = c(0.001),
  max_depth = c(8),
  min_data_in_leaf = c(100),
  num_iterations = c(500)
)


j <- 1

train_dates_bfa <- bfa_forecast_dates[2:(nrow(bfa_forecast_dates)),]
train_dates_mli <- mli_forecast_dates[2:(nrow(mli_forecast_dates)),]
train_dates_ner <- ner_forecast_dates[2:(nrow(ner_forecast_dates)),]

train_data <- rbind(forecast_data %>% filter(Country == "Burkina Faso", survey_date %in% train_dates_bfa$survey_date),
                    forecast_data %>% filter(Country == "Mali", survey_date %in% train_dates_mli$survey_date),
                    forecast_data %>% filter(Country == "Niger", survey_date %in% train_dates_ner$survey_date)) 

last_data <- train_data %>% group_by(Region) %>% filter(survey_date==max(survey_date)) %>% ungroup() %>% 
  select(Region, last_date = previous_survey, last_idp = lag_idp)

first_to_last <- left_join(first_data, last_data, by = "Region") %>%
  mutate(full_rate = (last_idp - first.idp)/(as.numeric(last_date) - as.numeric(first_date))*90) %>% select(Region, full_rate)

train_data <- left_join(train_data, first_to_last, by = "Region") %>% mutate(average_diff = full_rate*days_between/90,
                                                                             change_over_average_diff = real_diff - average_diff)


test_data  <- forecast_data %>% filter(survey_date %in% last_acled_date)

last_data <- test_data %>% group_by(Region) %>% filter(survey_date==max(survey_date)) %>% ungroup() %>% 
  select(Region, last_date = previous_survey, last_idp = lag_idp)

first_to_last <- left_join(first_data, last_data, by = "Region") %>%
  mutate(full_rate = (last_idp - first.idp)/(as.numeric(last_date) - as.numeric(first_date))*90) %>% select(Region, full_rate)

test_data <- left_join(test_data, first_to_last, by = "Region") %>% mutate(average_diff = full_rate*days_between/90,
                                                                           change_over_average_diff = real_diff - average_diff)


training_data <- train_data  %>% select(Country, Region, days_between,fatal_civil_rolling45, events_civil_rolling45,fatal_civil_rolling365, events_civil_rolling90, events_conflict_rolling90, events_kidnap90,events_islamic90, share_phase35, pop_phase35, geo_fatal_civil_rolling90:geo_events_islamic90,
                                        PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price,three_months_rate, rate_average, lag_change_over_average, ave_events_civil_rolling90:change_geo_events_civil_rolling90,average_diff) 


testing_data <- test_data  %>% select(Country, Region, days_between,fatal_civil_rolling45, events_civil_rolling45,fatal_civil_rolling365, events_civil_rolling90, events_conflict_rolling90,events_kidnap90,events_islamic90,  share_phase35, pop_phase35, geo_fatal_civil_rolling90:geo_events_islamic90,
                                      PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price,three_months_rate, rate_average, lag_change_over_average, ave_events_civil_rolling90:change_geo_events_civil_rolling90,average_diff) 

dtrain <- lgb.Dataset(training_data %>% data.matrix(), label = train_data$change_over_average, categorical_feature = c(1L,2L))


model_05 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.5,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j],
    num_leaves = 257
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)

model_01 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.1,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)

model_09 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.9,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)


forecast_df <- test_data %>% select(Country, Region, survey_date, real_diff, idp, lag_idp, rate_average)

forecast_df$diff_05 <- predict(model_05, testing_data %>% data.matrix()) + forecast_df$rate_average

forecast_df$diff_01 <- predict(model_01, testing_data %>% data.matrix()) + forecast_df$rate_average
forecast_df$diff_09 <- predict(model_09, testing_data %>% data.matrix()) + forecast_df$rate_average

# Two forecasts:
forecast_df %<>% mutate(forecast_all = lag_idp + diff_05)
# Final forecast is an average of the two:
forecast_df %<>% mutate(forecast_high = forecast_all + diff_09 - diff_05,
                        forecast_low = forecast_all - (diff_05 - diff_01))



print_forecast <- forecast_df %>% select(Country, Region, survey_date, lag_idp, forecast_all, forecast_high, forecast_low)






all_data2 <- left_join(all_dates_sheet %>% select(survey_date,previous_survey, first_survey, Country),
                       price_data %>% select(Country, Region, previous_survey, lag_idp), 
                       by = c("first_survey" = "previous_survey", "Country" = "Country")) %>% 
  rename(first.idp = lag_idp) %>% mutate( survey_date = as_date(survey_date), previous_survey = as_date(previous_survey), first_survey = as_date(first_survey))



future_data_bfa <- left_join(left_join(price_data %>% filter(Country=="Burkina Faso"), bfa_training_dates, 
                                       by = c("survey_date" = "Date")) %>% filter(!is.na(Future.Date) | survey_date == last_acled_date), 
                             price_data %>% select(Region, Future.Date = survey_date, Future.idp = idp), 
                             by = c("Region", "Future.Date"))

future_data_mli <- left_join(left_join(price_data %>% filter(Country=="Mali"), mli_training_dates, 
                                       by = c("survey_date" = "Date")) %>% filter(!is.na(Future.Date) | survey_date == last_acled_date), 
                             price_data %>% select(Region, Future.Date = survey_date, Future.idp = idp), 
                             by = c("Region", "Future.Date"))


future_data_ner <- left_join(left_join(price_data %>% filter(Country=="Niger"), ner_training_dates, 
                                       by = c("survey_date" = "Date")) %>% filter(!is.na(Future.Date) | survey_date == last_acled_date), 
                             price_data %>% select(Region, Future.Date = survey_date, Future.idp = idp), 
                             by = c("Region", "Future.Date"))


all_future <- rbind(future_data_bfa,
                    future_data_mli,
                    future_data_ner) %>% 
  mutate(future.diff = Future.idp - idp)


all_future <- left_join(all_future, all_data2 %>% select(-Country, -previous_survey), by = c("Region","survey_date")) %>%
  mutate(three_months_rate = (idp - first.idp)/(as.numeric(survey_date) - as.numeric(first_survey))*90, 
         days_ahead = as.numeric(Future.Date) - as.numeric(survey_date),
         rate_average = three_months_rate/90*days_ahead)



all_future$Region <- factor(all_future$Region, levels = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord",
                                                          "Centre-Ouest","Centre-Sud","Est","Hauts-Bassins","Nord","Plateau-Central",
                                                          "Sahel","Sud-Ouest","Bamako","Gao","Kayes","Kidal","Koulikoro",
                                                          "Menaka","Mopti","Ségou","Sikasso","Tombouctou",
                                                          "Tillaberi","Tahoua"))

all_future$Country <- factor(all_future$Country, levels = c("Burkina Faso","Mali","Niger"))

all_future %<>% mutate(change_over_average = future.diff-rate_average)

all_future <- all_future %>% group_by(Region) %>%
  mutate(ave_events_civil_rolling90 = mean(events_civil_rolling90), change_events_civil_rolling90 = events_civil_rolling90 - ave_events_civil_rolling90,
         ave_fatal_civil_rolling90 = mean(fatal_civil_rolling90), change_fatal_civil_rolling90 = fatal_civil_rolling90 - ave_fatal_civil_rolling90,
         ave_geo_events_civil_rolling90 = mean(geo_events_civil_rolling90), change_geo_events_civil_rolling90 = geo_events_civil_rolling90 - ave_geo_events_civil_rolling90,
         ave_geo_fatal_civil_rolling90 = mean(geo_fatal_civil_rolling90), change_geo_fatal_civil_rolling90 = geo_fatal_civil_rolling90 - ave_geo_fatal_civil_rolling90) %>% ungroup()


all_future <- all_future %>% mutate(real_diff = idp-lag_idp)  %>% arrange(Region, survey_date) %>%  
  group_by(Region) %>% 
  mutate(lag2_date = lag(previous_survey, 1),
         days_between0_1 = as.numeric(survey_date - previous_survey),
         days_between1_2 = as.numeric(previous_survey - lag2_date),
         lag_three_months_rate = lag(three_months_rate)) %>%
  ungroup()

all_future <- all_future %>% group_by(Region) %>% 
  mutate(lag_diff = ifelse(!(survey_date == "2022-04-30" & Country == "Burkina Faso"),lag(real_diff, default = 0),NA),
         lag2_diff = ifelse(!(lag(survey_date) == "2022-04-30" & Country == "Burkina Faso"),lag(real_diff,2, default = 0), NA),
         total_lag_diff = lag_diff + lag2_diff,
  ) %>% ungroup()


all_future %<>% mutate(lag_change_over_average = total_lag_diff - three_months_rate/90*(days_between0_1+days_between1_2))



bfa_forecast_dates <- all_future %>% filter(Country=="Burkina Faso") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))
mli_forecast_dates <- all_future %>% filter(Country=="Mali") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))
ner_forecast_dates <- all_future %>% filter(Country=="Niger") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))



# create hyperparameter grid
hyper_grid <- expand.grid(
  learning_rate = c(0.001),
  max_depth = c(20),
  min_data_in_leaf = c(10),
  num_iterations = c(200),
  bagging_fraction = c(1),
  feature_fraction = c(1)
)



train_dates_bfa <- bfa_forecast_dates[2:(nrow(bfa_forecast_dates)),]
train_dates_mli <- mli_forecast_dates[2:(nrow(mli_forecast_dates)),]
train_dates_ner <- ner_forecast_dates[2:(nrow(ner_forecast_dates)),]




train_data <- rbind(all_future %>% filter(Country == "Burkina Faso", survey_date %in% train_dates_bfa$survey_date),
                    all_future %>% filter(Country == "Mali", survey_date %in% train_dates_mli$survey_date),
                    all_future %>% filter(Country == "Niger", survey_date %in% train_dates_ner$survey_date))

test_data  <- all_future %>% filter(survey_date %in% last_acled_date)


training_data <- train_data %>% select(Country, Region, idp, lag_idp,days_ahead, 
                                       Population, fatal_rolling365:fatal_rolling182_pop1, share_phase35:pop_phase35,geo_fatal_civil_rolling90:geo_events_islamic90,
                                       PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, three_months_rate, rate_average, ave_events_civil_rolling90:change_fatal_civil_rolling90, lag_change_over_average) 


testing_data <- test_data %>% select(Country, Region, idp, lag_idp, days_ahead, 
                                     Population, fatal_rolling365:fatal_rolling182_pop1, share_phase35:pop_phase35,geo_fatal_civil_rolling90:geo_events_islamic90,
                                     PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, three_months_rate, rate_average, ave_events_civil_rolling90:change_fatal_civil_rolling90, lag_change_over_average)

dtrain <- lgb.Dataset(training_data %>% data.matrix(), label = train_data$change_over_average, categorical_feature = c(1L,2L))
#dtest <- lgb.Dataset.create.valid(dtrain, data = testing_data$data, label = testing_data_0$label)
model_l2 <- lgb.train(
  params = list(
    objective = "regression",
    metric = "l2",
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j],
    bagging_fraction = hyper_grid$bagging_fraction[j],
    bagging_freq = 1,
    feature_fraction = hyper_grid$feature_fraction[j],
    num_leaves = 257
    
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)
model_05 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.5,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j],
    bagging_fraction = hyper_grid$bagging_fraction[j],
    bagging_freq = 1,
    feature_fraction = hyper_grid$feature_fraction[j],
    num_leaves = 257
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)



model_01 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.1,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)

model_09 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.9,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)



futurecast_df <- test_data %>% select(Country, Region, previous_survey, lag_idp, lag_three_months_rate)

futurecast_df$days_ahead <- as.numeric(future_date) - as.numeric(last_acled_date)

futurecast_df <- left_join(futurecast_df, forecast_df %>% select(Region, idp_synth = forecast_all, previous_high = forecast_high, previous_low = forecast_low), by = "Region")

futurecast_df %<>% mutate(rate_average = lag_three_months_rate/90*days_ahead)

futurecast_df$diff_05 <- predict(model_05, testing_data %>% data.matrix()) + futurecast_df$rate_average
futurecast_df$diff_l2 <- predict(model_l2, testing_data %>% data.matrix()) + futurecast_df$rate_average

futurecast_df$diff_01 <- predict(model_01, testing_data %>% data.matrix()) + futurecast_df$rate_average
futurecast_df$diff_09 <- predict(model_09, testing_data %>% data.matrix()) + futurecast_df$rate_average


futurecast_df %<>% mutate(forecast_05 = idp_synth + diff_05, forecast_l2 = idp_synth + diff_l2,
                          forecast_both = ifelse(idp_synth<50000,forecast_05, forecast_l2))


# Final forecast is an average of the two:
futurecast_df %<>% mutate(forecast_high = forecast_both - idp_synth + previous_high + diff_09 - diff_05,
                        forecast_low = forecast_both - idp_synth + previous_low - (diff_05 - diff_01),
                        forecast_low = ifelse(forecast_low<0, 0, forecast_low))



print_future <- futurecast_df %>% select(Country, Region, last_survey = previous_survey, idp = lag_idp, idp_synth, forecast_future = forecast_both, forecast_high, forecast_low)

print_future$last_data_date <- last_acled_date
print_future$future_date <- future_date

print_future  %<>% select(Country, Region, last_survey, last_data_date, future_date, idp, idp_synth, forecast_future, forecast_high, forecast_low)













