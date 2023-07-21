


setwd("~/wacafi_project/scripts/")


# Load packages and data - Can take a lot of time because the ACLED file is big
source("00_packages_data_v1.r")



last_acled_date <- as.Date("2023-02-10", format = "%Y-%m-%d")
# Future scenario date:
future_date <- as.Date("2023-04-30", format = "%Y-%m-%d")
#past_start_date <- last_acled_date - (future_date - last_acled_date)


# Construct data.frame of indicators:

source("01_construct_frame_v1.r")


# Machine learning:


forecast_data <- price_data %>% mutate(real_diff = idp-lag_idp)  %>% arrange(Region, survey_date) %>%  
  group_by(Region) %>% 
  mutate(lag2_date = lag(previous_survey, 1),
         days_between0_1 = as.numeric(survey_date - previous_survey),
         days_between1_2 = as.numeric(previous_survey - lag2_date)) %>%
  ungroup()



# Try to use size of previous shorter lags
forecast_data <- forecast_data %>% group_by(Region) %>% 
  mutate(lag_diff = ifelse(survey_date != "2022-04-30",lag(real_diff, default = 0)*30/days_between0_1,NA),
         lag2_diff = ifelse(lag(survey_date) !="2022-04-30",lag(real_diff,2, default = 0)*30/days_between1_2, NA),
         total_lag_diff = lag_diff + lag2_diff,
         lag1_fatal = lag(fatal, default = 0),
         lag2_fatal = lag(fatal, 2, default = 0),
         lag3_fatal = lag(fatal, 3, default = 0),
         ave_fatal = (lag1_fatal+lag2_fatal+lag3_fatal)/3,
         overall_fatal = sum(fatal, na.rm = T)) %>% ungroup()


forecast_data$Region <- factor(forecast_data$Region, levels = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord",
                                                                "Centre-Ouest","Centre-Sud","Est","Hauts-Bassins","Nord","Plateau-Central",
                                                                "Sahel","Sud-Ouest","Bamako","Gao","Kayes","Kidal","Koulikoro",
                                                                "Menaka","Mopti","Ségou","Sikasso","Tombouctou",
                                                                "Tillaberi","Tahoua"))

forecast_data$Country <- factor(forecast_data$Country, levels = c("Burkina Faso","Mali","Niger"))


bfa_forecast_dates <- forecast_data %>% filter(Country=="Burkina Faso") %>% distinct(survey_date) %>% filter(survey_date != "2022-04-30", !(survey_date %in% c(last_acled_date)))
mli_forecast_dates <- forecast_data %>% filter(Country=="Mali") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))
ner_forecast_dates <- forecast_data %>% filter(Country=="Niger") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))


# create hyperparameter grid
hyper_grid <- expand.grid(
  learning_rate = c( 0.01),
  max_depth = c(4),
  min_data_in_leaf = c( 40),
  num_iterations = c(500)
)

j <- 1

train_data <- rbind(forecast_data %>% filter(Country == "Burkina Faso", survey_date %in% bfa_forecast_dates$survey_date),
                    forecast_data %>% filter(Country == "Mali", survey_date %in% mli_forecast_dates$survey_date),
                    forecast_data %>% filter(Country == "Niger", survey_date %in% ner_forecast_dates$survey_date))



training_data <- train_data %>% select(Country, Region, lag_idp, fatal, max_fatal:events_military, days_between, 
                                       Population, fatal_rolling182:fatal_rolling182_pop1, conflict_intensity, share_phase35:geo_conflict_intensity,
                                       PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, lag_diff:overall_fatal) 

test_data  <- forecast_data %>% filter(survey_date==last_acled_date)

testing_data <- test_data %>% select(Country, Region, lag_idp, fatal, max_fatal:events_military, days_between, 
                                     Population, fatal_rolling182:fatal_rolling182_pop1, conflict_intensity, share_phase35:geo_conflict_intensity,
                                     PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, lag_diff:overall_fatal) 

dtrain <- lgb.Dataset(training_data %>% data.matrix(), label = train_data$real_diff, categorical_feature = c(1L,2L))

model_05 <- lgb.train(
  params = list(
    objective = "quantile",
    metric = "quantile",
    alpha = 0.5,
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
  )
  , data = dtrain, categorical_feature = c(1L,2L)
)

model_l2 <- lgb.train(
  params = list(
    objective = "regression",
    metric = "l2",
    max_depth = hyper_grid$max_depth[j],
    learning_rate = hyper_grid$learning_rate[j],
    min_data_in_leaf = hyper_grid$min_data_in_leaf[j],
    num_iterations = hyper_grid$num_iterations[j]
    
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



#tree_imp = lgb.importance(model_09, percentage = TRUE)
#lgb.plot.importance(tree_imp, top_n = 26L, measure = "Gain")


forecast_df <- test_data %>% select(Country, Region, survey_date, real_diff, idp, lag_idp)

forecast_df$diff_l2 <- predict(model_l2, testing_data %>% data.matrix())
forecast_df$diff_05 <- predict(model_05, testing_data %>% data.matrix())

forecast_df$diff_01 <- predict(model_01, testing_data %>% data.matrix())
forecast_df$diff_09 <- predict(model_09, testing_data %>% data.matrix())

# Two forecasts:
forecast_df %<>% mutate(forecast_05 = lag_idp + diff_05, forecast_l2 = lag_idp + diff_l2)
# Final forecast is an average of the two:
forecast_df %<>% mutate(forecast_all = ifelse(lag_idp<150000, forecast_05, (forecast_05+3*forecast_l2)/4),
                        forecast_high = forecast_all + diff_09 - diff_05,
                        forecast_low = forecast_all - (diff_05 - diff_01))



print_forecast <- forecast_df %>% select(Country, Region, survey_date, lag_idp, forecast_all, forecast_high, forecast_low)


# Next steps is then to add the variables that forecast 90-180 days into the future


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
                    future_data_ner) %>% mutate(future.diff = Future.idp - idp)





all_future$Region <- factor(all_future$Region, levels = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord",
                                                                "Centre-Ouest","Centre-Sud","Est","Hauts-Bassins","Nord","Plateau-Central",
                                                                "Sahel","Sud-Ouest","Bamako","Gao","Kayes","Kidal","Koulikoro",
                                                                "Menaka","Mopti","Ségou","Sikasso","Tombouctou",
                                                                "Tillaberi","Tahoua"))

all_future$Country <- factor(all_future$Country, levels = c("Burkina Faso","Mali","Niger"))



bfa_forecast_dates <- all_future %>% filter(Country=="Burkina Faso") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))
mli_forecast_dates <- all_future %>% filter(Country=="Mali") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))
ner_forecast_dates <- all_future %>% filter(Country=="Niger") %>% distinct(survey_date) %>% filter(!(survey_date %in% c(last_acled_date)))


rev_time <- 5:1


# create hyperparameter grid
hyper_grid <- expand.grid(
  learning_rate = c(0.01),
  max_depth = c(8),
  min_data_in_leaf = c( 20),
  num_iterations = c(200),
  bagging_fraction = c(1),
  feature_fraction = c(1),
  min_RMSE_l2 = 0,
  min_RMSE_05 = 0,
  min_RMSE_base = 0,
  min_MAPE_l2 = 0,                      # a place to dump results
  min_MAPE_05 = 0,                      # a place to dump results
  min_MAPE_base = 0                      # a place to dump results
)




# total number of combinations
nrow(hyper_grid)

for(j in 1:nrow(hyper_grid)){
  eval_df_list <- list()
  
  
  
  for(i in c(1:5)){
    
    train_dates_bfa <- bfa_forecast_dates[1:(nrow(bfa_forecast_dates)-rev_time[i]),]
    test_dates_bfa <- bfa_forecast_dates[(nrow(bfa_forecast_dates)-rev_time[i] + 1),]
    train_dates_mli <- mli_forecast_dates[1:(nrow(mli_forecast_dates)-rev_time[i]),]
    test_dates_mli <- mli_forecast_dates[(nrow(mli_forecast_dates)-rev_time[i] + 1),]
    train_dates_ner <- ner_forecast_dates[1:(nrow(ner_forecast_dates)-rev_time[i]),]
    test_dates_ner <- ner_forecast_dates[(nrow(ner_forecast_dates)-rev_time[i] + 1),]
    
    train_data <- rbind(all_future %>% filter(Country == "Burkina Faso", survey_date %in% train_dates_bfa),
                        all_future %>% filter(Country == "Mali", survey_date %in% train_dates_mli),
                        all_future %>% filter(Country == "Niger", survey_date %in% train_dates_ner))
    test_data  <- rbind(all_future %>% filter(Country == "Burkina Faso", survey_date %in% test_dates_bfa),
                        all_future %>% filter(Country == "Mali", survey_date %in% test_dates_mli),
                        all_future %>% filter(Country == "Niger", survey_date %in% test_dates_ner))
    
    
    
    training_data <- train_data %>% select(Country, Region, lag_idp, fatal, max_fatal:events_military, days_between, 
                                           Population, fatal_rolling182:fatal_rolling182_pop1, conflict_intensity, share_phase35:geo_conflict_intensity,
                                           PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, lag_diff:overall_fatal) 
    
    
    testing_data <- test_data %>% select(Country, Region, lag_idp, fatal, max_fatal:events_military, days_between, 
                                         Population, fatal_rolling182:fatal_rolling182_pop1, conflict_intensity, share_phase35:geo_conflict_intensity,
                                         PRV.UW.CHLD:INC.GINI, addi_phase35, indexed_price, lag_diff:overall_fatal) 
    
    dtrain <- lgb.Dataset(training_data %>% data.matrix(), label = train_data$real_diff, categorical_feature = c(1L,2L))
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
        num_leaves = 257,
        mc = c(0,0,1,rep(0, 32), 1, rep(0, 17)),
        monotone_constraints_method = "advanced"
        
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
    
    
    
    eval_df <- test_data %>% select(Region, Province, survey_date, diff, PDI, Future.PDI)
    
    
    eval_df$diff_l2 <- predict(model_l2, testing_data %>% data.matrix())
    eval_df$diff_05 <- predict(model_05, testing_data %>% data.matrix())
    
    
    
    eval_df_list[[i]] <- eval_df
    
    print(i)
    
  }
  
  #hyper_grid$optimal_trees1[j] <- which.min(gbm2$cv.error)
  #hyper_grid$optimal_trees2[j] <- which.min(gbm_p$cv.error)
  
  eval_df <- bind_rows(eval_df_list)
  
  
  rmse <- eval_df %>% mutate(forecast_l2 = PDI + diff_l2, forecast_05 = PDI + diff_05) %>% 
    mutate(se_l2 = (forecast_l2-Future.PDI)*(forecast_l2-Future.PDI),
           se_05 = (forecast_05-Future.PDI)*(forecast_05-Future.PDI),
           se_baseline = (Future.PDI-PDI)*(Future.PDI-PDI)) %>% 
    summarise(rmse_l2 = sqrt(mean(se_l2)), rmse_05 = sqrt(mean(se_05)), rmse_base = sqrt(mean(se_baseline)))
  
  hyper_grid$min_RMSE_l2[j] <- rmse$rmse_l2
  hyper_grid$min_RMSE_05[j] <- rmse$rmse_05
  hyper_grid$min_RMSE_base[j] <- rmse$rmse_base
  
  mape <- eval_df %>% mutate(forecast_l2 = PDI + diff_l2, forecast_05 = PDI + diff_05) %>% 
    mutate(APE_l2 = abs(forecast_l2-Future.PDI)/Future.PDI, APE_05 = abs(forecast_05-Future.PDI)/Future.PDI, APE_naive = abs(Future.PDI-PDI)/Future.PDI) %>% 
    summarise(MAPE_l2=mean(APE_l2), MAPE_05=mean(APE_05), MAPE_naive = mean(APE_naive))
  
  hyper_grid$min_MAPE_l2[j] <- mape$MAPE_l2
  hyper_grid$min_MAPE_05[j] <- mape$MAPE_05
  hyper_grid$min_MAPE_base[j] <- mape$MAPE_naive
  
  
  print(paste("Loop:",j))
}





# Eval

# Potentially set up a regression model to evaluate the relationship between skill and forecast models

eval_df %>% mutate(forecast_l2 = PDI + diff_l2, forecast_05 = PDI + diff_05, forecast_both = ifelse(PDI<25000, forecast_05, ifelse(PDI<50000,(forecast_05+forecast_l2)/2, forecast_l2))) %>%
  mutate(APE_l2 = abs(forecast_l2-Future.PDI)/Future.PDI, APE_05 = abs(forecast_05-Future.PDI)/Future.PDI, APE_naive = abs(Future.PDI-PDI)/Future.PDI, APE_both = abs(forecast_both-Future.PDI)/Future.PDI) %>%
  summarise(MAPE_l2=mean(APE_l2), MAPE_05=mean(APE_05), MAPE_naive = mean(APE_naive), MAPE_both=mean(APE_both))




eval_df %>% mutate(forecast_l2 = PDI + diff_l2, forecast_05 = PDI + diff_05, forecast_both = ifelse(PDI<25000, forecast_05, ifelse(PDI<50000,(forecast_05+forecast_l2)/2, forecast_l2))) %>%
  mutate(se_l2 = (forecast_l2-Future.PDI)*(forecast_l2-Future.PDI),
         se_05 = (forecast_05-Future.PDI)*(forecast_05-Future.PDI), 
         se_both = (forecast_both-Future.PDI)*(forecast_both-Future.PDI), 
         se_baseline = (Future.PDI-PDI)*(Future.PDI-PDI)) %>%
  summarise(rmse_l2 = sqrt(mean(se_l2)), rmse_05 = sqrt(mean(se_05)), rmse_both = sqrt(mean(se_both)), rmse_base = sqrt(mean(se_baseline)))

#

