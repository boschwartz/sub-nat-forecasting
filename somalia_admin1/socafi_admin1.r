

library("raster")
library("ggplot2")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")
library("rvest")
library("stringi")
library("stringr")
library("lubridate")
library("reshape2")
library("tidyr")
library("patchwork")
library("lightgbm")

library("ggridges")
library("countrycode")

library("scales")
library("sf")
sf::sf_use_s2(FALSE)
library("rnaturalearth")
library("caret")
library("readxl")
library(purrr)
library(readr)
library("mgcv")
library("spdep")




setwd("~/Somalia/socafi/")


som_dates <- read_xlsx("som_dates.xlsx") %>% mutate(date = ymd(date), future.date = ymd(future.date))

prmn <- read_xlsx("UNHCR-PRMN-Displacement-Dataset(5).xlsx")

names(prmn) <- c("month_end","yearweek","region_arrival","district_arrival","region_departure","district_departure","reason","priority_need","idp")

prmn %<>% mutate(date = as.Date(month_end, format = "%d/%m/%Y"), month = month(date), year = year(date))


som_shp1 <- read_sf("som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp")

som12 <- data.frame(Region = som_shp1$ADM1_EN)




idp_df <- prmn %>% group_by(region_arrival, date, month, year, reason) %>% summarise(idp = sum(idp), .groups = "drop")

idp_df %<>% filter(reason == "Conflict/Insecurity")

idp_df %<>% complete(region_arrival, nesting(date, month, year), reason, fill = list(idp = 0))


# Try to use size of previous shorter lags
idp_df <- idp_df %>% group_by(region_arrival) %>% 
  mutate(lag_idp = lag(idp),
         lag2_idp = lag(idp,2),
         total_lag_diff = lag_idp + lag2_idp) %>% ungroup()



idp_df <- idp_df %>% group_by(region_arrival) %>% 
  mutate(mean6_roll_diff = zoo::rollapplyr(lag_idp, width = 6, FUN = mean, na.rm = T, partial = TRUE),
         mean12_roll_diff = zoo::rollapplyr(lag_idp, width = 12, FUN = mean, na.rm = T, partial = TRUE)) %>% ungroup()


forecast_data_som <- left_join(left_join(idp_df, som_dates, 
                                         by = "date") %>% filter(!is.na(future.date)), 
                               idp_df %>% select(region_arrival, future.date = date, future.idp = idp), 
                               by = c("region_arrival", "future.date"))

#distinct_dates <- idp_df %>% distinct(date)
#write_xlsx(distinct_dates, path = "som_dates.xlsx")




#acled_africa <- read_xlsx("Africa_1997-2023_Jun02-1.xlsx")



acled_som <- acled_africa %>% filter(COUNTRY %in% c("Somalia"), YEAR>2015) %>% mutate(MONTH = month(EVENT_DATE))




year_past_fatal <- acled_som %>% 
  mutate(FATALITIES_civil = ifelse(EVENT_TYPE=="Violence against civilians", FATALITIES, 0),
         EVENTS_civil = ifelse(EVENT_TYPE=="Violence against civilians", 1, 0),
         EVENTS_conflict = ifelse(EVENT_TYPE %in% c("Explosions/Remote violence","Battles","Violence against civilians"), 1, 0),
         EVENTS_kidnap = ifelse(SUB_EVENT_TYPE=="Abduction/forced disappearance", 1, 0),
         EVENTS_looting = ifelse(SUB_EVENT_TYPE=="Looting/property destruction", 1, 0),
         ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                   grepl("Al Shabaab",ACTOR1), 1, 0)) %>%
  group_by(COUNTRY, ADMIN1, EVENT_DATE) %>% 
  summarise(fatal = sum(FATALITIES),
            fatal_civil = sum(FATALITIES_civil),
            events_civil = sum(EVENTS_civil),
            events_conflict = sum(EVENTS_conflict),
            events_kidnap = sum(EVENTS_kidnap),
            events_looting = sum(EVENTS_looting),
            events_islamic = sum(ACTOR1_islamic), .groups = "drop") %>% 
  mutate(date = ymd(EVENT_DATE)) %>% dplyr::select(-EVENT_DATE) %>% 
  complete(date = full_seq(date, period = 1), nesting(COUNTRY, ADMIN1) , 
           fill = list(fatal = 0, fatal_civil = 0, events_civil =0, events_conflict = 0, events_kidnap = 0, events_looting = 0, events_islamic = 0)) %>% 
  group_by(ADMIN1) %>%
  mutate(fatal_rolling365 = zoo::rollapplyr(fatal, width = 365, FUN = sum, partial = TRUE),
         fatal_rolling90 = zoo::rollapplyr(fatal, width = 90, FUN = sum, partial = TRUE),
         fatal_rolling45 = zoo::rollapplyr(fatal, width = 45, FUN = sum, partial = TRUE),
         fatal_civil_rolling365 = zoo::rollapplyr(fatal_civil, width = 365, FUN = sum, partial = TRUE),
         fatal_civil_rolling90 = zoo::rollapplyr(fatal_civil, width = 90, FUN = sum, partial = TRUE),
         fatal_civil_rolling45 = zoo::rollapplyr(fatal_civil, width = 45, FUN = sum, partial = TRUE),
         events_civil_rolling365 = zoo::rollapplyr(events_civil, width = 365, FUN = sum, partial = TRUE),
         events_civil_rolling90 = zoo::rollapplyr(events_civil, width = 90, FUN = sum, partial = TRUE),
         events_civil_rolling45 = zoo::rollapplyr(events_civil, width = 45, FUN = sum, partial = TRUE),
         events_conflict_rolling365 = zoo::rollapplyr(events_conflict, width = 365, FUN = sum, partial = TRUE),
         events_conflict_rolling90 = zoo::rollapplyr(events_conflict, width = 90, FUN = sum, partial = TRUE),
         events_conflict_rolling45 = zoo::rollapplyr(events_conflict, width = 45, FUN = sum, partial = TRUE),
         events_kidnap182 = zoo::rollapplyr(events_kidnap, width = 182, FUN = sum, partial = TRUE),
         events_kidnap90 = zoo::rollapplyr(events_kidnap, width = 90, FUN = sum, partial = TRUE),
         events_looting90 = zoo::rollapplyr(events_looting, width = 90, FUN = sum, partial = TRUE),
         events_islamic90 = zoo::rollapplyr(events_islamic, width = 90, FUN = sum, partial = TRUE))  %>% mutate(year = year(date))




setup_df <- left_join(forecast_data_som, year_past_fatal %>% select(-COUNTRY, year) %>% ungroup(), by = c("date" = "date","region_arrival"="ADMIN1"))




library(spdep)
xx <- poly2nb(som_shp1)


xx_mat <- nb2mat(xx)


all_list <- list()
bfa_part <- setup_df

som_forecast_dates <- bfa_part %>% distinct(date)

bfa_part %<>% arrange(factor(region_arrival, levels = som12$region_arrival), date)

N <- nrow(som_forecast_dates)


for(i in 1:N){
  
  old_df <- bfa_part %>% filter(date == som_forecast_dates$date[i]) 
  
  test_mat <- old_df %>% select(fatal_rolling45) %>% as.matrix()
  
  new_df <- xx_mat %*% test_mat %>% as.data.frame() %>% rename(geo_fatal_rolling45 = fatal_rolling45)
  
  test_mat2 <- old_df %>% select(fatal_civil_rolling45) %>% as.matrix()
  
  new_df2 <- xx_mat %*% test_mat2 %>% as.data.frame() %>% rename(geo_fatal_civil_rolling45 = fatal_civil_rolling45)
  
  test_mat3 <- old_df %>% select(fatal_rolling365) %>% as.matrix()
  
  new_df3 <- xx_mat %*% test_mat3 %>% as.data.frame() %>% rename(geo_fatal_rolling365 = fatal_rolling365)
  
  test_mat4 <- old_df %>% select(fatal_rolling90) %>% as.matrix()
  
  new_df4 <- xx_mat %*% test_mat4 %>% as.data.frame() %>% rename(geo_fatal_rolling90 = fatal_rolling90)
  
  test_mat5 <- old_df %>% select(events_civil_rolling90) %>% as.matrix()
  
  new_df5 <- xx_mat %*% test_mat5 %>% as.data.frame() %>% rename(geo_events_civil_rolling90 = events_civil_rolling90)
  
  test_mat6 <- old_df %>% select(lag_idp) %>% as.matrix()
  
  new_df6 <- xx_mat %*% test_mat6 %>% as.data.frame() %>% rename(geo_lag_diff = lag_idp)
  
  test_mat7 <- old_df %>% select(events_kidnap90) %>% as.matrix()
  
  new_df7 <- xx_mat %*% test_mat7 %>% as.data.frame() %>% rename(geo_events_kidnap90 = events_kidnap90)
  
  test_mat8 <- old_df %>% select(events_looting90) %>% as.matrix()
  
  new_df8 <- xx_mat %*% test_mat8 %>% as.data.frame() %>% rename(geo_events_looting90 = events_looting90)
  
  all_list[[i]] <- cbind(old_df, new_df, new_df2, new_df3, new_df4, new_df5, new_df6, new_df7, new_df8)
}

more_df <- bind_rows(all_list)
row.names(more_df) <- NULL



forecast_data <- more_df










forecast_data$region_arrival <- factor(forecast_data$region_arrival, levels = som12$Region)





bfa_forecast_dates <- forecast_data %>% distinct(date)

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
    
    train_dates_som <- som_forecast_dates[1:(nrow(som_forecast_dates)-rev_time[i]),]
    test_dates_som <- som_forecast_dates[(nrow(som_forecast_dates)-rev_time[i] + 1),]
    
    
    train_data <- forecast_data %>% filter(date %in% train_dates_som$date)
    test_data  <- forecast_data %>% filter(date %in% test_dates_som$date)
    
    
    
    training_data <- train_data %>% select(region_arrival, idp, lag_idp:mean12_roll_diff, fatal_rolling365:events_conflict_rolling45, 
                                           geo_fatal_rolling45:geo_events_looting90) 
    
    
    testing_data <- test_data %>% select(region_arrival, idp, lag_idp:mean12_roll_diff, fatal_rolling365:events_conflict_rolling45, 
                                         geo_fatal_rolling45:geo_events_looting90) 
    
    dtrain <- lgb.Dataset(training_data %>% data.matrix(), label = train_data$future.idp, categorical_feature = c(1L))
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
      , data = dtrain, categorical_feature = c(1L)
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
      , data = dtrain, categorical_feature = c(1L)
    )
    
    
    
    eval_df <- test_data %>% select(region_arrival, idp, future.idp)
    
    
    eval_df$idp_l2 <- predict(model_l2, testing_data %>% data.matrix())
    eval_df$idp_05 <- predict(model_05, testing_data %>% data.matrix())
    
    simple_displ <- rbind(train_data,test_data) %>% group_by(region_arrival) %>% summarise(all_idp = sum(idp), n= n()) %>% mutate(average_change = all_idp/n)
    
    eval_df <- left_join(eval_df, simple_displ %>% select(region_arrival, average_change), by = "region_arrival")
    
    
    
    eval_df_list[[i]] <- eval_df
    
    print(i)
    
  }
  
  #hyper_grid$optimal_trees1[j] <- which.min(gbm2$cv.error)
  #hyper_grid$optimal_trees2[j] <- which.min(gbm_p$cv.error)
  
  eval_df <- bind_rows(eval_df_list)
  
  
  rmse <- eval_df %>% 
    mutate(se_l2 = (idp_l2-future.idp)*(idp_l2-future.idp),
           se_05 = (idp_05-future.idp)*(idp_05-future.idp),
           se_simp = (average_change-future.idp)*(average_change-future.idp),
           se_baseline = (future.idp-idp)*(future.idp-idp)) %>% 
    summarise(rmse_l2 = sqrt(mean(se_l2)), rmse_05 = sqrt(mean(se_05)), rmse_simp = sqrt(mean(se_simp)), rmse_base = sqrt(mean(se_baseline)))
  
  hyper_grid$min_RMSE_l2[j] <- rmse$rmse_l2
  hyper_grid$min_RMSE_05[j] <- rmse$rmse_05
  hyper_grid$min_RMSE_base[j] <- rmse$rmse_base
  
  mape <- eval_df %>% 
    mutate(APE_l2 = abs(idp_l2-future.idp)/future.idp, APE_05 = abs(idp_05-future.idp)/future.idp, APE_simp = abs(average_change-future.idp)/future.idp, APE_naive = abs(future.idp-idp)/future.idp) %>% 
    summarise(MAPE_l2=mean(APE_l2), MAPE_05=mean(APE_05), MAPE_simp=mean(APE_simp), MAPE_naive = mean(APE_naive))
  
  hyper_grid$min_MAPE_l2[j] <- mape$MAPE_l2
  hyper_grid$min_MAPE_05[j] <- mape$MAPE_05
  hyper_grid$min_MAPE_base[j] <- mape$MAPE_naive
  
  
  print(paste("Loop:",j))
}




eval_df %>% mutate(forecast_l2 = idp_l2, forecast_05 = idp_05, 
                   forecast_simp = average_change) %>%
  mutate(APE_l2 = abs(forecast_l2-future.idp)/future.idp, APE_05 = abs(forecast_05-future.idp)/future.idp,
         APE_simp = abs(forecast_simp - future.idp)/future.idp) %>%
  summarise(MAPE_l2=mean(APE_l2), MAPE_05=mean(APE_05), MAPE_simp = mean(APE_simp))




eval_df %>% mutate(forecast_l2 = idp_l2, forecast_05 = idp_05, 
                   forecast_simp = average_change) %>%
  mutate(se_l2 = (forecast_l2-future.idp)*(forecast_l2-future.idp),
         se_05 = (forecast_05-future.idp)*(forecast_05-future.idp), 
         se_simp = (forecast_simp-future.idp)*(forecast_simp-future.idp)) %>%
  summarise(rmse_l2 = sqrt(mean(se_l2)), rmse_05 = sqrt(mean(se_05)), rmse_simp = sqrt(mean(se_simp)))

#