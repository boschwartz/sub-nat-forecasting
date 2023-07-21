
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




setwd("~/wacafi_project/data/")

all_dates_sheet <- read_xlsx("all_dates.xlsx")

bfa_df <- read.csv("bfa_idp_data.csv") # Burkina admin1



bfa_idp2 <- read_xlsx("bfa_idp_adm2.xlsx")
bfa_idp2 %<>% mutate(Province = ifelse(Province == "Komondjari", "Komonjdjari", Province))
bfa_idp2 %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

bfa_idp2 <- bfa_idp2 %>% group_by(Province) %>% 
  mutate(lag1_date = lag(Date), lag2_date = lag(Date, 2),
         days_between0_1 = as.numeric(Date - lag1_date),
         days_between1_2 = as.numeric(lag1_date - lag2_date))



bfa_training_dates <- read_xlsx("bfa2_dates.xlsx")
bfa_training_dates %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Future.Date = as.Date(Future.Date, format = "%Y-%m-%d"))

mli_training_dates <- read_xlsx("mli1_dates.xlsx")
mli_training_dates %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Future.Date = as.Date(Future.Date, format = "%Y-%m-%d"))

ner_training_dates <- read_xlsx("ner_dates.xlsx")
ner_training_dates %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Future.Date = as.Date(Future.Date, format = "%Y-%m-%d"))


bfa_shp1 <- read_sf("bfa_admbnda_adm1_igb_20200323.shp")
bfa_shp1 <- cbind(bfa_shp1, st_coordinates(st_centroid(bfa_shp1)))
bfa_area <- tibble(Area = st_area(bfa_shp1)/1000000, Region = bfa_shp1$ADM1_FR)


wca2_shp <- read_sf("wca_admbnda_adm2_ocha_29062021.shp")
bfa_shp2 <- wca2_shp %>% filter(admin0Name %in% c("Burkina Faso") )
bfa12 <- data.frame(Region = bfa_shp2$admin1Name, Province = bfa_shp2$admin2Name)

mli_shp1 <- read_sf("mli_admbnda_adm1_1m_gov_20211110b.shp")
mli_shp1 <- cbind(mli_shp1, st_coordinates(st_centroid(mli_shp1)))
mli_area <- tibble(Area = st_area(mli_shp1)/1000000, Region = mli_shp1$admin1Name)

ner_shp1 <- read_sf("NER_adm01_feb2018.shp")
ner_shp1 %<>% mutate(adm_01 = ifelse(adm_01=="Tillabéri","Tillaberi", adm_01))
ner_shp1 <- cbind(ner_shp1, st_coordinates(st_centroid(ner_shp1)))
ner_area <- tibble(Area = st_area(ner_shp1)/1000000, Region = ner_shp1$adm_01)

lga_area <- rbind(bfa_area, mli_area, ner_area)

mli_pop_adm2 <- read_csv("mli_pop_adm2(1).csv", show_col_types = FALSE)
bfa_pop_adm1 <- read_csv("bfa_adm1_pop.csv", show_col_types = FALSE)
ner_pop_adm1 <- read_csv("ner_pplp_adm1_2018.csv", show_col_types = FALSE) %>% mutate(Region = paste0(substr(Région,1,1), tolower(substr(Région,2,80))))

mli_pop_df <- mli_pop_adm2 %>% filter(!is.na(admin2Name_fr)) %>% 
  mutate(admin1Name_fr = ifelse(admin2Name_fr=="Ménaka","Menaka",admin1Name_fr)) %>% 
  group_by(Region = admin1Name_fr) %>% summarise(Population = sum(Ensemble)) %>% mutate(Country = "Mali")
bfa_pop_df <- bfa_pop_adm1 %>% select(Country = ADM0_EN, Region = ADM1_FR, Population = Total) %>%
  mutate(Region = ifelse(Region == "Plateau Central", "Plateau-Central", Region))
ner_pop_df <- ner_pop_adm1 %>% select(Country = pays, Region, Population)



niger_data <- read_excel("Niger IDP Compilation - V2.xlsx", sheet = 2)
niger_df <- niger_data %>% filter(!is.na(Date), !is.na(Tillaberi))


mali_iom <- read_csv("mali_iom.csv", show_col_types = FALSE)


mali_data <- mali_iom %>% group_by(survey_date, admin1) %>% 
  summarise(idp = sum(total_idp, na.rm = T), 
            ret_idp = sum(ret_idp, na.rm = T), 
            ret_ref = sum(ret_refugees, na.rm = T), 
            total_ret = sum(total_ret, na.rm = T), .groups = "drop") 

mali_march22 <- read_csv("mali_idp_march22.csv", show_col_types = FALSE)


print("Loaded: IDP data")

#acled_africa <- read_xlsx("Africa_1997-2023_Apr07.xlsx")

acled_africa %<>% mutate(ADMIN1 = ifelse(ADMIN1=="Segou", "Ségou", ADMIN1))
acled_africa %<>% filter(!(COUNTRY=="Mali" & ADMIN1 == "Boucle du Mouhoun"))


acled_burkina <- acled_africa %>% filter(COUNTRY=="Burkina Faso", YEAR>2011)
acled_mali <- acled_africa %>% filter(COUNTRY=="Mali", YEAR>2011)
acled_niger <- acled_africa %>% filter(COUNTRY=="Niger", YEAR>2011)

print("Loaded: ACLED")



# Load in Cadre Harmonise




cadre <- read_excel("cadre_harmonise_caf_ipc(7).xlsx", sheet = 1)

cadre %<>% mutate(adm1_name = ifelse(adm2_name=="Menaka","Menaka",adm1_name))

rec_cadre_periods <- read_excel("recommended_cadre_periods.xlsx")

cadre_lga <- cadre %>% filter(adm0_pcod3 %in% c("MLI","BFA","NER")) %>% group_by(adm0_pcod3, adm1_name, chtype, exercise_label, exercise_year, reference_label, reference_year) %>% 
  summarise(phase1 = sum(phase1), phase2 = sum(phase2), phase3 = sum(phase3), phase4 = sum(phase4), phase5 = sum(phase5), .groups = "drop")

cadre_lga %<>% mutate(adm1_name = ifelse(adm1_name=="Segou", "Ségou", adm1_name))
cadre_lga %<>% mutate(adm1_name = ifelse(adm1_name=="Plateau Central", "Plateau-Central", adm1_name))

cadre_lga <- inner_join(cadre_lga, rec_cadre_periods, by = c("adm0_pcod3","chtype", "exercise_label","exercise_year", "reference_year","reference_label")) %>% ungroup()


# Create average projection of Jan-May 2023:
cadre_synth <- cadre_lga %>% filter((reference_label == "Sep-Dec" & reference_year == 2022) | (reference_label == "Jun-Aug" & reference_year == 2023)) %>% group_by(adm0_pcod3, adm1_name) %>% summarise(phase1 = mean(phase1), phase2 = mean(phase2), phase3 = mean(phase3), phase4 = mean(phase4), phase5 = mean(phase5), .groups = "drop")

cadre_synth %<>% mutate(chtype = "synth",exercise_label = "Sep-Dec", exercise_year = 2022, reference_label = "Jan-May", reference_year = 2023)

cadre_lga <- rbind(cadre_lga, cadre_synth)

cadre_lga_1_5 <- cadre_lga %>% distinct(adm1_name, reference_label, reference_year) %>% filter(reference_label == "Jan-May") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 5))
cadre_lga_1_5$month <- rep(1:5, nrow(cadre_lga_1_5)/5)

cadre_lga_1_5 <- left_join(cadre_lga_1_5, cadre_lga %>% filter(reference_label == "Jan-May") %>% select(adm1_name, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm1_name", "reference_year"))


cadre_lga_6_8 <- cadre_lga %>% distinct(adm1_name, reference_label, reference_year) %>% filter(reference_label == "Jun-Aug") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 3))
cadre_lga_6_8$month <- rep(6:8, nrow(cadre_lga_6_8)/3)

cadre_lga_6_8 <- left_join(cadre_lga_6_8, cadre_lga %>% filter(reference_label == "Jun-Aug") %>% select(adm1_name, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm1_name", "reference_year"))



cadre_lga_9_12 <- cadre_lga %>% distinct(adm1_name, reference_label, reference_year) %>% filter(reference_label == "Sep-Dec") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 4))
cadre_lga_9_12$month <- rep(9:12, nrow(cadre_lga_9_12)/4)

cadre_lga_9_12 <- left_join(cadre_lga_9_12, cadre_lga %>% filter(reference_label == "Sep-Dec") %>% select(adm1_name, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm1_name", "reference_year"))


cadre_lga_full <- rbind(cadre_lga_1_5, cadre_lga_6_8, cadre_lga_9_12) %>% arrange(adm1_name, reference_year, month)

cadre_lga_full %<>% mutate(total = phase1 + phase2 + phase3 + phase4 + phase5, share_phase35 = (phase3 + phase4 + phase5)/total, pop_phase35 = (phase3 + phase4 + phase5) )


print("Loaded: Cadre Harmonisé")

# INFORM data

extra_sahel <- read_excel("inform_sahel_data.xlsx")

extra_sahel %<>% mutate(Region = ifelse(Region == "Plateau Central", "Plateau-Central", Region),
                        Region = ifelse(Region == "Segou", "Ségou", Region))



# Data on vegetation health

vhi1 <- read.csv("vhi_adm1_dekad_data(14)_NER.csv")

vhi1_monthly <- vhi1 %>% group_by(Province, Year, Month) %>% summarise(vhi = mean(Data), .groups = "drop") %>% mutate(date = as.Date(paste0(Year,"-",Month,"-1"))) 
vhi1_monthly$Country <- "Niger"

vhi2 <- read.csv("vhi_adm1_dekad_data(13)_BFA.csv")

vhi2_monthly <- vhi2 %>% group_by(Province, Year, Month) %>% summarise(vhi = mean(Data), .groups = "drop") %>% mutate(date = as.Date(paste0(Year,"-",Month,"-1"))) 
vhi2_monthly$Country <- "Burkina Faso"


vhi3 <- read.csv("vhi_adm1_dekad_data(12)_MLI.csv")

vhi3_monthly <- vhi3 %>% group_by(Province, Year, Month) %>% summarise(vhi = mean(Data), .groups = "drop") %>% mutate(date = as.Date(paste0(Year,"-",Month,"-1"))) 
vhi3_monthly$Country <- "Mali"


vhi_monthly <- rbind(vhi1_monthly, vhi2_monthly, vhi3_monthly)


vhi_monthly <- vhi_monthly %>% group_by(Province) %>%
  mutate(three_month_mean = (lag(vhi) + lag(vhi,2) + lag(vhi,3))/3) %>% ungroup()

vhi_monthly %<>% mutate(Province = case_when(Province == "Boucle Du Mouhoun" ~ "Boucle du Mouhoun",
                                             Province == "Centre-est" ~ "Centre-Est",
                                             Province == "Centre-nord" ~ "Centre-Nord",
                                             Province == "Centre-ouest" ~ "Centre-Ouest",
                                             Province == "Centre-sud" ~ "Centre-Sud",
                                             Province == "Hauts-bassins" ~ "Hauts-Bassins",
                                             Province == "Sud-ouest" ~ "Sud-Ouest",
                                             Province == "Segou" ~ "Ségou",
                                             Province == "Plateau Central" ~ "Plateau-Central",
                                             TRUE ~ Province))
vhi_added <- vhi_monthly %>% filter(Province == "Gao") %>% mutate(Province="Menaka")
vhi_monthly <- rbind(vhi_monthly, vhi_added)


print("Loaded: INFORM + VHI")

# Food prices from WFP



distinct_markets_bfa <- read_xlsx("distinct_markets.xlsx", sheet = 1)
distinct_markets_mli <- read_xlsx("distinct_markets.xlsx", sheet = 2)
distinct_markets_ner <- read_xlsx("distinct_markets.xlsx", sheet = 3)

wfp_bfa <- read_csv("WFP_2023Feb06_BurkinaFaso_FoodPricesData.csv",show_col_types = FALSE)
wfp_bfa <- left_join(wfp_bfa, distinct_markets_bfa %>% select(adm1_name, Market = mkt_name), by = "Market")
wfp_bfa <- wfp_bfa %>% group_by(Market) %>% mutate(ave_price3 = zoo::rollapply(Price, width = 3, partial = T, function(x) mean(x, na.rm = T)),
                                                   ave_price5 = zoo::rollapply(Price, width = 5, partial = T, function(x) mean(x, na.rm = T))) %>%
  ungroup() %>% mutate(real_price = case_when(is.na(Price) & is.na(ave_price3) ~ ave_price5,
                                              is.na(Price) ~ ave_price3,
                                              TRUE ~ Price))

price_region_bfa <- wfp_bfa %>% filter(Commodity=="Millet", `Price Type`=="Retail") %>% group_by(Country,Year, Month, adm1_name) %>% summarise(mean_price = mean(real_price, na.rm = T), .groups = "drop")


wfp_mli <- read_csv("WFP_2023Feb06_Mali_FoodPricesData.csv", show_col_types = FALSE)
wfp_mli <- left_join(wfp_mli, distinct_markets_mli %>% select(adm1_name, Market = mkt_name), by = "Market")
wfp_mli <- wfp_mli %>% group_by(Market) %>% mutate(ave_price3 = zoo::rollapply(Price, width = 3, partial = T, function(x) mean(x, na.rm = T)),
                                                   ave_price5 = zoo::rollapply(Price, width = 5, partial = T, function(x) mean(x, na.rm = T))) %>%
  ungroup() %>% mutate(real_price = case_when(is.na(Price) & is.na(ave_price3) ~ ave_price5,
                                              is.na(Price) ~ ave_price3,
                                              TRUE ~ Price))

wfp_mli %<>% mutate(adm1_name = ifelse(Market=="Aglal", "Tombouctou", adm1_name))
wfp_mli %<>% mutate(adm1_name = ifelse(Market=="Agazrahane", "Menaka", adm1_name))
wfp_mli %<>% mutate(adm1_name = ifelse(Market=="Anderamboukane", "Menaka", adm1_name))
wfp_mli %<>% mutate(adm1_name = ifelse(Market=="Menaka", "Menaka", adm1_name))


price_region_mli <- wfp_mli %>% filter(Commodity=="Millet", `Price Type`=="Retail")  %>% group_by(Country,Year, Month, adm1_name) %>% summarise(mean_price = mean(real_price, na.rm = T), .groups = "drop")


wfp_ner <- read_csv("WFP_2023Feb06_Niger_FoodPricesData.csv", show_col_types = FALSE)

wfp_ner <- left_join(wfp_ner, distinct_markets_ner %>% select(adm1_name, Market = mkt_name), by = "Market")
wfp_ner <- wfp_ner %>% group_by(Market) %>% mutate(ave_price3 = zoo::rollapply(Price, width = 3, partial = T, function(x) mean(x, na.rm = T)),
                                                   ave_price5 = zoo::rollapply(Price, width = 5, partial = T, function(x) mean(x, na.rm = T))) %>%
  ungroup() %>% mutate(real_price = case_when(is.na(Price) & is.na(ave_price3) ~ ave_price5,
                                              is.na(Price) ~ ave_price3,
                                              TRUE ~ Price))

price_region_ner <- wfp_ner %>% filter(Commodity=="Millet", `Price Type`=="Retail")  %>% group_by(Country,Year, Month, adm1_name) %>% summarise(mean_price = mean(real_price, na.rm = T), .groups = "drop")


map_names <- data.frame(adm1_name = c("Boucle Du Mouhoun","Cascades","Centre","Centre-est","Centre-nord","Centre-ouest","Centre-sud",
                                      "Est","Hauts-bassins","Nord","Plateau Central","Sahel","Sud-ouest",
                                      "Bamako","Gao","Kayes","Kidal","Koulikoro","Mopti","Segou","Sikasso","Tombouctou","Menaka",
                                      "Agadez","Diffa","Dosso","Maradi","Niamey","Tahoua","Tillaberi","Zinder"),
                        ADM1_FR = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord","Centre-Ouest","Centre-Sud",
                                    "Est","Hauts-Bassins","Nord","Plateau Central","Sahel","Sud-Ouest",
                                    "Bamako","Gao","Kayes","Kidal","Koulikoro","Mopti","Segou","Sikasso","Tombouctou","Menaka",
                                    "Agadez","Diffa","Dosso","Maradi","Niamey","Tahoua","Tillabéri","Zinder"),
                        Region = c("Boucle du Mouhoun","Cascades","Centre","Centre-Est","Centre-Nord","Centre-Ouest","Centre-Sud",
                                   "Est","Hauts-Bassins","Nord","Plateau-Central","Sahel","Sud-Ouest",
                                   "Bamako","Gao","Kayes","Kidal","Koulikoro","Mopti","Ségou","Sikasso","Tombouctou","Menaka",
                                   "Agadez","Diffa","Dosso","Maradi","Niamey","Tahoua","Tillaberi","Zinder"))




price_lga <-  rbind(price_region_mli, price_region_bfa, price_region_ner)



index_price20 <- price_lga %>% filter(Year==2021, Month == "01") %>% select(adm1_name, index = mean_price)

changed_price <- left_join(price_lga %>% filter(Year>2016), index_price20, by = "adm1_name") %>% 
  mutate(indexed_price = mean_price*100/index,
         date_price = as.Date(paste0(Year, "-",Month,"-1"), format = "%Y-%m-%d"),
         label_date = format(date_price, "%b %Y"))



print("Loaded: WFP Food price data")

# Potentially include a rm function to remove everything above that is superfluous

setwd("~/wacafi_project/scripts/")
