




# Load in packages

library("raster")
library("ggplot2")
library("dplyr")
library("magrittr")
library("stringi")
library("stringr")
library("lubridate")
library("reshape2")
library("tidyr")
library("patchwork")
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("brms")
library("tidybayes")
library("ggridges")
library("countrycode")
library("scales")
library("sf")
library("readxl")
library(purrr)
library(readr)
library("mgcv")
library(cmdstanr)
library(bayesplot)


# Load in data
# Set up the correct file directory

setwd("~/wacafi_project/data/")

bfa_idp2 <- read_xlsx("bfa_idp_adm2.xlsx")
bfa_idp2 %<>% mutate(Province = ifelse(Province == "Komondjari", "Komonjdjari", Province))
bfa_idp2 %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

## Niger data

ner_idp2 <- read_xlsx("niger_adm2_data.xlsx", sheet = 2)
ner_idp2 %<>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

ner_idp2 %<>% mutate(Province = ifelse(Province == "Ayérou", "Ayerou", Province))
ner_idp2 %<>% mutate(Province = ifelse(Province == "Gotheye", "Gothèye", Province))
ner_idp2 %<>% mutate(Province = ifelse(Province == "Tchinta", "Tchintabaraden", Province))

ner_idp2_extra1 <- ner_idp2 %>% filter(row_number()==1) %>% select(Date, PDI)
ner_idp2_extra <- data.frame(Region = c("Tahoua","Tahoua","Tahoua","Tahoua","Tahoua","Tahoua"), 
                             Province = c("Abalak","Birni N'Konni","Bouza","Illéla","Keita","Malbaza"), Date = ner_idp2_extra1$Date, PDI = 0)

ner_idp2 <- rbind(ner_idp2, ner_idp2_extra) %>% complete(nesting(Region, Province), Date, fill = list(PDI=0))



cadre <- read_excel("cadre_harmonise_caf_ipc(8).xlsx", sheet = 1)

cadre %<>% mutate(adm2_name = ifelse(adm1_name=="Bamako", "Bamako", adm2_name),
                  adm2_name = ifelse(adm2_name=="Koro_limitedaccess", "Koro", adm2_name),
                  adm2_name = ifelse(adm2_name=="Koro_accessible", "Koro", adm2_name),
                  adm2_name = ifelse(adm2_name=="Taoudenit", "Tombouctou", adm2_name))

rec_cadre_periods <- read_excel("recommended_cadre_periods.xlsx")

cadre_lga <- cadre %>% filter(adm0_pcod3 %in% c("BFA","MLI","NER")) %>% group_by(adm0_pcod3, adm2_name,adm2_pcod2, chtype, exercise_label, exercise_year, reference_label, reference_year) %>% 
  summarise(phase1 = sum(phase1), phase2 = sum(phase2), phase3 = sum(phase3), phase4 = sum(phase4), phase5 = sum(phase5), .groups = "drop")


cadre_lga <- inner_join(cadre_lga, rec_cadre_periods, by = c("adm0_pcod3","chtype", "exercise_label","exercise_year", "reference_year","reference_label")) %>% ungroup()

# Create average projection of Jan-May 2023:
cadre_synth <- cadre_lga %>% filter((reference_label == "Sep-Dec" & reference_year == 2022) | (reference_label == "Jun-Aug" & reference_year == 2023)) %>% group_by(adm0_pcod3, adm2_name,adm2_pcod2) %>% summarise(phase1 = mean(phase1), phase2 = mean(phase2), phase3 = mean(phase3), phase4 = mean(phase4), phase5 = mean(phase5), .groups = "drop")

cadre_synth %<>% mutate(chtype = "synth",exercise_label = "Sep-Dec", exercise_year = 2022, reference_label = "Jan-May", reference_year = 2023)

cadre_lga <- rbind(cadre_lga, cadre_synth)


cadre_lga_1_5 <- cadre_lga %>% distinct(adm2_name,adm2_pcod2, reference_label, reference_year) %>% filter(reference_label == "Jan-May") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 5))
cadre_lga_1_5$month <- rep(1:5, nrow(cadre_lga_1_5)/5)

cadre_lga_1_5 <- left_join(cadre_lga_1_5, cadre_lga %>% filter(reference_label == "Jan-May") %>% select(adm2_name,adm2_pcod2, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm2_name","adm2_pcod2", "reference_year"))


cadre_lga_6_8 <- cadre_lga %>% distinct(adm2_name,adm2_pcod2, reference_label, reference_year) %>% filter(reference_label == "Jun-Aug") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 3))
cadre_lga_6_8$month <- rep(6:8, nrow(cadre_lga_6_8)/3)

cadre_lga_6_8 <- left_join(cadre_lga_6_8, cadre_lga %>% filter(reference_label == "Jun-Aug") %>% select(adm2_name,adm2_pcod2, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm2_name","adm2_pcod2", "reference_year"))



cadre_lga_9_12 <- cadre_lga %>% distinct(adm2_name,adm2_pcod2, reference_label, reference_year) %>% filter(reference_label == "Sep-Dec") %>% select(-reference_label) %>% dplyr::slice(rep(1:n(), each = 4))
cadre_lga_9_12$month <- rep(9:12, nrow(cadre_lga_9_12)/4)

cadre_lga_9_12 <- left_join(cadre_lga_9_12, cadre_lga %>% filter(reference_label == "Sep-Dec") %>% select(adm2_name,adm2_pcod2, reference_year, phase1, phase2, phase3, phase4, phase5), by = c("adm2_name","adm2_pcod2", "reference_year"))


cadre_lga_full <- rbind(cadre_lga_1_5, cadre_lga_6_8, cadre_lga_9_12) %>% arrange(adm2_name, reference_year, month)

cadre_lga_full %<>% mutate(total = phase1 + phase2 + phase3 + phase4 + phase5, share_phase35 = (phase3 + phase4 + phase5)/total, pop_phase35 = (phase3 + phase4 + phase5) )


all_cadre <- cadre_lga_full %>% select(year = reference_year, Month = month, Province = adm2_name, adm2_pcod2, share_phase35, pop_phase35)
all_cadre <- rbind(all_cadre, all_cadre %>% filter(year==2023, Month==8) %>% mutate(Month= 9))
all_cadre <- rbind(all_cadre, all_cadre %>% filter(year==2023, Month==9) %>% mutate(Month= 10))





