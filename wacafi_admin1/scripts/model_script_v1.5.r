


setwd("~/wacafi_project/scripts/")


# Load packages and data - Can take a lot of time because the ACLED file is big
source("00_packages_data_v1.r")


# Last day of the dataset:
last_acled_date <- as.Date("2023-04-07", format = "%Y-%m-%d")
# Future scenario date:
future_date <- as.Date("2023-06-01", format = "%Y-%m-%d")


# Construct data.frame of indicators:

source("01_construct_frame_v1.r")


# Machine learning:

source("02_forecast_now_v1.r")


# The data.frame"print_future" contains:
# idp = latest IDP data, 
# idp_synth = the projected displacement at the date of the dataset
# forecast = forecast for the future scenario date




# Plot example:

plot_country <- "Burkina Faso"
plot_region <- "Cascades"
last_date <- as.Date("2023-02-28")

plot_country <- "Mali"
plot_region <- "Kidal"
last_date <- as.Date("2022-12-31")

plot_country <- "Niger"
plot_region <- "Tahoua"
last_date <- as.Date("2023-03-31")


region_data <- price_data %>% filter(year>2019, Country == plot_country, Region == plot_region)

region_projection0 <- print_forecast %>% filter( Country == plot_country, Region == plot_region)
region_projection1 <- print_future %>% filter( Country == plot_country, Region == plot_region)


plot_next <- tibble(survey_date = c(last_date, last_acled_date, future_date),
                    idp = c(region_projection1$idp, region_projection1$idp_synth, region_projection1$forecast_future),
                    ymin = c(region_projection1$idp, region_projection0$forecast_low, region_projection1$forecast_low),
                    ymax = c(region_projection1$idp, region_projection0$forecast_high, region_projection1$forecast_high))


#Change the date_title:
date_title = paste0("Forecast for 1st June 2023\n",
                    plot_region,", ",plot_country)

region_data %>%
  ggplot(aes(x=survey_date, y=idp))+
  geom_point(data=plot_next, aes(x=survey_date,y=idp), color ="dodgerblue", size=1.1)+
  geom_line(data=plot_next, aes(x=survey_date,y=idp), color ="dodgerblue", size=0.5) +
  geom_line(size=0.5, color = "#E94F35") + geom_point(size=1, color = "#E94F35") + theme_minimal() +
  scale_y_continuous(labels = comma, name = "IDP", limits = c(0,max(c(plot_next$ymax,region_data$idp), na.rm = T)*1.15), expand = c(0,1)) +
  theme(plot.background = element_rect(fill="white"),
        panel.grid.minor = element_blank(), panel.spacing.x = unit(0.2,"in"),
        legend.position = "bottom",
        legend.margin = margin(-10,0,0,0),
        axis.text = element_text(size=8),
        legend.text = element_text(size=12)) +
  labs(title=date_title) +
  xlab("") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_ribbon(data=plot_next, aes(x=survey_date,ymin=ymin,ymax=ymax), color = NA, fill="dodgerblue", alpha=0.4) 



ggsave(paste0(plot_region,"16032023.png"),device = "png", dpi = 300, width = 4, height = 2.8, units = "in")












