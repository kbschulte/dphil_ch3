## AQ Channel Analysis (DPhil Chapter 3) - Data Viz eval##
## 18/8/2022 ##
## Kayla Schulte ##

#load packages

packages <- c("dplyr", "ggplot2", "anytime", "lubridate", "httr", "jsonlite", "reshape2", "tibble", "tidyverse", 
              "sf", "geosphere", "leaflet", "ggforce")

lapply(packages, require, character.only = TRUE)

#Load data

load("/Users/kaylaschulte/Research/AQ_channel_analysis/data/data_viz.rda")

head(master_df_clean_AQI_test)

#Plot data
#US EPA AQI

timeseries_AQI_US <- master_df_clean_AQI_test %>%
  group_by(API) %>%
  filter(API %in% c("API4","API6")) %>%
  ggplot(aes(x = datetime, y = AQI, color = API)) +
  geom_rect(ymin = -Inf, ymax = 50, 
            xmin = -Inf, xmax = Inf, fill = 'lightgreen', alpha = 0.5) + 
  geom_rect(ymin = 50, ymax = 100, 
            xmin = -Inf, xmax = Inf, fill = 'yellow', alpha = 0.9) +
  geom_rect(ymin = 100, ymax = 150, 
            xmin = -Inf, xmax = Inf, fill = 'orange', alpha = 0.9) +
  geom_rect(ymin = 150, ymax = 200, 
            xmin = -Inf, xmax = Inf, fill = 'red', alpha = 0.9) +
  geom_rect(ymin = 200, ymax = 300, 
            xmin = -Inf, xmax = Inf, fill = 'maroon', alpha = 0.9) +
  geom_rect(ymin = 300, ymax = 500, 
            xmin = -Inf, xmax = Inf, fill = 'purple', alpha = 0.5) +
  geom_point(alpha = 0.4) + 
  facet_wrap_paginate(loc_sampled ~ .) + 
  ylab((expression(paste(
    "AQI")))) +
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.spacing = unit(2, "lines"), strip.text.y.right = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_text(vjust = -5), plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), date_labels = "%m-%d-%y") + xlab("Date") + ylab("AQI (US EPA)") + 
  scale_color_manual(values = c('Black', "grey50")) + ylim(0, 500) +
  scale_alpha_manual(name = "AQ Threshold",
                     values = c("good", "moderate","unhealthy for sensitive groups", "unhealthy", "very unhealthy", "hazardous"),
                     breaks = c("good", "moderate","unhealthy for sensitive groups", "unhealthy", "very unhealthy", "hazardous"))

timeseries_AQI_US
