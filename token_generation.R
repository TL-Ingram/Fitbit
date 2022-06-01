# Pkg load ---------------------------------------------------------------------
suppressPackageStartupMessages({
library(fitbitr)
library(httpuv)
library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(scales)
library(cowplot)
library(padr)
library(mlr)
library(zoo)
library(here)
library(lintr)
library(styler)
}
)
lint("token_generation.R")
style_file("token_generation.R")


# API load ---------------------------------------------------------------------
client_id <- "238GCK"
client_secret <- "f719c27b39cfdb748ec01b2597c1d899"
callback <- "http://localhost:1410/"
token <- generate_token(client_id, client_secret)


# API query for date "x" data ----------------------------------------------------
add_dates <- function(x) {
  date <- Sys.Date() + x
  
  #Fitbit tables queried
  summary <- activity_summary(date)
  sleep_summary <- sleep_summary(date)
  distance <- distance((date), (date - 1))

  scale <- function(x, na.rm = FALSE) {
    (x / 60)
  }

  # Data processing: joining tables and cleaning
  x <- left_join(summary, sleep_summary, by = "date") %>%
    left_join(., distance, by = "date")
  x_clean <- x %>%
    select(1, 5, 9, 11, 14, 19, 23) %>%
    mutate(Distance = distance / 1.609, distance = NULL) %>%
    rename(
      Date = 1,
      Calories = 2,
      rhr = 3,
      Steps = 4,
      sSleep = 5,
      "Sleep_hours" = 6
    ) %>%
    mutate_at("Sleep_hours", scale) %>%
    mutate(sSleep = ymd_hms(sSleep)) %>%
    mutate(h_o_s = hour(sSleep)) %>%
    mutate(h_o_s = case_when(
      h_o_s >= 19 ~ "Early",
      h_o_s <= 19 ~ "Late",
      TRUE ~ "NA"
    ))

  # Output "date" file locally and concatonate with historic dates
  write.csv(x_clean, here("archive_daily_output", 
                          paste(date, "_data.csv", sep = "")), 
            row.names = FALSE)
  files_update_daily <- do.call(
    rbind,
    lapply(list.files(here("archive_daily_output"), full.names = TRUE),
      read.csv,
      header = TRUE, sep = ","
    )
  )
  write.csv(files_update_daily, here("concatonated_data", 
                                     "dates_concatonated.csv"), 
            row.names = FALSE)
}
# Function: 0 represents current day. Pull yesterday with -1 ---------------
add_dates(0)


# Data processing: adjusting data type for "Date" variable ---------------------
ready_data <- here("concatonated_data", "dates_concatonated.csv") %>%
  read_csv() %>%
  as_tibble() %>%
  mutate(Date = ymd(Date))

# Plot: sleep duration and onset
sleep_1 <- ready_data %>%
  filter(!(h_o_s >= 6 & h_o_s <= 19)) %>%
  filter(!(Sleep_hours < 4)) %>%
  ggplot(aes(sSleep, Sleep_hours, label = sSleep, colour = h_o_s)) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("steelblue", "black")) +
  geom_text(hjust = 0, vjust = 1.5, size = 4.5, 
            aes(label = format(sSleep, format = "%H:%M"))) +
  expand_limits(y = 4:10) +
  scale_y_continuous(breaks = seq(4, 10, 1)) +
  scale_x_datetime(breaks = date_breaks("24 hours"), 
                   minor_breaks = date_breaks("24 hours"), 
                   labels = date_format("%b %d")) +
  geom_hline(aes(yintercept = 7), 
             size = 1, linetype = "dashed", colour = "#482677FF") +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 10,
    axis_title_size = 14
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line("grey50"),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.ticks.x = element_line(colour = "grey50", size = 0.2)
  ) +
  labs(
    x = "Time of sleep onset",
    y = "Sleep duration (hours)",
    colour = "Sleep time",
    title = "Sleep onset time against sleep duration",
    subtitle = "April 2022 : Present",
    caption = "Horizontal dotted line represents subjectively appreciated \  
    `good` sleep time quantity"
  )

# Plot: sleep duration distribution
sleep_2 <- ready_data %>%
  filter(!(h_o_s >= 6 & h_o_s <= 19)) %>%
  filter(!(Sleep_hours < 4)) %>%
  ggplot(aes(Sleep_hours)) +
  geom_density() +
  geom_vline(aes(xintercept = median(Sleep_hours)), size = 1) +
  geom_vline(aes(xintercept = quantile(Sleep_hours, 0.25)), 
             linetype = "dashed", alpha = 0.4) +
  geom_vline(aes(xintercept = quantile(Sleep_hours, 0.75)), 
             linetype = "dashed", alpha = 0.4) +
  geom_text(aes(x = 8.5, label = paste("mean = ", round(mean(Sleep_hours)), 
                                       digits = 2), y = 0.45), 
            colour = "black", angle = 0) +
  geom_text(aes(x = 8.5, label = paste("n = ", length(Sleep_hours)), 
                y = 0.42), colour = "black", angle = 0) +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 10,
    axis_title_size = 14
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line("grey50"),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.ticks.x = element_line(colour = "grey50", size = 0.2)
  ) +
  labs(
    x = "Sleep duration (hours)",
    y = "Density",
    caption = "Lines depict mean (solid), 1st and 3rd quartiles (dashed). \  
    Naps removed.",
    title = "Density distribution of sleep duration",
    subtitle = "April 2022 : Present"
  )

# Facet plot: bind sleep plots rowwise
sleep_plots <- plot_grid(sleep_1, sleep_2, nrow = 2, labels = "") %>%
  plot_grid()
save_plot(plot = sleep_plots, here("plots", "sleep_plots.tiff"), 
          dpi = 300, base_width = 16, base_height = 12)

# Plot: resting heart rate
rhr_plot <- ready_data %>%
  distinct(Date, .keep_all = TRUE) %>%
  ggplot(aes(Date, rhr)) +
  geom_point(alpha = 0.8) +
  geom_line(size = 1.5, alpha = 0.5) +
  scale_x_date(date_labels = "%b-%d", breaks = "1 day") +
  scale_y_continuous(breaks = seq(50, 66, 1)) +
  geom_vline(aes(xintercept = as.Date("2022-04-11")), 
             linetype = "dashed", alpha = 0.5, colour = "green", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-18")), 
             linetype = "dashed", alpha = 0.5, colour = "green", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-19")), 
             linetype = "dashed", alpha = 0.5, colour = "red", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-26")), 
             linetype = "dashed", alpha = 0.5, colour = "red", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-05-03")), 
             linetype = "dashed", alpha = 0.5, colour = "blue", size = 1) +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 10,
    axis_title_size = 14
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90"),
    axis.text.x = element_text(
      angle = 25,
      vjust = 1.0, hjust = 1.0
    ),
    axis.line.x = element_line("grey50"),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.ticks.x = element_line(colour = "grey50", size = 0.2)
  ) +
  labs (
    caption = "Vertical dotted lines represent notable events: \  
    green = holiday; red = illness; blue = notable event. \  
    Blue 2022-05-03 = Started new job.",
    title = "Mean daily resting heart rate",
    subtitle = "April 2022 : Present"
)

# Data processing: imputation and labelling of missing step data ---------------
impute_steps <- ready_data %>%
  distinct(Date, .keep_all = TRUE) %>%
  pad(by = "Date")

imp <- impute(impute_steps, cols = list(Steps = imputeMean()))
impute_steps <- as_tibble(imp$data) %>%
  mutate(imputed = case_when(
    rhr >= 1 ~ "Organic",
    TRUE ~ "Imputed"
  )) %>%
  mutate(threeday = rollmean(Steps, k = 3, fill = NA)) %>%
  mutate(Date = ymd(Date))

# Plot: step count by day
steps_plot <- impute_steps %>%
  distinct(Date, .keep_all = TRUE) %>%
  ggplot(aes(Date, Steps)) +
  geom_col(alpha = 0.2, aes(fill = imputed)) +
  geom_path(aes(Date, threeday), size = 1.5, alpha = 0.5) +
  scale_fill_manual(values = c("grey65", "grey25")) +
  scale_x_date(date_labels = "%b-%d", breaks = "1 day") +
  geom_vline(aes(xintercept = as.Date("2022-04-11")), 
             linetype = "dashed", alpha = 0.5, colour = "green", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-18")), 
             linetype = "dashed", alpha = 0.5, colour = "green", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-19")), 
             linetype = "dashed", alpha = 0.5, colour = "red", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-04-26")), 
             linetype = "dashed", alpha = 0.5, colour = "red", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-05-03")), 
             linetype = "dashed", alpha = 0.5, colour = "blue", size = 1) +
  geom_hline(aes(yintercept = 10000), 
             size = 1, linetype = "dashed", colour = "#482677FF", alpha = 0.3) +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 10,
    axis_title_size = 14
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(
      angle = 25,
      vjust = 1.0, hjust = 1.0
    ),
    axis.line.x = element_line("grey50"),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.ticks.x = element_line(colour = "grey50", size = 0.2),
    legend.position = "bottom"
  ) +
  labs(
    fill = "Data source",
    caption = "Vertical dotted lines represent notable events: \  
    green = holiday; red = illness; blue = notable event.
          Blue 2022-05-03 = Started new job.
          No data collected between 16-04 : 21:04 - \  
    values imputed using mean steps for entire period.
          Path represents three-day moving average (mean).",
    title = "Daily step count",
    subtitle = "April 2022 : Present"
  )

# Facet plot: bind rhr and step plots rowwise
rhr_steps_plots <- plot_grid(rhr_plot, steps_plot, nrow = 2, labels = "") %>%
  plot_grid()
save_plot(plot = rhr_steps_plots, here("plots", "rhr_steps_plots.tiff"), 
          dpi = 300, base_width = 16, base_height = 12)
