# Packages load ---------------------------------------------------------------------
suppressPackageStartupMessages({
  shelf(fitbitr, httpuv, tidyverse, lubridate, viridis, hrbrthemes, ggrepel,
        cowplot, scales, padr, zoo, here, lintr, styler, hms, glue, 
        janitor, fitbitViz, caret, boot, e1071, glmnet, mgcv)
})
# lint("token_generation.R")
# style_file("token_generation.R")


# API load ---------------------------------------------------------------------
client_id <- "238GCK"
client_secret <- "f719c27b39cfdb748ec01b2597c1d899"
refresh_token = '0aaf03a055c37de15dc9c4d5fc5f42100e87b5da404e6066e30a1548b87db46f'
new_token <- refresh_token_app(client_id = client_id,
                              client_secret = client_secret,
                              refresh_token = refresh_token)
new_token <- new_token$access_token
callback <- "http://localhost:1410/"
token <- generate_token(client_id, client_secret)
token <- load_cached_token()

# API query for date "x" data --------------------------------------------------
# Function: 0 represents current day. Pull yesterday with -1 -------------------
y <- 1
while (y < 14) {
  date <- Sys.Date() - y

  # Fitbit tables queried
  try({
  summary <- activity_summary(date)
  sleep_summary <- sleep_summary(date)
  distance <- distance((date), (date - 1))
  })

  scale <- function(x, na.rm = FALSE) {
    (x / 60)
  }

  # Data processing: joining tables and cleaning
  x <- left_join(summary, sleep_summary, by = "date") %>%
    left_join(., distance, by = "date")
  x_clean <- x %>%
    select(date, calories_out, resting_heart_rate, steps, start_time, minutes_asleep, distance) %>%
    mutate(Distance = distance / 1.609, distance = NULL) %>%
    rename(
      "Date" = date,
      "Calories" = calories_out,
      "rHR" = resting_heart_rate,
      "Steps" = steps,
      "sSleep" = start_time,
      "Sleep_hours" = minutes_asleep
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
  write.csv(x_clean, here(
    "archive_daily_output",
    paste(date, "_data.csv", sep = "")
  ),
  row.names = FALSE
  )
  files_update_daily <- do.call(
    rbind,
    lapply(list.files(here("archive_daily_output"), full.names = TRUE, 
                      pattern = "2023"),
      read.csv,
      header = TRUE, sep = ","
    )
  )
  write.csv(files_update_daily, here(
    "concatonated_data",
    "dates_concatonated.csv"
  ),
  row.names = FALSE
  )
  print(y)
  y <- y + 1
}

# Source: data processing and plotting scripts ---------------------------------
source(here("scripts", "misc.R"))
source(here("scripts", "plots.R"))


# Plots ------------------------------------------------------------------------
# Sleep duration and time of onset
sleep_ovd
# Sleep duration distribution
sleep_dist
# Time in sleep stages by day of week
s_stage
# Resting heart rate time-series
rHR_change
# Daily step count time-series
steps_plot

#####
# hrv_data <- heart_rate_intraday(date = first_date_sleep, minutes = TRUE)
# hrv_day <- hrv_data %>%
#   mutate(minutes = (minute(time)),
#          five_min_index = 1:nrow(hrv_data) %/% 5) %>%
#   group_by(five_min_index) %>%
#   summarise(mean = mean(heart_rate),
#             SD = sd(heart_rate))
# 
# 
# date_range = as.character(seq(first_date_sleep, (Sys.Date() - 1), "days"))
# 
# list_hrv <- list()
# for (i in date_range) {
#     hrv_day <- fitbitViz::fitbit_data_type_by_date(user_id = "7B7QC2",
#                                     token = new_token,
#                                     date = i,
#                                     type = "hrv")
#     hrv_dt <- hrv_day %>%
#       mutate(Date = date(minute)) %>%
#       select(-(minute))
#     
#     list_hrv[[glue("{i}_hrv")]] <- hrv_dt
#     hrv_all <- bind_rows(list_hrv)
# }
# 
# hrv_all %>%
#   mutate(Date = date(time)) %>%
#   mutate(Date = as.factor(Date)) %>%
#   ggplot(aes(x = Date, y = rmssd)) +
#   geom_boxplot(outlier.alpha = 0.2)
# 
# benchmark_hrv <- hrv_all %>%
#   summarise(mean = mean(rmssd),
#             SD = sd(rmssd))
# hrv_mean <- hrv_all %>%
#   group_by(time) %>%
#   summarise(mean = mean(rmssd))
#   
# benchmark_test = data.frame(low = benchmark_hrv$mean - benchmark_hrv$SD,
#                             high = benchmark_hrv$mean + benchmark_hrv$SD,
#                             threshold = "thresholds")
# 
# hrv_mean %>%
#   ggplot(aes(x = Date, y = mean)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = benchmark_test$low, ymax = benchmark_test$high), 
#               fill = "#b7ded2", color = "#b7ded2", alpha = 0.5) +
#   theme_minimal()
# 
# full_data <- left_join(ready_data, hrv_mean, by = "Date")
# 
# hrv_mean %>%
#   ggplot(aes(x = Date, y = mean)) +
#   geom_line() +
#   geom_line(data = full_data, aes(x = Date, y = rHR)) +
#   geom_ribbon(aes(ymin = benchmark_test$low, ymax = benchmark_test$high), 
#               fill = "#b7ded2", color = "#b7ded2", alpha = 0.5) +
#   theme_minimal()
