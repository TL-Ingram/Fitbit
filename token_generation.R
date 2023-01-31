# Packages load ---------------------------------------------------------------------
suppressPackageStartupMessages({
  shelf(fitbitr, httpuv, tidyverse, lubridate, viridis, hrbrthemes, ggrepel,
        cowplot, scales, padr, zoo, here, lintr, styler, hms, mlr3, glue)
})
# lint("token_generation.R")
# style_file("token_generation.R")


# API load ---------------------------------------------------------------------
client_id <- "238GCK"
client_secret <- "f719c27b39cfdb748ec01b2597c1d899"
callback <- "http://localhost:1410/"
token <- generate_token(client_id, client_secret)


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
# Resting heart rate time-series
rHR_change
# Daily step count time-series
steps_plot
