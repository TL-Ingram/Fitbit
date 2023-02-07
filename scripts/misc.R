# Data processing scripts ------------------------------------------------------
ready_data <- here("concatonated_data", "dates_concatonated.csv") %>%
     read_csv() %>%
     as_tibble() %>%
     mutate(Date = ymd(Date))


# Sleep specific filtering -----------------------------------------------------
# Sleep onset and duration work
sleep_filter <- ready_data %>%
     filter(!(h_o_s >= 6 & h_o_s <= 19)) %>%
     filter(!(Sleep_hours < 4))
n_sleep = length(sleep_filter$Sleep_hours)
median_sleep = round(median(sleep_filter$Sleep_hours), digits = 2)
first_date_sleep = min(sleep_filter$Date)
first_date_steps = min(ready_data$Date)
q1_sleep = round(as.numeric(quantile(sleep_filter$Sleep_hours, 0.25)), 
                 digits = 2)
q3_sleep = round(as.numeric(quantile(sleep_filter$Sleep_hours, 0.75)), 
                 digits = 2)

# Sleep stage work
sleep_stage <- sleep_stage_summary(start_date = ymd("2023-01-01"),
                                end_date = Sys.Date() - 1)

mean_all_minutes <- sleep_stage %>%
        group_by(date) %>%
        summarize(all = sum(minutes)) %>%
        ungroup() %>%
        pull(all) %>%
        mean()

benchmark_deep = data.frame(low = 0.12*mean_all_minutes/60,
                            high = 0.23*mean_all_minutes/60,
                            stage = "deep")
benchmark_light = data.frame(low = 0.32*mean_all_minutes/60,
                             high = 0.68*mean_all_minutes/60,
                             stage = "light")
benchmark_rem = data.frame(low = 0.15*mean_all_minutes/60,
                           high = 0.25*mean_all_minutes/60,
                           stage = "rem")
stage_ready <- sleep_stage %>%
        filter(stage %in% c("deep", "light", "rem")) %>%
        group_by(date) %>%
        group_modify(~ adorn_totals(.x, where = "row")) %>%
        mutate(weekday = wday(date),
               weekday_label = wday(date, label = TRUE))

rm(mean_all_minutes)

# Imputation and labelling of missing step data --------------------------------
impute_steps <- ready_data %>%
     distinct(Date, .keep_all = TRUE) %>%
     pad(by = "Date") %>%
     mutate() %>%
     mutate(imputed = case_when(
          rHR >= 1 ~ "Organic",
          TRUE ~ "Imputed"
     )) %>%
     mutate(threeday = rollmean(Steps, k = 3, fill = NA)) %>%
     mutate(Date = ymd(Date))



date_range = as.character(seq(first_date_sleep, (Sys.Date()), "days"))
