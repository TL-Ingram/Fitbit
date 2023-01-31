# Data processing scripts ------------------------------------------------------
ready_data <- here("concatonated_data", "dates_concatonated.csv") %>%
     read_csv() %>%
     as_tibble() %>%
     mutate(Date = ymd(Date))


# Sleep specific filtering -----------------------------------------------------
sleep_filter <- ready_data %>%
     filter(!(h_o_s >= 6 & h_o_s <= 19)) %>%
     filter(!(Sleep_hours < 4))
n_sleep = length(sleep_filter$Sleep_hours)
median_sleep = round(median(sleep_filter$Sleep_hours), digits = 2)
first_date = min(sleep_filter$Date)
first_date_steps = min(ready_data$Date)
q1_sleep = round(as.numeric(quantile(sleep_filter$Sleep_hours, 0.25)), digits = 2)
q3_sleep = round(as.numeric(quantile(sleep_filter$Sleep_hours, 0.75)), digits = 2)


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