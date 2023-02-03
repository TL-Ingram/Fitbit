#####
hrv_data <- heart_rate_intraday(date = i, minutes = TRUE)
test <- hrv_data %>%
     group_by(mean = (row_number() -1) %/% 5) %>%
     mutate(mean = mean(heart_rate),
            time = ymd_hms(time)) %>%
     ungroup(.) %>%
     mutate(minutes = minute(time)) %>%
     filter(minutes %% 5 == 0) %>%
     select(-(minutes))


# Loop through each day and pull-clean hrv and hr ------------------------------
list_hrv <- list()
for (i in date_range) {
     # Pull full day heart rate variability
     hrv_day <- fitbitViz::fitbit_data_type_by_date(user_id = "7B7QC2",
                                                    token = new_token,
                                                    date = i,
                                                    type = "hrv")
     
     # Clean time var and create two dfs: daily mean and 5 min hrv
     hrv_dt <- hrv_day %>%
          mutate(time = date(minute)) %>%
          select(-(minute))
     hrv_5min <- hrv_day %>%
          mutate(time = ymd_hms(minute)) %>%
          select(-(minute))
     
     # Pull full day heart rate
     hr_dt <- heart_rate_intraday(date = i, minutes = TRUE)
     # modify heart rate df to match hrv
     hr_5min <- hr_dt %>%
          group_by(mean = (row_number() -1) %/% 5) %>%
          mutate(heart_rate = mean(heart_rate),
                 time = ymd_hms(time)) %>%
          ungroup(.) %>%
          mutate(minutes = minute(time)) %>%
          filter(minutes %% 5 == 0) %>%
          select(-(c(minutes, mean)))
     
     # Join hr and hrv by time - only sleeptime (hrv only measured +3 hrs sleep)
     hr_hrv <- left_join(hrv_5min, hr_5min, by = "time")
     
     # Add to list and bind all days together
     list_hrv[[glue("{i}_hrv")]] <- hr_hrv
     hrv_all <- bind_rows(list_hrv)
}


# Linear regression
hrv_all %>%
     ggplot(aes(x = rmssd, y = heart_rate)) +
     geom_point() +
     geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)

lm <- lm(heart_rate ~ rmssd, data = hrv_all)
summary(lm)
l_quad <- lm(heart_rate ~ rmssd + I(heart_rate^3), data=hrv_all)
summary(quad)









f <- function(x){
     return(0.0087606*x^2--0.0003897*x+27.9995968)
}

#####
hrv_all %>%
     mutate(Date = as.factor(Date)) %>%
     ggplot(aes(x = Date, y = rmssd)) +
     geom_boxplot(outlier.alpha = 0.2)

benchmark_hrv <- hrv_all %>%
     summarise(mean = mean(rmssd),
               SD = sd(rmssd))
hrv_mean <- hrv_all %>%
     group_by(Date) %>%
     summarise(mean = mean(rmssd))

benchmark_test = data.frame(low = benchmark_hrv$mean - benchmark_hrv$SD,
                            high = benchmark_hrv$mean + benchmark_hrv$SD,
                            threshold = "thresholds")

hrv_mean %>%
     ggplot(aes(x = Date, y = mean)) +
     geom_line() +
     geom_ribbon(aes(ymin = benchmark_test$low, ymax = benchmark_test$high), 
                 fill = "#b7ded2", color = "#b7ded2", alpha = 0.5) +
     theme_minimal()

full_data <- left_join(ready_data, hrv_mean, by = "Date")

hrv_mean %>%
     ggplot(aes(x = Date, y = mean)) +
     geom_line() +
     geom_line(data = full_data, aes(x = Date, y = rHR)) +
     geom_ribbon(aes(ymin = benchmark_test$low, ymax = benchmark_test$high), 
                 fill = "#b7ded2", color = "#b7ded2", alpha = 0.5) +
     theme_minimal()


# want to take day minutes in one col and match with hrv in another col
