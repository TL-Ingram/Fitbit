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
rm(hr_5min, hr_dt, hrv_5min, hrv_day, hrv_dt)


##### --------------------------------------------------------------------------
# set.seed(10)

# Filter to independent/predictor variables only. Remove outliers.
lm_data <- hrv_all %>%
        select(., c("rmssd", "heart_rate")) %>%
        filter(., rmssd < 80)
# lm_data <- as_tibble(scale.default(lm_data, center = T))

# Separate training, validation, test sets
spec = c(train = .8, test = .1, validate = .1)
groups = sample(cut(
        seq(nrow(lm_data)), 
        nrow(hrv_all)*cumsum(c(0,spec)),
        labels = names(spec)
))
groups_list = split(lm_data, groups)

l_1 <- glm(rmssd ~ heart_rate, data = groups_list[["train"]])
l_2 <- glm(rmssd ~ poly(heart_rate, 2), data = groups_list[["train"]])
l_3 <- glm(rmssd ~ poly(heart_rate, 3), data = groups_list[["train"]])
(cv.err.10 <- cv.glm(groups_list[["train"]], l_1, K = 10)$delta)
(cv.err.10 <- cv.glm(groups_list[["train"]], l_2, K = 10)$delta)
(cv.err.10 <- cv.glm(groups_list[["train"]], l_3, K = 10)$delta)
summary(l_1)
svr.model <- svm(rmssd ~ heart_rate , data = groups_list[["train"]], cross = 10)
summary(svr.model)
pred <- predict(svr.model, data = groups_list[["train"]])
# working out own MSE (mean squared error)
# y.lm.reg <- predict(l_1)
# sum((y.lm.reg - groups_list[["train"]]$heart_rate)^2)
# mean((y.lm.reg - groups_list[["train"]]$heart_rate)^2)
# sqrt(mean((y.lm.reg - groups_list[["train"]]$heart_rate)^2))
ggplot(data = groups_list[["train"]]) +
        geom_point(aes(x = heart_rate, y = rmssd)) +
        geom_line(aes(x = heart_rate, y = l_1$fitted.values),color = "red") +
        geom_smooth(aes(x = heart_rate, y = l_2$fitted.values),color = "blue") +
        geom_smooth(aes(x = heart_rate, y = l_3$fitted.values),color = "green") +
        geom_smooth(aes(x = heart_rate, y = svr.model$fitted), color = "orange")
     # geom_smooth(method = "lm", formula = y ~ poly(x, 2), data = hrv_all)

# dist plot
ggplot(data = groups_list[["train"]]) +
        aes(x = heart_rate) +
        geom_density()


fit.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
tuneGrid <- expand.grid(
        C = c(0.25, .5, 1),
        sigma = 0.1
)
set.seed(1)
fit_lm <- train(rmssd ~ heart_rate, data = groups_list[["train"]], method = "lm", trControl = fit.control)
fit_glm <- train(rmssd ~ heart_rate, data = groups_list[["train"]], method = "gam", trControl = fit.control)
# fit_lasso <- train(rmssd ~ ., data = groups_list[["train"]], method = "glmnet", trControl = fit.control)
fit_svm <- train(rmssd ~ heart_rate, data = groups_list[["train"]], method = "svmRadial", trControl = fit.control, preProcess = c("center", "scale"), tuneGrid = tuneGrid)
resamps <- resamples(list(m1 = fit_lm,
                          m2 = fit_glm,
                          m3 = fit_svm))
plot(fit_svm)
summary(resamps)
fit_svm
xyplot(resamps, what = "BlandAltman", metric = "RMSE", models = c("m1", "m2"))

gam1 <- gam(rmssd ~ s(heart_rate), data = groups_list[["train"]])
plot(gam1)
#####
hrv_mean <- hrv_all %>%
        mutate(Date = date(time)) %>%
        group_by(Date) %>%
        summarise(rmssd = mean(rmssd))

full_data <- left_join(ready_data, hrv_mean, by = "Date") %>%
        filter(Sleep_hours > 4)


# want to take day minutes in one col and match with hrv in another col
