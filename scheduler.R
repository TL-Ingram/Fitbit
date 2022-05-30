library(taskscheduleR)

taskscheduler_create(taskname = "test_run", rscript = "C:/Users/User/Documents/R/Fitbit data/token_generation.R", 
                     schedule = "HOURLY", starttime = "17:36", modifier = 1)
taskscheduler_stop("test_run")

taskscheduler_delete("test_run")
tasks <- taskscheduler_ls()
str(tasks)


taskscheduler_create(taskname = "test_run_2", rscript = "C:/Users/User/Documents/R/Fitbit data/open_log.R", 
                     schedule = "HOURLY", starttime = "17:37", modifier = 1)
taskscheduler_stop("test_run_2")
taskscheduler_delete("test_run_2")
