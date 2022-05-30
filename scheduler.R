library(taskscheduleR)
library(here)

taskscheduler_create(taskname = "test_run", rscript = here("token_generation.R"), 
                     schedule = "HOURLY", starttime = "17:36", modifier = 1)
taskscheduler_create(taskname = "test_run_2", rscript = here("open_log.R"), 
                     schedule = "HOURLY", starttime = "17:37", modifier = 1)


# taskscheduler_stop("test_run")
# taskscheduler_delete("test_run")

# taskscheduler_stop("test_run_2")
# taskscheduler_delete("test_run_2")

# tasks <- taskscheduler_ls()
# tasks