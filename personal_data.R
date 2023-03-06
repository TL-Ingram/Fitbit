# Join files together
personal_data <- readxl::read_xlsx("Personal_data copy.xlsx") 

# go through each column and clean

# change gym column to yes, no and one column per time of day
gym <- personal_data %>%
     mutate(gym =)

# change fruit column into apple, ban, other, no fruit


