library(lubridate)
library(readr)
library(dplyr)
library(stringr)
library(zoo)


## LOOK BACK 7 DAYS AND PLAN THE NEXT 7
range_start <- Sys.Date() - days(7)
range_end <- Sys.Date() + days(7)

num_cardio <- 3
num_muscular <- 3
num_non <- 3
# num_restore <- 1

# GET THE WORKOUTS DATA
source("02_hk_data_functions.R")
sports <- get_sports()

workouts <- get_workouts_data() %>% 
    filter(date >= range_start) %>% 
    left_join(sports, by = "name")

last_workout <- get_avg_workouts_data(get_workouts_data(), condensed = TRUE) %>% 
    left_join(sports, by = "name")

workout_sum <- workouts %>% 
    group_by(name) %>% 
    summarise(num_efforts = length(name),
              last_effort = max(date)) %>% 
    mutate(days_since_last = Sys.Date() - last_effort) %>% 
    left_join(sports, by = "name")

workout_dates <- workouts %>% 
    group_by(date) %>% 
    summarise(workouts_completed = paste0(name, collapse = " + "))

category_sum <- workouts %>% 
    group_by(category) %>% 
    summarise(num_efforts = length(name),
              last_effort = max(date)) %>% 
    mutate(days_since_last = Sys.Date() - last_effort,
           behind = case_when(
               category == "cardiovascular" & num_efforts < num_cardio ~ TRUE,
               category == "cardiovascular" & num_efforts >= num_cardio ~ FALSE,
               category == "muscular" & num_efforts < num_muscular ~ TRUE,
               category == "muscular" & num_efforts >= num_muscular ~ FALSE,
               category == "non-cardiovascular" & num_efforts < num_non ~ TRUE,
               category == "non-cardiovascular" & num_efforts >= num_non ~ FALSE
           )) %>% 
    arrange(desc(behind), desc(days_since_last))
    
# EACH DAY SHOULD EITHER BE CARDIO OR LIFTING OR RECOVERY
# 1. Cardio if I am both behind and it is the most days since last
# 2. Lifting if I am both behind and it is the most days since last
# 3. Randomly pick if they are both  behind and equal days since last
# 4. Recovery if cardio and lifting are both not behind

