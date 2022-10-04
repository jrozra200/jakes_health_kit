#!/usr/bin/env Rscript

###############
## LIBRARIES ##
###############

library(lubridate)
library(readr)
library(dplyr)
library(stringr)
library(zoo)

####################################
## HOW MANY WORKOUTS ON A ROLLING ##
## 7 DAY WINDOW DO I WANT TO DO?  ##
####################################

NUM_CARDIO <- 3 # 3 Days Cardio
NUM_MUSCULAR <- 3 # 3 Days Lifting
NUM_NON <- 3 # 3 Days Walking the Dog
NUM_RESTORE <- 1 # 1 Day Break

###############################
## LOOK BACK 6 DAYS AND PLAN ##
## TODAY AND THE NEXT 6      ##
###############################

range_start <- Sys.Date() - days(6)
range_end <- Sys.Date() + days(6)

# GET THE WORKOUTS DATA
source("02_hk_data_functions.R")
sports <- get_sports()

workouts <- get_workouts_data() %>% 
    filter(date >= range_start) %>% 
    left_join(sports, by = "name")

workout_dates <- workouts %>% 
    group_by(date) %>% 
    summarise(workouts_completed = paste0(name, collapse = " + ")) %>% 
    left_join(get_cycles_data(), by = c("date" = "day_start")) %>% 
    select(date, workouts_completed, day_strain) %>% 
    mutate(day_strain = day_strain * 1000,
           avg_strain = get_thirty_day_strain(get_cycles_data()),
           over_avg = ifelse(day_strain >= avg_strain, TRUE, FALSE),
           cons_ov_avg = ifelse(day_strain >= avg_strain, 
                                cumsum(over_avg),
                                0))

# If there are any missing dates, then they count as recovery
missing_dates <- range_start:(Sys.Date() - days(1))
missing_dates <- missing_dates[!missing_dates %in% workout_dates$date]

tmp <- tibble()

if (length(missing_dates) > 0) {
    tmp <- tibble(date = as_date(missing_dates),
                  workouts_completed = "Other - Recovery")
    
    workout_dates <- bind_rows(workout_dates, tmp)
    
    tmp <- tmp %>% 
        mutate(category = "restorative") %>% 
        rename(name = workouts_completed)
}

category_sum <- workouts %>% 
    select(date, name, category) %>% 
    bind_rows(tmp) %>%
    left_join(workout_dates, by = "date") %>% 
    mutate(category = ifelse(name == "Walking" & workouts_completed == "Walking",
                             "restorative", category)) %>% 
    group_by(category) %>% 
    summarise(num_efforts = length(name),
              last_effort = max(date)) %>% 
    mutate(days_since_last = Sys.Date() - last_effort,
           behind = case_when(
               category == "cardiovascular" & num_efforts < NUM_CARDIO ~ TRUE,
               category == "cardiovascular" & num_efforts >= NUM_CARDIO ~ FALSE,
               category == "muscular" & num_efforts < NUM_MUSCULAR ~ TRUE,
               category == "muscular" & num_efforts >= NUM_MUSCULAR ~ FALSE,
               category == "non-cardiovascular" & num_efforts < NUM_NON ~ TRUE,
               category == "non-cardiovascular" & num_efforts >= NUM_NON ~ FALSE,
               category == "restorative" & num_efforts < NUM_RESTORE ~ TRUE,
               category == "restorative" & num_efforts >= NUM_RESTORE ~ FALSE
           )) %>% 
    arrange(-behind, days_since_last)

missing_category <- c("cardiovascular", 
                      "muscular", 
                      "non-cardiovascular", 
                      "restorative")
missing_category <- missing_category[!missing_category %in% category_sum$category]    

tmp <- tibble()

if (length(missing_category) > 0) {
    tmp <- tibble(category = missing_category,
                  num_efforts = 0,
                  last_effort = as_date(NA),
                  days_since_last = Sys.Date() - (Sys.Date() - days(7)),
                  behind = TRUE)
}

category_sum <- bind_rows(category_sum, tmp) %>% 
    arrange(desc(behind), desc(days_since_last))

# EACH DAY SHOULD EITHER BE CARDIO OR LIFTING OR RECOVERY
# 0. Even if I am overdue for something else... If I have had heavy strain, 
#    prioritize recovery
# 1. Cardio if I am both behind and it is the most days since last
# 2. Lifting if I am both behind and it is the most days since last
# 3. Randomly pick if they are both  behind and equal days since last
# 4. Recovery if cardio and lifting are both not behind
# 5. If you've already worked out today, you can do a recovery

# If you've worked out today, you can optionally do a recovery
if (!is.na(min(category_sum$days_since_last)) & min(category_sum$days_since_last) == 0) {
    
    today_type <- "restorative"

# If muscular and cardio are behind by the same amount of days, choose randomly    
} else if ((category_sum$behind[category_sum$category == "cardiovascular"] & 
     category_sum$behind[category_sum$category == "muscular"] & 
     (category_sum$num_efforts[category_sum$category == "cardiovascular"] == 
         category_sum$num_efforts[category_sum$category == "muscular"]))) {
    
    today_type <- sample(c("cardiovascular", "muscular"), 1)

# If muscular and cardio are behind but cardio by more days, choose cardio    
} else if (category_sum$behind[category_sum$category == "cardiovascular"] & 
           category_sum$behind[category_sum$category == "muscular"] & 
           (category_sum$num_efforts[category_sum$category == "cardiovascular"] > 
            category_sum$num_efforts[category_sum$category == "muscular"])) {
    
    today_type <- "cardiovascular"

# If muscular and cardio are behind but muscular by more days, choose muscular
} else if (category_sum$behind[category_sum$category == "cardiovascular"] & 
           category_sum$behind[category_sum$category == "muscular"] & 
           (category_sum$num_efforts[category_sum$category == "cardiovascular"] < 
            category_sum$num_efforts[category_sum$category == "muscular"])) {
    
    today_type <- "muscular"
    
# If cardio is the only one behind, choose cardio
} else if (category_sum$behind[category_sum$category == "cardiovascular"]) {
    
    today_type <- "cardiovascular"
    
# If muscular is the only one behind, choose muscular
} else if (category_sum$behind[category_sum$category == "muscular"]) {

    today_type <- "muscular"

# If nothing is behind, recover!    
} else {
    today_type <- "restorative"
}

############################
## Is today a soccer day? ##
## Then override the type ##
############################
soccer_sched <- get_soccer_data() %>% 
    filter(as_date(datetime) == Sys.Date())

if (dim(soccer_sched)[1] > 0) {
    today_type <- "cardiovascular"
    today_name <- "Soccer"
}

#################
## Add a Walk? ##
#################
class_sched <- get_class_data() %>% 
    filter(as_date(class_dates) == Sys.Date())

if (dim(class_sched)[1] > 0) {
    today_name <- paste0(today_name, " + Walking")
}