#!/usr/bin/env Rscript

###############
## LIBRARIES ##
###############

library(lubridate)
library(readr)
library(dplyr)
library(stringr)
library(zoo)
library(tidyr)

####################################
## HOW MANY WORKOUTS ON A ROLLING ##
## 7 DAY WINDOW DO I WANT TO DO?  ##
####################################


NUM_CARDIO <- 3 # 3 Days Cardio
NUM_MUSCULAR <- 3 # 3 Days Lifting
NUM_NON <- 3 # 3 Days Walking the Dog
NUM_RESTORE <- 1 # 1 Day Break
MAX_DAYS_OVER <- 2 # Can sustain 2 above average days in a row
MAX_DAYS_UNDER <- 3 # Can allow 3 below average days in a row

###############################
## LOOK BACK 6 DAYS AND PLAN ##
## TODAY AND THE NEXT 6      ##
###############################

range_start <- Sys.Date() - days(6)
range_end <- Sys.Date() + days(6)

# GET THE WORKOUTS DATA
source("02_hk_data_functions.R")

workout_dates <- get_workout_dates_data(range_start)

category_sum <- get_category_sum_data(workout_dates)

current_rating <- get_current_rating(workout_dates)

# EACH DAY SHOULD EITHER BE CARDIO OR LIFTING OR RECOVERY
# 0. Even if I am overdue for something else... If I have had heavy strain, 
#    prioritize recovery
# 1. Cardio if I am both behind and it is the most days since last
# 2. Lifting if I am both behind and it is the most days since last
# 3. Randomly pick if they are both  behind and equal days since last
# 4. Recovery if cardio and lifting are both not behind
# 5. If you've already worked out today, you can do a recovery

# If you've worked out today, you can optionally do a recovery
if (min(category_sum$days_since_last) == 0) {
    
    today_type <- "restorative"

# If I've had too many hard days in a row, prioritize a recovery    
} else if (current_rating > MAX_DAYS_OVER) {
    
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

# Now that I know the type, let's name the exercise
# 1. Restorative can be sauna, yoga, walking
# 2. Muscular is lifting
# 3. Cardio is cycling or running (or soccer, but that's handled separately)
#    If I have been under average for a while, choose running

today_name <- case_when(
    today_type == "restorative" ~ sample(c("Sauna", "Yoga", "Walking"), 1),
    today_type == "muscular" ~ "Lifting",
    today_type == "cardiovascular" & current_rating < MAX_DAYS_UNDER ~ "Running",
    today_type == "cardiovascular" & current_rating >= MAX_DAYS_UNDER ~ "Cycling"
)


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

if (dim(class_sched)[1] == 0) {
    today_name <- paste0(today_name, " + Walking")
}
