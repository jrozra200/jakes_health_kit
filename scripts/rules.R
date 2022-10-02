library(lubridate)
library(readr)
library(dplyr)
library(stringr)

## Work it out for this week

week_start <- floor_date(Sys.Date(), unit = "week", week_start = 1)
week_end <- week_start + days(6)
days_of_week <- tibble(day = 1:7,
                       date = as_date(week_start:week_end),
                       Weightlifting = NA,
                       Soccer = NA,
                       Running = NA,
                       Walking = NA,
                       Cycling = NA,
                       sauna = NA,
                       recovery = NA) %>% 
    mutate(dotw = weekdays(date))

## Encode rules

# Soccer is on Mondays and Thursdays (and scheduled)
fuss_sched <- read_csv("../data/futbol_schedule.csv") %>% 
    filter(datetime >= week_start &
               datetime <= week_end)

# I _should_ Walking the dog on *weekdays* that I don't have class 
class_days <- read_csv("../data/class_schedule.csv") %>% 
    pull(class_dates)

Walking_poppy <- days_of_week %>% 
    filter(!date %in% class_days & 
               !dotw %in% c("Sunday", "Saturday")) %>% 
    pull(date)

# Get workout info from this week
source("02_hk_data_functions.R")
workouts <- get_workouts_data() %>% 
    filter(date >= week_start & 
               date <= week_end) %>% 
    group_by(date) %>% 
    summarise(workouts = paste0(name, collapse = " + "))

days_of_week <- days_of_week %>% 
    left_join(workouts, by = "date")

# I need to Weightlifting at least 3 times a week
TOT_LIFTS <- 3
num_Weightliftings <- 0
yest_Weightlifting <- 0

# I need to Running at least 2 times a week (Soccer counts as Runningning)
TOT_RUNS <- 2
num_Runnings <- 0
yest_Running <- 0
yest_fuss <- 0

# I need to have 1 [active] recovery day per week
#    - Long, light jog (i.e. 3 miles @ 10 min/mile)
#    - 30 minute low impact ride
TOT_RECOV <- 1
num_recov <- 0
yest_recov <- 0

# I need to have 1 sauna session per week
TOT_SAUNA <- 1
num_sauna <- 0
yest_sauna <- 0

num_Walkings <- 0

# I need to have at least one workout session per day
    # - Active Recovery
    # - Run
    # - Soccer
    # - Lift
    # - Walk longer than 1 hour
TOT_SESS_DAY <- 1

# What have I done already?
tracked <- c("WeightWeightliftinging", 
             "Soccer", 
             "Running", 
             "Walking", 
             "Cycling", 
             "Sauna")

for (day in 1:7) {
    if (!is.na(days_of_week$workouts[day])) {
        this_days_wo <- unlist(
            str_split(days_of_week$workouts[days_of_week$day == day],
                      " \\+ "))
        
        if ("Soccer" %in% this_days_wo) {
            num_Runnings <- num_Runnings + 1
            days_of_week[days_of_week$day == day, "Soccer"] <- TRUE
        }
        
        if ("Running" %in% this_days_wo) {
            num_Runnings <- num_Runnings + 1
            days_of_week[days_of_week$day == day, "Running"] <- TRUE
        }
        
        if ("Weightlifting" %in% this_days_wo) {
            num_Weightliftings <- num_Weightliftings + 1
            days_of_week[days_of_week$day == day, "Weightlifting"] <- TRUE
        }
        
        if ("Walking" %in% this_days_wo) {
            num_Walkings <- num_Walkings + 1
            days_of_week[days_of_week$day == day, "Walking"] <- TRUE
        }
        
        for (nf in tracked[!tracked %in% this_days_wo]) {
            days_of_week[days_of_week$day == day, nf] <- FALSE
        }
        
        next
    }
    
    wd <- days_of_week$date[day]
    
    # Is today a Soccer day?
    if (wd %in% as_date(fuss_sched$datetime)) {
        days_of_week$Soccer[day] <- TRUE
        num_Runnings <- num_Runnings + 1
        yest_fuss <- yest_fuss + 1
        
        days_of_week$Running[day] <- FALSE
        yest_Running <- 0
        
        days_of_week$Weightlifting[day] <- FALSE
        yest_Weightlifting <- 0
        
        days_of_week$sauna[day] <- FALSE
        yest_sauna <- 0
        
        days_of_week$recovery[day] <- FALSE
        yest_recov <- 0
        
    } else {
        days_of_week$Soccer[day] <- FALSE
        yest_fuss <- 0
    }
    
    # Is today a Walking poppy day?
    if (wd %in% Walking_poppy) {
        days_of_week$Walking[day] <- TRUE
    } else {
        days_of_week$Walking[day] <- FALSE
    }
    
    # Is today a Weightliftinging or Runningning day? 
    if (days_of_week$Soccer[day] == FALSE) { 
        
        # Lift first, unless you've Weightliftinged for the last 2 days
        if(num_Weightliftings < TOT_LIFTS & yest_Weightlifting < 2) {
            days_of_week$Weightlifting[day] <- TRUE
            num_Weightliftings <- num_Weightliftings + 1
            yest_Weightlifting <- yest_Weightlifting + 1
            
            days_of_week$Running[day] <- FALSE
            yest_Running <- 0
            
            days_of_week$recovery[day] <- FALSE
            yest_recov <- 0
            
            days_of_week$Soccer[day] <- FALSE
            yest_fuss <- 0
            
            days_of_week$sauna[day] <- FALSE
            yest_sauna <- 0
        } 
        
        # Run next, unless you ran yesterday (or played Soccer)
        # If you have an extra day, it is a Running day
        else if (yest_fuss == 0 & yest_Running == 0 & 
                    (num_Runnings < TOT_RUNS | 
                        (num_Weightliftings >= TOT_LIFTS & num_recov >= TOT_RECOV))) {
            days_of_week$Running[day] <- TRUE
            num_Runnings <- num_Runnings + 1
            yest_Running <- yest_Running + 1
            
            days_of_week$Weightlifting[day] <- FALSE
            yest_Weightlifting <- 0
            
            days_of_week$recovery[day] <- FALSE
            yest_recov <- 0
            
            days_of_week$Soccer[day] <- FALSE
            yest_fuss <- 0
            
            days_of_week$sauna[day] <- FALSE
            yest_sauna <- 0
        } 
        
        # Recover last (with a trip to the sauna)
        else {
            days_of_week$recovery[day] <- TRUE
            num_recov <- num_recov + 1
            yest_recov <- yest_recov + 1
            
            days_of_week$sauna[day] <- TRUE
            num_sauna <- num_sauna + 1
            yest_sauna <- yest_sauna + 1
            
            days_of_week$Weightlifting[day] <- FALSE
            yest_Weightlifting <- 0
            
            days_of_week$Running[day] <- FALSE
            yest_Running <- 0
            
            days_of_week$Soccer[day] <- FALSE
            yest_fuss <- 0
        }
    } 
}

write_csv(days_of_week, "../data/workout_schedule.csv")
