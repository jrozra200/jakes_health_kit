library(lubridate)
library(readr)
library(dplyr)
library(stringr)

## Work it out for this week

week_start <- floor_date(Sys.Date(), unit = "week", week_start = 1)
week_end <- week_start + days(6)
days_of_week <- tibble(day = 1:7,
                       date = as_date(week_start:week_end),
                       lift = NA,
                       soccer = NA,
                       run = NA,
                       walk = NA,
                       sauna = NA,
                       recovery = NA,
                       projected_strain = NA) %>% 
    mutate(dotw = weekdays(date))

## Encode rules

# Soccer is on Mondays and Thursdays (and scheduled)
fuss_sched <- read_csv("../data/futbol_schedule.csv") %>% 
    filter(datetime >= week_start &
               datetime <= week_end)

# I _should_ walk the dog on *weekdays* that I don't have class 
class_days <- read_csv("../data/class_schedule.csv") %>% 
    pull(class_dates)

walk_poppy <- days_of_week %>% 
    filter(!date %in% class_days & 
               !dotw %in% c("Sunday", "Saturday")) %>% 
    pull(date)

# I need to lift at least 3 times a week
TOT_LIFTS <- 3
num_lifts <- 0
yest_lift <- 0

# I need to run at least 2 times a week (soccer counts as running)
TOT_RUNS <- 2
num_runs <- 0
yest_run <- 0
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

# I need to have at least one workout session per day
    # - Active Recovery
    # - Run
    # - Soccer
    # - Lift
    # - Walk longer than 1 hour
TOT_SESS_DAY <- 1

for (day in days_of_week$day) {
    wd <- days_of_week$date[day]
    
    # Is today a soccer day?
    if (wd %in% as_date(fuss_sched$datetime)) {
        days_of_week$soccer[day] <- TRUE
        num_runs <- num_runs + 1
        yest_fuss <- yest_fuss + 1
        
        days_of_week$run[day] <- FALSE
        yest_run <- 0
        
        days_of_week$lift[day] <- FALSE
        yest_lift <- 0
        
        days_of_week$sauna[day] <- FALSE
        yest_sauna <- 0
        
        days_of_week$recovery[day] <- FALSE
        yest_recov <- 0
        
    } else {
        days_of_week$soccer[day] <- FALSE
        yest_fuss <- 0
    }
    
    # Is today a walk poppy day?
    if (wd %in% walk_poppy) {
        days_of_week$walk[day] <- TRUE
    } else {
        days_of_week$walk[day] <- FALSE
    }
    
    # Is today a lifting or running day? 
    if (days_of_week$soccer[day] == FALSE) { 
        
        # Lift first, unless you've lifted for the last 2 days
        if(num_lifts < TOT_LIFTS & yest_lift < 2) {
            days_of_week$lift[day] <- TRUE
            num_lifts <- num_lifts + 1
            yest_lift <- yest_lift + 1
            
            days_of_week$run[day] <- FALSE
            yest_run <- 0
            
            days_of_week$recovery[day] <- FALSE
            yest_recov <- 0
            
            days_of_week$soccer[day] <- FALSE
            yest_fuss <- 0
            
            days_of_week$sauna[day] <- FALSE
            yest_sauna <- 0
        } 
        
        # Run next, unless you ran yesterday (or played soccer)
        # If you have an extra day, it is a run day
        else if (yest_fuss == 0 & yest_run == 0 & 
                    (num_runs < TOT_RUNS | 
                        (num_lifts >= TOT_LIFTS & num_recov >= TOT_RECOV))) {
            days_of_week$run[day] <- TRUE
            num_runs <- num_runs + 1
            yest_run <- yest_run + 1
            
            days_of_week$lift[day] <- FALSE
            yest_lift <- 0
            
            days_of_week$recovery[day] <- FALSE
            yest_recov <- 0
            
            days_of_week$soccer[day] <- FALSE
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
            
            days_of_week$lift[day] <- FALSE
            yest_lift <- 0
            
            days_of_week$run[day] <- FALSE
            yest_run <- 0
            
            days_of_week$soccer[day] <- FALSE
            yest_fuss <- 0
        }
    } 
}

cycles <- read_csv('../data/cycles.csv')
sleeps <- read_csv('../data/sleeps.csv')
recovery <- read_csv('../data/recovery.csv')
workouts <- read_csv('../data/workouts.csv')

# Convert raw strain to strain: exp((0.398850 * log(raw)) + 4.509219)

wo <- workouts %>% 
    filter(!is.na(name)) %>% 
    mutate(name = case_when(
        (name == "Powerlifting" | 
             name == "Functional Fitness") ~ "Weightlifting",
        1 == 1 ~ name
    )) %>% 
    group_by(name) %>% 
    slice_head(n = 10) %>% 
    summarise(count = length(name),
              avg_strain = mean(intensity_score),
              avg_intensity = mean(raw_intensity_score))


day_start <- unlist(
        str_split(cycles$during, ",")
    )[grepl("\\[", unlist(str_split(cycles$during, ",")))]
day_start <- str_remove_all(day_start, "\\[|\\'")
day_start <- as_date(day_start, format = "%Y-%m-%dT%H:%M:%OSZ")

cycles <- cycles %>% 
    mutate(day_start = day_start,
           dotw = weekdays(day_start))

cycles_sum <- cycles %>% 
    group_by(dotw) %>% 
    slice_head(n = 10) %>% 
    summarise(avg_strain = mean(day_strain),
              avg_score = mean(scaled_strain),
              avg_kj = mean(day_kilojoules))