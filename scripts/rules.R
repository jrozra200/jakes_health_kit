library(lubridate)
library(readr)
library(dplyr)

## Work it out for this week

week_start <- floor_date(Sys.Date(), unit = "week")
week_end <- week_start + days(7)
days_of_week <- tibble(date = as_date(week_start:week_end)) %>% 
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
# I need to run at least 2 times a week (soccer counts as running)
# I need to have 1 [active] recovery day per week
#    - Long, light jog (i.e. 3 miles @ 10 min/mile)
#    - 30 minute low impact ride
# I need to have 1 sauna session per week
# I need to have at least one workout session per day
    # - Active Recovery
    # - Run
    # - Soccer
    # - Lift
    # - Walk longer than 1 hour