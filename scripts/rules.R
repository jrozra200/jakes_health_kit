library(lubridate)


## Work it out for this week

week_start <- floor_date(Sys.Date(), unit = "week")
week_end <- week_start + days(7)

## Encode rules

# Soccer is on Mondays and Thursdays (and scheduled)
fuss_sched <- read.csv("../data/futbol_schedule.csv")

- I _should_ walk the dog on weekdays that I don't have class 
- I need to lift at least 3 times a week
- I need to run at least 2 times a week (soccer counts as running)
- I need to have 1 [active] recovery day per week
    - Long, light jog (i.e. 3 miles @ 10 min/mile)
    - 30 minute low impact ride
- I need to have 1 sauna session per week
- I need to have at least one workout session per day
    - Active Recovery
    - Run
    - Soccer
    - Lift
    - Walk longer than 1 hour