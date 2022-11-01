#########################
## UNIVERSAL VARIABLES ##
#########################

## Constants ##

MS_IN_HOUR <- 3600000 # Miliseconds in an hour
KJ_TO_CAL <- 0.239 # Kilajoules to calories
M_TO_MILE <- 1609.344 # Meters to miles
M_TO_FT <- 3.28084 # Meters to feet

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
