
get_workouts_data <- function() {
    workouts <- read_csv('../data/workouts.csv')
    
    workouts <- workouts %>% 
        bind_cols(
            as_tibble(do.call(rbind, str_split(workouts$during, ","))) %>% 
                mutate(wo_start = as_datetime(V1, format = "['%Y-%m-%dT%H:%M:%OSZ'") - hours(4),
                       wo_end = as_datetime(V2, format = "'%Y-%m-%dT%H:%M:%OSZ')") - hours(4)) %>% 
                select(wo_start,
                       wo_end)
        ) %>% 
        mutate(date = as_date(wo_start),
               dotw = weekdays(date, abbreviate = TRUE),
               name = case_when(
                   (name == "Powerlifting" | 
                        name == "Functional Fitness") ~ "Weightlifting",
                   is.na(name) ~ "NA",
                   1 == 1 ~ name
               )
        ) %>% 
        group_by(name, date, dotw) %>% 
        summarise(intensity_score = sum(intensity_score, na.rm = TRUE),
                  raw_intensity_score = sum(raw_intensity_score, na.rm = TRUE) * 1000,
                  kilojoules = sum(kilojoules, na.rm = TRUE),
                  calories = sum(kilojoules, na.rm = TRUE) * KJ_TO_CAL,
                  max_heart_rate = max(max_heart_rate, na.rm = TRUE), 
                  average_heart_rate = mean(average_heart_rate, na.rm = TRUE),
                  distance = sum(distance, na.rm = TRUE) / M_TO_MILE, 
                  altitude_gain = sum(altitude_gain, na.rm = TRUE) / M_TO_FT,
                  altitude_change = sum(altitude_change, na.rm = TRUE) / M_TO_FT,
                  zone_durations = paste0(zone_durations, collapse = "|"),
                  wo_start = min(wo_start, na.rm = TRUE),
                  wo_end = max(wo_end, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(wo_end))
    
    
    return(workouts)
}

get_sports <- function() {
    sports <- read_csv("../data/sports.csv") %>% 
        select(name,
               category)
    
    return(sports)
}

get_workout_dates_data <- function(range_start) {
    workout_dates <- get_workouts_data() %>% # Start with the workout data 
        
        # Get the sports names and join that to this dataset
        left_join(get_sports(), by = "name") %>% 
        
        # Create a new category name so that cardio and non-cardio are distinct
        mutate(new_cat = str_sub(category, 1, 5)) %>% 
        
        # Group by day
        group_by(date) %>% 
        
        # Calculate how many workouts completed, what category they are, 
        # and the strain
        summarise(workouts_completed = paste0(name, collapse = " + "),
                  category = paste0(new_cat, collapse = " + "),
                  act_strain = sum(raw_intensity_score)) %>% 
        
        # Join the strain data (so we can tell when I've missed a day working out)
        full_join(get_cycles_data(), by = c("date" = "date")) %>% 
        
        # Filter by the start of the range
        filter(date >= range_start) %>% 
        
        # Only need certain columns
        select(date, workouts_completed, category, act_strain, day_strain) %>% 
        
        # If there is no workout, then that was a "recovery"
        # Otherwise, it was what it was
        mutate(workouts_completed = ifelse(is.na(workouts_completed), 
                                           "Other - Recovery",
                                           workouts_completed),
               category = ifelse(is.na(category), 
                                 str_sub("restorative", 1, 5),
                                 category),
               day_strain = ifelse(is.na(day_strain), 0, day_strain * 1000),
               avg_strain = last_30_day_ma(get_cycles_data(),
                                           "day_strain",
                                           "date",
                                           range_start,
                                           Sys.Date()) * 1000,
               over_avg = ifelse(day_strain >= avg_strain, 1, -1),
               category_restore = case_when(
                   workouts_completed == "Walking" & act_strain < 1 ~ TRUE,
                   category == "resto" ~ TRUE,
                   1 == 1 ~ FALSE),
               category_muscular = ifelse(grepl("muscu", category), TRUE, FALSE),
               category_cardio = ifelse(grepl("cardi", category), 
                                        TRUE, FALSE),
               category_non = ifelse(grepl("non-c", category) & 
                                         !(workouts_completed == "Walking" & 
                                               act_strain < 1), TRUE, FALSE))
    
    # Calculate the plus minus
    workout_dates <- workout_dates %>% 
        arrange(date) %>%
        mutate(over_avg_lag = lag(over_avg, 1),
               over_avg_lag = ifelse(is.na(over_avg_lag),
                                     over_avg,
                                     over_avg_lag))
    
    current_score <- 0
    for (dt in 1:dim(workout_dates)[1]) {
        if(workout_dates$over_avg[dt] == workout_dates$over_avg_lag[dt]) {
            current_score <- current_score + 1
        } else {
            current_score <- 1
        }
        
        workout_dates$plus_minus[dt] <- current_score
    }
    
    workout_dates$plus_minus <- workout_dates$plus_minus * workout_dates$over_avg
    
    return(workout_dates)
}

get_todays_wo <- function() {
    
    # GET THE WORKOUTS DATA
    workout_dates <- get_workout_dates_data(range_start) %>% 
        filter(date < Sys.Date()) # Remove today's workout
    
    category_sum <- get_category_sum_data(workout_dates,
                                          NUM_CARDIO,
                                          NUM_MUSCULAR,
                                          NUM_NON,
                                          NUM_RESTORE)
    
    current_rating <- get_current_rating(workout_dates)
    
    yesterday <- workout_dates %>% 
        filter(date == Sys.Date() - days(1)) %>% 
        pull(category)
    
    # EACH DAY SHOULD EITHER BE CARDIO OR LIFTING OR RECOVERY
    # 0. Even if I am overdue for something else... If I have had heavy strain, 
    #    prioritize recovery
    # 1. Cardio if I am both behind and it is the most days since last
    # 2. Lifting if I am both behind and it is the most days since last
    # 3. Randomly pick if they are both  behind and equal days since last
    # 4. Recovery if cardio and lifting are both not behind
    # 5. If you've already worked out today, you can do a recovery
    # 6. Don't do the same workout two days in a row (no back to back lifts, etc.)
    
    # If I've had too many hard days in a row, prioritize a recovery    
    if (current_rating > MAX_DAYS_OVER) {
        
        today_type <- "restorative"
        
        # If I'm getting soft, then work hard next
    } else if (current_rating < MAX_DAYS_UNDER) {
        
        today_type <- "cardiovascular"
        
        # If restorative is behind, then restore
    } else if (category_sum$behind[category_sum$category == "restorative"]) {
        
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
               ((NUM_CARDIO - 
                 category_sum$num_efforts[category_sum$category == "cardiovascular"]) > 
                (NUM_MUSCULAR - 
                 category_sum$num_efforts[category_sum$category == "muscular"]))) {
        
        today_type <- "cardiovascular"
        
        # If muscular and cardio are behind but muscular by more days, choose muscular
    } else if (category_sum$behind[category_sum$category == "cardiovascular"] & 
               category_sum$behind[category_sum$category == "muscular"] & 
               ((NUM_CARDIO - 
                 category_sum$num_efforts[category_sum$category == "cardiovascular"]) < 
                (NUM_MUSCULAR - 
                 category_sum$num_efforts[category_sum$category == "muscular"]))) {
        
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
        today_type == "restorative" ~ sample(c("Yoga", "Walking"), 1),
        today_type == "muscular" & !grepl("muscu", yesterday) ~ "Weightlifting",
        today_type == "muscular" & grepl("muscu", yesterday) & 
            current_rating < MAX_DAYS_OVER ~ "Running",
        today_type == "muscular" & grepl("muscu", yesterday) & 
            current_rating >= MAX_DAYS_OVER ~ "Spin",
        today_type == "cardiovascular" & current_rating < MAX_DAYS_OVER & 
            !grepl("cardi", yesterday) ~ "Running",
        today_type == "cardiovascular" & current_rating < MAX_DAYS_OVER & 
            grepl("cardi", yesterday) ~ "Weightlifting",
        today_type == "cardiovascular" & current_rating >= MAX_DAYS_OVER & 
            grepl("cardi", yesterday) ~ "Weightlifting"
    )
    
    
    ############################
    ## Is today a soccer day? ##
    ## Then override the type ##
    ############################
    soccer_sched <- get_soccer_data(events) 
    
    if (dim(soccer_sched)[1] > 0) {
        today_type <- "cardiovascular"
        today_name <- "Soccer"
    }
    
    #################
    ## Add a Walk? ##
    #################
    class_sched <- get_class_data(events)
    
    if (dim(class_sched)[1] == 0) {
        today_name <- paste0(today_name, " + Walking")
    }
    
    return(today_name)
}

get_category_sum_data <- function(workout_dates,
                                  NUM_CARDIO,
                                  NUM_MUSCULAR,
                                  NUM_NON,
                                  NUM_RESTORE) {
    category_sum <- workout_dates %>% 
        summarise(restorative = sum(category_restore),
                  cardiovascular = sum(category_cardio),
                  `non-cardiovascular` = sum(category_non),
                  muscular = sum(category_muscular)) %>% 
        pivot_longer(cols = c("cardiovascular", 
                              "muscular", 
                              "non-cardiovascular", 
                              "restorative"),
                     names_to = "category",
                     values_to = "num_efforts") %>% 
        mutate(behind = case_when(category == "cardiovascular" & num_efforts < NUM_CARDIO ~ TRUE,
                                  category == "cardiovascular" & num_efforts >= NUM_CARDIO ~ FALSE,
                                  category == "muscular" & num_efforts < NUM_MUSCULAR ~ TRUE,
                                  category == "muscular" & num_efforts >= NUM_MUSCULAR ~ FALSE,
                                  category == "non-cardiovascular" & num_efforts < NUM_NON ~ TRUE,
                                  category == "non-cardiovascular" & num_efforts >= NUM_NON ~ FALSE,
                                  category == "restorative" & num_efforts < NUM_RESTORE ~ TRUE,
                                  category == "restorative" & num_efforts >= NUM_RESTORE ~ FALSE))
    
    days_since_last <- tibble(
        category = c("cardiovascular", 
                     "muscular", 
                     "non-cardiovascular", 
                     "restorative"),
        last_date = c(max(workout_dates$date[workout_dates$category_cardio == TRUE]),
                      max(workout_dates$date[workout_dates$category_muscular == TRUE]),
                      max(workout_dates$date[workout_dates$category_non == TRUE]),
                      max(workout_dates$date[workout_dates$category_restore == TRUE]))) %>%
        mutate(last_date = ifelse(is.infinite(last_date), 
                                  Sys.Date() - days(7),
                                  last_date),
               last_date = as_date(last_date),
               days_since_last = Sys.Date() - last_date)
    
    category_sum <- category_sum %>% 
        left_join(days_since_last, by = "category")
    
    return(category_sum)
}

get_current_rating <- function(workout_dates) {
    current_rating <- workout_dates %>% 
        arrange(date) %>% 
        tail(1) %>% 
        pull(plus_minus)
    
    return(current_rating)
}

get_soccer_data <- function(events) {
    soccer <- events %>% 
        filter(grepl("soccer", tolower(event)))
    
    return(soccer)
}

get_class_data <- function(events) {
    class <- events %>% 
        filter(grepl("stat", tolower(event)))
    
    return(class)
}

get_wo_message <- function(todays_wo, wo_window_num, wo_windows) {
    if (grepl("Soccer", todays_wo)) {
        soccer <- read_csv("../data/futbol_schedule.csv") %>% 
            filter(as_date(datetime) == Sys.Date())
        wo_message <- paste0("You have soccer tonight with ", soccer$team[1], " at ",
                             format(soccer$datetime[1], "%I:%M %p"), " on field ",
                             soccer$field[1], ".")
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, "\n\nYou have ", wo_window_num, 
                                 " window(s) long enough to get your walk in today.",
                                 "\n\nTry to get your walk in during these windows:",
                                 wo_windows, "\n\n")
        }
    } else {
        wo_message <- paste0("You have ", wo_window_num, " window(s) long enough ", 
                             "to get your workout in today.\n\nTry to get your time ", 
                             "in during these windows:", wo_windows, "\n\n")
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, "\n\nDon't forget... you also have to ", 
                                 "get your walk with Poppy in today.")
        }
    }
    
    return(wo_message)
}
