# UNIVERSAL VARIABLES
MS_IN_HOUR <- 3600000
KJ_TO_CAL <- 0.239

# Today's Events

get_events_data <- function() {
    events <- read_csv("../data/calendar_events.csv") 
    
    if (dim(events)[1] > 0) {
        events <- events %>% 
            mutate(length = difftime(end.dateTime, start.dateTime, units = "mins"),
                   start.dateTime = start.dateTime - hours(4),
                   end.dateTime = end.dateTime - hours(4)) %>% 
            rename(start = start.dateTime,
                   end = end.dateTime,
                   event = summary) %>% 
            select(event, 
                   start,
                   end,
                   length,
                   calendar)
        
        if (!weekdays(Sys.Date()) %in% c("Saturday", "Sunday")) {
            start.dateTime = strptime(paste0(Sys.Date(), ":06:15:00"), 
                                      format = "%Y-%m-%d:%H:%M:%S", 
                                      tz = "UTC")
            end.dateTime = strptime(paste0(Sys.Date(), ":07:30:00"), 
                                    format = "%Y-%m-%d:%H:%M:%S",
                                    tz = "UTC")
            day_start <- tibble(
                event = "With the Family",
                start = start.dateTime,
                end = end.dateTime,
                length = difftime(end.dateTime, start.dateTime, units = "mins"),
                calendar = "rozran00@gmail.com"
            )
            
            events <- bind_rows(day_start, events)
            
            #######################################################
            
            start.dateTime = strptime(paste0(Sys.Date(), ":07:30:00"), 
                                      format = "%Y-%m-%d:%H:%M:%S", 
                                      tz = "UTC")
            end.dateTime = strptime(paste0(Sys.Date(), ":08:30:00"), 
                                    format = "%Y-%m-%d:%H:%M:%S",
                                    tz = "UTC")
            day_start <- tibble(
                event = "Drop Off Kids",
                start = start.dateTime,
                end = end.dateTime,
                length = difftime(end.dateTime, start.dateTime, units = "mins"),
                calendar = "rozran00@gmail.com"
            )
            
            events <- bind_rows(day_start, events)
            
            #######################################################
            
            start.dateTime = strptime(paste0(Sys.Date(), ":16:00:00"), 
                                      format = "%Y-%m-%d:%H:%M:%S", 
                                      tz = "UTC")
            end.dateTime = strptime(paste0(Sys.Date(), ":19:00:00"), 
                                    format = "%Y-%m-%d:%H:%M:%S",
                                    tz = "UTC")
            day_end <- tibble(
                event = "With the Family",
                start = start.dateTime,
                end = end.dateTime,
                length = difftime(end.dateTime, start.dateTime, units = "mins"),
                calendar = "rozran00@gmail.com"
            )
            
            events <- bind_rows(events, day_end) %>% 
                arrange(start, end)
        } 
        
        events <- events %>% 
            filter(!is.na(start) | !is.na(end)) %>% 
            mutate(calendar = ifelse(event == "STAT 4380", 
                                     "jacob.rozran@villanova.edu",
                                     calendar)) %>% 
            unique()
        
        max_events <- dim(events)[1]
        event <- 2
        
        while (event <= max_events) {
            last_event <- event - 1
            
            end_time <- events$start[event]
            start_time <- events$end[last_event]
            
            time_diff <- difftime(end_time, 
                                  start_time, 
                                  units = "mins")
            
            if (time_diff > 0 & events$event[last_event] != events$event[event]) {
                max_events <- max_events + 1
                
                free_time <- tibble(
                    event = "Free Time",
                    start = start_time,
                    end = end_time,
                    length = time_diff,
                    calendar = "rozran00@gmail.com"
                )
                
                events <- bind_rows(
                    events[1:last_event, ],
                    free_time,
                    events[event:max_events, ]
                )
            }
            
            event <- event + 1
        }
        
        events <- events %>% 
            filter(!is.na(event)) %>% 
            mutate(start = format(start, "%I:%M:%S %p"),
                   end = format(end, "%I:%M:%S %p"))
    } 
    
    return(events)
}

get_wo_windows_num <- function(events) {
    if (dim(events)[1] > 0) {
        free_time <- events %>% 
            filter(event == "Free Time")
        
        wo_window_num <- dim(free_time[free_time$length >= 60, ])[1]
    } else {
        wo_window_num <- 1
    }
    
    return(wo_window_num)
}

get_wo_windows <- function(events) {
    if (dim(events)[1] > 0) {
        free_time <- events %>% 
            filter(event == "Free Time")
        
        wo_windows <- free_time %>% 
            filter(length >= 60) %>% 
            mutate(wo_windows = paste0(start, "-", end)) %>% 
            pull(wo_windows) %>% 
            paste0(collapse = "; ")
    } else {
        wo_windows <- "all day"
    }
    
    return(wo_windows)
}

## This Week's Workout Schedule
get_wo_schedule_data <- function() {
    wo <- read_csv("../data/workout_schedule.csv") %>% 
        mutate(day_workout = case_when(
            lift == TRUE ~ "Weightlifting",
            soccer == TRUE ~ "Soccer",
            run == TRUE ~ "Running",
            sauna == TRUE ~ "Recovery + Sauna"),
            day_workout = ifelse(walk == TRUE, 
                                 paste0(day_workout, " + Walking"),
                                 day_workout)) %>% 
        select(dotw,
               date, 
               day_workout) %>% 
        rename(Day = dotw,
               Date = date,
               Workout = day_workout)
    
    return(wo)
}

## Today's workout 
get_todays_wo_data <- function(wo) {
    todays_wo <- wo %>% 
        filter(Date == Sys.Date()) %>% 
        pull(Workout)
    
    return(todays_wo)
}

## Workout Availability

get_wo_message <- function(todays_wo, wo_window_num) {
    if (grepl("Soccer", todays_wo)) {
        soccer <- read_csv("../data/futbol_schedule.csv") %>% 
            filter(as_date(datetime) == Sys.Date())
        wo_message <- paste0("You have soccer tonight with ", soccer$team[1], " at ",
                             format(soccer$datetime[1], "%I:%M %p"), " on field ",
                             soccer$field[1], ".")
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, " You also have ", wo_window_num, 
                                 " window(s) long enough to get your walk in today.",
                                 " Try to get your walk in during these windows: ",
                                 wo_windows)
        }
    } else {
        wo_message <- paste0("You have ", wo_window_num, " window(s) long enough ", 
                             "to get your workout in today. Try to get your time ", 
                             "in during these windows: ", wo_windows)
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, " Don't forget... you also have to ", 
                                 "get your walk with Poppy in today.")
        }
    }
    
    return(wo_message)
}

# Cycles Data
get_cycles_data <- function() {
    cycles <- read_csv("../data/cycles.csv") %>% 
        mutate(day_cal = 0.239 * day_kilojoules)
    
    day_start <- unlist(
        str_split(cycles$during, ",")
    )[grepl("\\[", unlist(str_split(cycles$during, ",")))]
    day_start <- str_remove_all(day_start, "\\[|\\'")
    day_start <- as_date(day_start, format = "%Y-%m-%dT%H:%M:%OSZ")
    
    cycles <- cycles %>% 
        mutate(day_start = day_start,
               dotw = weekdays(day_start, abbreviate = TRUE))
    day_start <- NULL
    
    return(cycles)
}

get_cycles_plot_data <- function(cycles) {    
    cycles_sum <- cycles %>% 
        group_by(dotw)  %>% 
        summarise(avg_strain = rollmean(day_strain, 
                                        10, 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  avg_score = rollmean(scaled_strain,
                                       10, 
                                       align = "left", 
                                       fill = NA, 
                                       na.rm = TRUE),
                  avg_kj = rollmean(day_kilojoules,
                                    10, 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE),
                  avg_cal = rollmean(day_cal,
                                     10, 
                                     align = "left", 
                                     fill = NA, 
                                     na.rm = TRUE),
                  date = rollmax(day_start, 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    cycles_plot <- cycles %>%
        head(10) %>% 
        left_join(cycles_sum, by = c("dotw" = "dotw", 
                                     "day_start" = "date")) %>% 
        select(day_start, 
               day_strain,
               scaled_strain,
               day_kilojoules,
               day_cal,
               avg_strain,
               avg_score,
               avg_kj,
               avg_cal,
               dotw) %>% 
        mutate(avg_strain = avg_strain * 1000,
               day_strain = day_strain * 1000)
    
    return(cycles_plot)
}

get_todays_strain_exp_data <- function(cycles_plot) {
    todays_strain_exp <- cycles_plot %>% 
        head(1) %>% 
        pull(avg_strain) %>% 
        round(2)
    
    return(todays_strain_exp)
}

# Recovery
get_recovery_data <- function() {
    recovery <- read_csv("../data/recovery.csv") %>% 
        mutate(date = as_date(date),
               dotw = weekdays(date, abbreviate = TRUE),
               hrv_rmssd = hrv_rmssd * 1000, 
               skin_temp_f = skin_temp_celsius * 9 / 5 + 32,
               spo2 = spo2 / 100)
    
    return(recovery)
}

last_30_day_mean <- function(data, var, start_day = 2, end_day = 31) {
    mean_var <- data %>% 
        slice(start_day:end_day) %>% 
        pull(get(var)) %>% 
        mean(na.rm = TRUE)
    
    return(mean_var)
}


# Workouts 

get_workouts_data <- function() {
    workouts <- read_csv('../data/workouts.csv')
    
    workouts <- workouts %>% 
        bind_cols(
            as_tibble(do.call(rbind, str_split(workouts$during, ","))) %>% 
                mutate(wo_start = as_datetime(V1, format = "['%Y-%m-%dT%H:%M:%OSZ'"),
                       wo_end = as_datetime(V2, format = "'%Y-%m-%dT%H:%M:%OSZ')")) %>% 
                select(wo_start,
                       wo_end)
        ) %>% 
        mutate(date = as_date(wo_start),
               name = case_when(
                   (name == "Powerlifting" | 
                        name == "Functional Fitness") ~ "Weightlifting",
                   1 == 1 ~ name
               ))
    
    return(workouts)
}

get_avg_workouts_data <- function(workouts, condensed = FALSE) {
    avg_workout <- workouts %>% 
        group_by(name)  %>% 
        summarise(event_count = length(name),
                  avg_strain = rollmean(raw_intensity_score * 1000, 
                                        ifelse(event_count < 10, event_count, 10), 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  avg_score = rollmean(intensity_score,
                                       ifelse(event_count < 10, event_count, 10), 
                                       align = "left", 
                                       fill = NA, 
                                       na.rm = TRUE),
                  avg_kj = rollmean(kilojoules,
                                    ifelse(event_count < 10, event_count, 10), 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE),
                  avg_cal = rollmean(kilojoules * 0.239,
                                     ifelse(event_count < 10, event_count, 10), 
                                     align = "left", 
                                     fill = NA, 
                                     na.rm = TRUE),
                  date = rollmax(date, 
                                 ifelse(event_count < 10, event_count, 10), 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) %>% 
        filter(!is.na(name))
    
    if (condensed) {
        avg_workout <- avg_workout %>% 
            slice_head(n = 1)
    }
    
    return(avg_workout)
}

get_todays_workout_data <- function(workouts) {
    todays_workouts <- workouts %>% 
        filter(as_date(created_at) == Sys.Date())
}

# Sleep

get_sleep_data <- function() {
    sleep <- read_csv("../data/sleeps.csv") %>% 
        filter(is_nap == FALSE)
    
    sleep <- sleep %>% 
        bind_cols(
            as_tibble(do.call(rbind, str_split(sleep$during, ","))) %>% 
                mutate(sleep_start = as_datetime(V1, format = "['%Y-%m-%dT%H:%M:%OSZ'"),
                       sleep_end = as_datetime(V2, format = "'%Y-%m-%dT%H:%M:%OSZ')")) %>% 
                select(sleep_start,
                       sleep_end)
        ) %>% 
        mutate(sleep_start = sleep_start + hours(timezone_offset / 100),
               sleep_end = sleep_end + hours(timezone_offset / 100),
               quality_duration = quality_duration / MS_IN_HOUR,
               latency = latency / MS_IN_HOUR,
               debt_pre = debt_pre / MS_IN_HOUR,
               debt_post = debt_post / MS_IN_HOUR,
               need_from_strain = need_from_strain / MS_IN_HOUR,
               sleep_need = sleep_need / MS_IN_HOUR,
               habitual_sleep_need = habitual_sleep_need / MS_IN_HOUR,
               time_in_bed = time_in_bed / MS_IN_HOUR,
               light_sleep_duration = light_sleep_duration / MS_IN_HOUR,
               slow_wave_sleep_duration = slow_wave_sleep_duration / MS_IN_HOUR,
               rem_sleep_duration = rem_sleep_duration / MS_IN_HOUR,
               wake_duration = wake_duration / MS_IN_HOUR,
               arousal_time = arousal_time / MS_IN_HOUR,
               credit_from_naps = credit_from_naps / MS_IN_HOUR,
               projected_sleep = projected_sleep / MS_IN_HOUR,
               total_sleep = credit_from_naps + quality_duration,
               date = as_date(sleep_end),
               dotw = weekdays(date, abbreviate = TRUE)) %>% 
        select(date,
               dotw,
               sleep_start,
               sleep_end,
               score, 
               total_sleep,
               quality_duration,
               latency,
               debt_pre,
               debt_post,
               need_from_strain,
               sleep_need,
               habitual_sleep_need,
               disturbances,
               time_in_bed,
               light_sleep_duration,
               slow_wave_sleep_duration,
               rem_sleep_duration,
               cycles_count,
               wake_duration,
               arousal_time,
               in_sleep_efficiency,
               credit_from_naps,
               respiratory_rate,
               sleep_consistency) 
    
    return(sleep)
}

get_sleep_plot_data <- function(sleep) {    
    cycles_sum <- sleep %>% 
        group_by(dotw)  %>% 
        summarise(avg_strain = rollmean(day_strain, 
                                        10, 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  avg_score = rollmean(scaled_strain,
                                       10, 
                                       align = "left", 
                                       fill = NA, 
                                       na.rm = TRUE),
                  avg_kj = rollmean(day_kilojoules,
                                    10, 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE),
                  avg_cal = rollmean(day_cal,
                                     10, 
                                     align = "left", 
                                     fill = NA, 
                                     na.rm = TRUE),
                  date = rollmax(day_start, 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    cycles_plot <- cycles %>%
        head(10) %>% 
        left_join(cycles_sum, by = c("dotw" = "dotw", 
                                     "day_start" = "date")) %>% 
        select(day_start, 
               day_strain,
               scaled_strain,
               day_kilojoules,
               day_cal,
               avg_strain,
               avg_score,
               avg_kj,
               avg_cal,
               dotw) %>% 
        mutate(avg_strain = avg_strain * 1000,
               day_strain = day_strain * 1000)
    
    return(cycles_plot)
}


get_expected_strain_data <- function(todays_wo, todays_workouts, avg_workout) {
    # If there are two events in today's plan
    if (grepl("\\+", todays_wo)) {
        todays_wo <- unlist(str_split(todays_wo, " \\+ "))
    }
    
    expected_strain <- 0
    
    # Have you done those workouts yet? 
    for (two in todays_wo) {
        
        if (two %in% todays_workouts$name) {
            next
        } 
        
        expected_strain <- expected_strain + avg_workout$avg_strain[avg_workout$name == two]
    }
    
    return(expected_strain)
}

get_todays_strain_plot_data <- function(expected_strain) {
    plot_data <- cycles_plot %>% 
        head(1)  %>% 
        select(day_start,
               day_strain, 
               avg_strain, 
               dotw)%>% 
        mutate(additional_strain = expected_strain + day_strain)
    
    return(plot_data)
}


get_recovery_trendz_data <- function(dat, fieldz, dayz) {
    recovery_trends <- tibble()
    
    for (day in dayz) {
        recovery_trends_day <- dat %>% 
            filter(date < Sys.Date()) %>% 
            select(date,
                   dotw) %>% 
            mutate(days = day)
        
        for (field in fieldz) {
            ma_name <- paste0(field, "_ma")
            
            recovery_trends_tmp <- dat %>% 
                filter(date < Sys.Date()) %>% 
                mutate(tmp = rollmean(get(field), 
                                      day, 
                                      align = "left", 
                                      fill = NA, 
                                      na.rm = TRUE)) %>% 
                select(as.name(field),
                       tmp) 
            
            names(recovery_trends_tmp) <- c(field, ma_name)
            
            recovery_trends_day <- bind_cols(recovery_trends_day, 
                                             recovery_trends_tmp)
            recovery_trends_tmp <- NULL
        }
        recovery_trends <- bind_rows(recovery_trends, recovery_trends_day)
        recovery_trends_day <- NULL
    }
    
    for (field in fieldz) {
        ma_name <- paste0(field, "_ma")
        nm_name <- paste0(ma_name, "_nm")
        old_names <- names(recovery_trends)
        
        recovery_trends <- recovery_trends %>% 
            mutate(tmp = ((get(ma_name) - min(get(ma_name), na.rm = TRUE)) / 
                              (max(get(ma_name), na.rm = TRUE) - 
                                   min(get(ma_name), na.rm = TRUE))))
        
        names(recovery_trends) <- c(old_names, nm_name)
    }
    
    plot_dat <- recovery_trends %>% 
        select(date, 
               dotw,
               days,
               ends_with("_nm")) %>% 
        pivot_longer(!c(date, dotw, days), names_to = "measure", values_to = "values") %>% 
        mutate(length = factor(paste0(days, "d Moving Average")),
               alpha = (days - min(days)) / (max(days) - min(days)))
    
    return(plot_dat)
}