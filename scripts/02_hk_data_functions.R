# UNIVERSAL VARIABLES
MS_IN_HOUR <- 3600000
KJ_TO_CAL <- 0.239
M_TO_MILE <- 1609.344
M_TO_FT <- 3.28084

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
            mutate(wo_windows = paste0(start, " - ", end)) %>% 
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

get_wo_message <- function(todays_wo, wo_window_num, wo_windows) {
    if (grepl("Soccer", todays_wo)) {
        soccer <- read_csv("../data/futbol_schedule.csv") %>% 
            filter(as_date(datetime) == Sys.Date())
        wo_message <- paste0("You have soccer tonight with ", soccer$team[1], " at ",
                             format(soccer$datetime[1], "%I:%M %p"), " on field ",
                             soccer$field[1], ".")
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, " You also have ", wo_window_num, 
                                 " window(s) long enough to get your walk in today.",
                                 "\n\nTry to get your walk in during these windows:\n\t",
                                 wo_windows, ".\n\n")
        }
    } else {
        wo_message <- paste0("You have ", wo_window_num, " window(s) long enough ", 
                             "to get your workout in today.\n\nTry to get your time ", 
                             "in during these windows:\n\t", wo_windows, ".\n\n")
        
        if (grepl("\\+ Walk", todays_wo)) {
            wo_message <- paste0(wo_message, "\n\nDon't forget... you also have to ", 
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
        mutate(date = day_start,
               dotw = weekdays(day_start, abbreviate = TRUE))
    day_start <- NULL
    
    return(cycles)
}

get_cycles_plot_data <- function(cycles) {    
    cycles_sum <- cycles %>% 
        group_by(dotw)  %>% 
        summarise(lead_strain = lead(day_strain, 1),
                  avg_strain = rollmean(lead_strain, 
                                        10, 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  uq_strain = rollapply(lead_strain,
                                        width = 10,
                                        align = "left",
                                        fill = NA,
                                        FUN = quantile,
                                        probs = 0.95,
                                        na.rm = TRUE),
                  lq_strain = rollapply(lead_strain,
                                        width = 10,
                                        align = "left",
                                        fill = NA,
                                        FUN = quantile,
                                        probs = 0.05,
                                        na.rm = TRUE),
                  lead_score = lead(scaled_strain, 1),
                  avg_score = rollmean(lead_score,
                                       10, 
                                       align = "left", 
                                       fill = NA, 
                                       na.rm = TRUE),
                  uq_score = rollapply(lead_score,
                                       width = 10,
                                       align = "left",
                                       fill = NA,
                                       FUN = quantile,
                                       probs = 0.95,
                                       na.rm = TRUE),
                  lq_score = rollapply(lead_score,
                                       width = 10,
                                       align = "left",
                                       fill = NA,
                                       FUN = quantile,
                                       probs = 0.95,
                                       na.rm = TRUE),
                  lead_kj = lead(day_kilojoules, 1),
                  avg_kj = rollmean(lead_kj,
                                    10, 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE),
                  uq_kj = rollapply(lead_kj,
                                    width = 10,
                                    align = "left",
                                    fill = NA,
                                    FUN = quantile,
                                    probs = 0.95,
                                    na.rm = TRUE),
                  lq_kj = rollapply(lead_kj,
                                    width = 10,
                                    align = "left",
                                    fill = NA,
                                    FUN = quantile,
                                    probs = 0.95,
                                    na.rm = TRUE),
                  lead_cal = lead(day_cal, 1),
                  avg_cal = rollmean(lead_cal,
                                     10, 
                                     align = "left", 
                                     fill = NA, 
                                     na.rm = TRUE),
                  uq_cal = rollapply(lead_cal,
                                    width = 10,
                                    align = "left",
                                    fill = NA,
                                    FUN = quantile,
                                    probs = 0.95,
                                    na.rm = TRUE),
                  lq_cal = rollapply(lead_score,
                                    width = 10,
                                    align = "left",
                                    fill = NA,
                                    FUN = quantile,
                                    probs = 0.95,
                                    na.rm = TRUE),
                  date = rollmax(date, 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    cycles_plot <- cycles %>%
        head(10) %>% 
        left_join(cycles_sum, by = c("dotw" = "dotw", 
                                     "date" = "date")) %>% 
        select(date, 
               day_strain,
               scaled_strain,
               day_kilojoules,
               day_cal,
               avg_strain,
               uq_strain,
               lq_strain,
               avg_score,
               uq_score,
               lq_score,
               avg_kj,
               uq_kj,
               lq_kj,
               avg_cal,
               uq_cal,
               lq_cal,
               dotw) %>% 
        mutate(avg_strain = avg_strain * 1000,
               day_strain = day_strain * 1000,
               uq_strain = uq_strain * 1000,
               lq_strain = lq_strain * 1000)
    
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

last_30_day_ma <- function(data, 
                           var, 
                           date_var, 
                           start_date, 
                           end_date, 
                           days_back = 30) {
    mean_var <- data %>%
        mutate(ma = rollmean(get(var), 
                             days_back, 
                             fill = NA, 
                             align = "left", 
                             na.rm = TRUE)) %>% 
        filter(get(date_var) >= start_date & 
                   get(date_var) <= end_date) %>% 
        pull(ma)
        
    
    return(mean_var)
}


# Workouts 

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

get_avg_workouts_data <- function(workouts, condensed = FALSE) {
    avg_workout <- workouts %>% 
        group_by(name)  %>% 
        arrange(desc(date)) %>% 
        summarise(avg_strain = rollmean(raw_intensity_score, 
                                        10,
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE,
                                        partial = TRUE),
                  avg_score = rollmean(intensity_score,
                                       10,
                                       align = "left", 
                                       fill = NA, 
                                       na.rm = TRUE,
                                       partial = TRUE),
                  avg_kj = rollmean(kilojoules,
                                    10,
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE,
                                    partial = TRUE),
                  avg_cal = rollmean(kilojoules * 0.239,
                                     10,
                                     align = "left", 
                                     fill = NA, 
                                     na.rm = TRUE,
                                     partial = TRUE),
                  date = rollmax(date, 
                                 10,
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE,
                                 partial = TRUE)) %>% 
        filter(!is.na(name))
    
    if (condensed) {
        avg_workout <- avg_workout %>% 
            slice_head(n = 1)
    }
    
    return(avg_workout)
}

get_todays_workout_data <- function(workouts) {
    todays_workouts <- workouts %>% 
        filter(as_date(date) == Sys.Date())
    
    return(todays_workouts)
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
                  date = rollmax(date, 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    cycles_plot <- cycles %>%
        head(10) %>% 
        left_join(cycles_sum, by = c("dotw" = "dotw", 
                                     "date" = "date")) %>% 
        select(date, 
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

get_todays_strain_plot_data <- function(cycles_plot, expected_strain) {
    plot_data <- cycles_plot %>% 
        head(1)  %>% 
        select(date,
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

get_workout_health_plot_data <- function(workouts, avg_workout_long) {
    plot_data <- workouts %>% 
        filter(!is.na(name) & 
                   date >= Sys.Date() - days(10)) %>% 
        left_join(avg_workout_long, by = c("name", "date")) %>% 
        select(date, 
               dotw,
               name,
               intensity_score,
               raw_intensity_score,
               kilojoules,
               max_heart_rate,
               average_heart_rate,
               distance,
               altitude_gain,
               altitude_change,
               zone_durations,
               avg_strain, # RAW
               avg_score, # NOT RAW
               avg_kj,
               avg_cal) %>% 
        group_by(date) %>% 
        arrange(desc(date), desc(name)) %>% 
        mutate(label_y = cumsum(raw_intensity_score)) %>% 
        ungroup()
}

get_workout_health_tab_data <- function(workouts, avg_workout_long) {
    plot_data <- workouts %>% 
        filter(!is.na(name) & 
                   date >= floor_date(Sys.Date(), week_start = 1, unit = "week")) %>% 
        left_join(avg_workout_long, by = c("name", "date")) %>% 
        mutate(percent_over = percent((raw_intensity_score - avg_strain) / avg_strain, accuracy = 0.01)) %>% 
        select(date, 
               dotw,
               name,
               raw_intensity_score,
               percent_over,
               calories,
               max_heart_rate,
               average_heart_rate,
               distance,
               altitude_gain,
               avg_strain, # RAW
               avg_cal)
}

get_sports <- function() {
    sports <- read_csv("../data/sports.csv") %>% 
        select(name,
               category)
    
    return(sports)
}

get_soccer_data <- function() {
    soccer <- read_csv("../data/futbol_schedule.csv")
    
    return(soccer)
}

get_class_data <- function() {
    class <- read_csv("../data/class_schedule.csv")
    
    return(class)
}

get_workout_dates_data <- function(range_start) {
    workout_dates <- get_workouts_data() %>% 
        filter(date >= range_start) %>% 
        left_join(get_sports(), by = "name") %>% 
        mutate(new_cat = str_sub(category, 1, 5)) %>% 
        group_by(date) %>% 
        summarise(workouts_completed = paste0(name, collapse = " + "),
                  category = paste0(new_cat, collapse = " + "),
                  act_strain = sum(raw_intensity_score)) %>% 
        full_join(get_cycles_data(), by = c("date" = "date")) %>% 
        filter(date >= range_start & 
                   date <= Sys.Date() - days(1)) %>% 
        select(date, workouts_completed, category, act_strain, day_strain) %>% 
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
                                           Sys.Date() - days(1)) * 1000,
               over_avg = ifelse(day_strain >= avg_strain, 1, -1),
               plus_minus = cumsum(over_avg),
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
    
    return(workout_dates)
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

get_todays_wo <- function() {
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
    workout_dates <- get_workout_dates_data(range_start)
    
    category_sum <- get_category_sum_data(workout_dates,
                                          NUM_CARDIO,
                                          NUM_MUSCULAR,
                                          NUM_NON,
                                          NUM_RESTORE)
    
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
        today_type == "muscular" ~ "Weightlifting",
        today_type == "cardiovascular" & current_rating < MAX_DAYS_UNDER ~ "Running",
        today_type == "cardiovascular" & current_rating >= MAX_DAYS_UNDER ~ "Spin"
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
    
    return(today_name)
}

get_anomoly_data <- function(dat, var, date_var) { 
    
    dat_sum <- get(dat) %>% 
        rename(int_var = var,
               date = date_var) %>% 
        arrange(desc(date)) %>% 
        group_by(dotw)  %>% 
        mutate(new_day_strain = lead(int_var, 1)) %>% 
        summarise(avg_strain = rollmean(new_day_strain,
                                        10, 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  sd_strain = rollapply(new_day_strain,
                                        10,
                                        align = "left",
                                        fill = NA,
                                        na.rm = TRUE,
                                        FUN = sd),
                  date = rollmax(date, 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    # If you've been below the 30 day moving average for so many days in a row, alert
    # If you've been above the 30 day moving average for so many days in a row, alert
    # If you've trended negative for so many days in a row, alert
    # If you've trended positive for so many days in a row, alert
    # If you've exceeded the last 10 week (for this day) moving average upper quartile, alert
    # If you've fall below the last 10 week (for this day) moving average lower quartile, alert
    
    dat_anom <- get(dat) %>% 
        rename(date = date_var,
               int_var = var) %>% 
        filter(((date <= Sys.Date() & 
                     (dat == "recovery" | 
                          dat == "sleep")) | 
                    (date <= Sys.Date() - days(1) & 
                         dat == "cycles"))) %>% 
        left_join(dat_sum, by = "date") %>% 
        select(date, avg_strain, sd_strain, int_var) %>% 
        arrange(desc(date)) %>% 
        mutate(avg_vs_day = ifelse(int_var > avg_strain, 1, -1),
               three_day_trend_avg = rollsum(avg_vs_day,
                                             3,
                                             align = "left",
                                             fill = NA,
                                             na.rm = TRUE),
               alert_tdt_avg = ifelse(three_day_trend_avg == 3 |
                                          three_day_trend_avg == -3, 
                                      TRUE,
                                      FALSE),
               new_day_strain = lead(int_var, 1),
               today_greater = ifelse(int_var > new_day_strain, 1, -1),
               three_day_trend = rollsum(today_greater,
                                         3,
                                         align = "left",
                                         fill = NA,
                                         na.rm = TRUE),
               alert_tdt = ifelse(three_day_trend == 3 |
                                      three_day_trend == -3, 
                                  TRUE,
                                  FALSE),
               normp = pnorm(int_var, 
                             mean = avg_strain, 
                             sd = sd_strain),
               ma_3 = rollmean(new_day_strain,
                               3, 
                               align = "left", 
                               fill = NA, 
                               na.rm = TRUE),
               ou_1_3 = ifelse(int_var > ma_3, TRUE, FALSE),
               ma_7 = rollmean(new_day_strain,
                               7, 
                               align = "left", 
                               fill = NA, 
                               na.rm = TRUE),
               ou_1_7 = ifelse(int_var > ma_7, TRUE, FALSE),
               ou_3_7 = ifelse(ma_3 > ma_7, TRUE, FALSE),
               ma_14 = rollmean(new_day_strain,
                                14, 
                                align = "left", 
                                fill = NA, 
                                na.rm = TRUE),
               ou_1_14 = ifelse(int_var > ma_14, TRUE, FALSE),
               ou_3_14 = ifelse(ma_3 > ma_14, TRUE, FALSE),
               ou_7_14 = ifelse(ma_7 > ma_14, TRUE, FALSE),
               ma_30 = rollmean(new_day_strain,
                                30, 
                                align = "left", 
                                fill = NA, 
                                na.rm = TRUE),
               ou_1_30 = ifelse(int_var > ma_30, 1, -1),
               three_day_trend_30 = rollsum(ou_1_30,
                                            3,
                                            align = "left",
                                            fill = NA,
                                            na.rm = TRUE),
               alert_tdt_30 = ifelse(three_day_trend_30 == 3 |
                                         three_day_trend_30 == -3, 
                                     TRUE,
                                     FALSE),
               ou_3_30 = ifelse(ma_3 > ma_30, TRUE, FALSE),
               ou_7_30 = ifelse(ma_7 > ma_30, TRUE, FALSE),
               ou_14_30 = ifelse(ma_14 > ma_30, TRUE, FALSE),
               percent_dif_10_week = (int_var / avg_strain) - 1,
               npdtw = lead(percent_dif_10_week, 1),
               lq_pdtw = as.numeric(
                   rollapply(npdtw,
                             width = 10000,
                             FUN = quantile,
                             probs = 0.05,
                             na.rm = TRUE,
                             align = "left",
                             partial = TRUE,
                             fill = NA)),
               uq_pdtw = as.numeric(
                   rollapply(npdtw,
                             width = 10000,
                             FUN = quantile,
                             probs = 0.95,
                             na.rm = TRUE,
                             align = "left",
                             partial = TRUE,
                             fill = NA)),
               alert_diff_10_week = ifelse(
                   percent_dif_10_week < lq_pdtw | 
                       percent_dif_10_week > uq_pdtw,
                   TRUE, 
                   FALSE))
    
    names_of_var <- names(dat_anom)
    names_of_var <- ifelse(names_of_var == "int_var", var, names_of_var)
    names(dat_anom) <- names_of_var
    
    dat_anom$variable <- var
    dat_anom$df <- dat
    
    alert_tdt_avg <- ifelse(dat_anom$alert_tdt_avg == TRUE, 
                            ifelse(dat_anom$three_day_trend_avg > 0, 
                                   paste0("You have had 3 days in a row OVER the daily average ",
                                          "(previous 10 weeks for that day of the week - i.e.: this ",
                                          "previous Monday was compared to the 10 week average only ",
                                          "for Mondays) for ", var, "."),
                                   paste0("You have had 3 days in a row UNDER the daily average ",
                                          "(previous 10 weeks for that day of the week - i.e.: this ",
                                          "previous Monday was compared to the 10 week average only ",
                                          "for Mondays) for ", var, ".")),
                            "")
    
    alert_tdt <- ifelse(dat_anom$alert_tdt == TRUE,
                        ifelse(dat_anom$three_day_trend > 0,
                               paste0("Each day over the past 3 days, you've INCREASED your amount", 
                                      " of ", var, " (three days ago was less than two days ago ",
                                      "which was less than yesterday)."),
                               paste0("Each day over the past 3 days, you've DECREASED your amount", 
                                      " of ", var, " (three days ago was more than two days ago ",
                                      "which was more than yesterday).")),
                        "")
    
    alert_tdt_30 <- ifelse(dat_anom$alert_tdt_30 == TRUE,
                           ifelse(dat_anom$three_day_trend_30 > 0,
                                  paste0("You have had 3 days in a row OVER the 30 day moving ",
                                         "average (average of previous 30 days) for ", var, "."),
                                  paste0("You have had 3 days in a row UNDER the 30 day moving ",
                                         "average (average of previous 30 days) for ", var, ".")),
                           "")
    
    alert_diff_10_week <- ifelse(dat_anom$alert_diff_10_week == TRUE,
                                 ifelse(dat_anom$percent_dif_10_week > 0,
                                        paste0("You have EXCEEDED the 95th percentile for ", var, "."),
                                        paste0("You were UNDER the 5th percentile for ", var, ".")),
                                 "")
    
    messages <- paste0(alert_tdt_avg, alert_tdt, alert_tdt_30, alert_diff_10_week,
                       sep = "\n")
    
    dat_anom$message <- messages
    
    return(dat_anom)
}

get_tracked_measures <- function() {
    return(
        tibble(
            dat = c(rep("cycles", 2),
                    rep("recovery", 4),
                    rep("sleep", 9)),
            var = c("day_strain",
                    "day_cal",
                    "hrv_rmssd",
                    "resting_heart_rate",
                    "skin_temp_f",
                    "spo2",
                    "total_sleep",
                    "latency",
                    "light_sleep_duration",
                    "slow_wave_sleep_duration",
                    "rem_sleep_duration",
                    "wake_duration",
                    "cycles_count",
                    "disturbances",
                    "respiratory_rate"),
            date_var = c(rep("date", 2),
                         rep("date", 4),
                         rep("date", 9))
        )
    )
}
