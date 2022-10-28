####################
## Data Functions ##
####################

#######################
# Part 1 - Get Events #
#######################

# Get Today's Events
get_events_data <- function() {
    events <- read_csv("../data/calendar_events.csv") 
    
    if (dim(events)[1] > 0) {
        events <- events %>% 
            filter(response != "declined") %>% 
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
            end.dateTime = strptime(paste0(Sys.Date(), ":19:30:00"), 
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

# How many work out windows do you have today?
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

# What are the work out windows?
get_wo_windows <- function(events) {
    if (dim(events)[1] > 0) {
        free_time <- events %>% 
            filter(event == "Free Time")
        
        wo_windows <- free_time %>% 
            filter(length >= 60) %>% 
            mutate(wo_windows = paste0(start, " to ", end)) %>% 
            pull(wo_windows) %>% 
            paste0(collapse = "\n- ")
        
        wo_windows <- paste0("\n\n- ", wo_windows)
    } else {
        wo_windows <- "all day"
    }
    
    return(wo_windows)
}
