

get_cycles_data <- function() {
    cycles <- read_csv("../data/cycles.csv") %>% 
        mutate(day_cal = 0.239 * day_kilojoules)
    
    day_start <- unlist(
        str_split(cycles$days, ",")
    )[grepl("\\[", unlist(str_split(cycles$days, ",")))]
    day_start <- str_remove_all(day_start, "\\[|\\'")
    day_start <- as_date(day_start, format = "%Y-%m-%d")
    
    cycles <- cycles %>% 
        mutate(date = day_start,
               dotw = weekdays(day_start, abbreviate = TRUE))
    day_start <- NULL
    
    return(cycles)
}
