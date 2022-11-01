
last_30_day_ma <- function(data, 
                           var, 
                           date_var, 
                           start_date, 
                           end_date, 
                           days_back = 30) {
    mean_var <- data %>%
        arrange(desc(get(date_var))) %>% 
        mutate(lead_var = lead(get(var), 1),
               ma = rollmean(lead_var, 
                             days_back, 
                             fill = NA, 
                             align = "left", 
                             na.rm = TRUE)) %>% 
        filter(get(date_var) >= start_date & 
                   get(date_var) <= end_date) %>% 
        pull(ma)
    
    
    return(mean_var)
}
