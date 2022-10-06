# Monitor Metrics on the following: 

## Strain ##
# 1. Day Strain (cycles: day_strain)
get_anomoly_data <- function(dat, var, date_var) { 
    dat_sum <- dat %>% 
        group_by(dotw)  %>% 
        summarise(avg_strain = rollmean(get(var), 
                                        10, 
                                        align = "left", 
                                        fill = NA, 
                                        na.rm = TRUE),
                  sd_strain = rollapply(get(var),
                                        10,
                                        align = "left",
                                        fill = NA,
                                        na.rm = TRUE,
                                        FUN = sd),
                  date = rollmax(get(date_var), 
                                 10, 
                                 align = "left", 
                                 fill = NA, 
                                 na.rm = TRUE)) 
    
    # If you've been below the 30 day moving average for so many days in a row, alert
    # If you've been above the 30 day moving average for so many days in a row, alert
    # If you've trended negative for so many days in a row, alert
    # If you've trended positive for so many days in a row, alert
    # If you've exceeded the last 10 week (for this day) moving average by so much, alert
    # If you've fall below the last 10 week (for this day) moving average by so much, alert
    
    dat_anom <- dat %>% 
        rename(date = date_var) %>% 
        filter(date < Sys.Date()) %>% 
        left_join(dat_sum, by = "date") %>% 
        select(date, avg_strain, sd_strain, var) %>% 
        arrange(date) %>% 
        mutate(normp = pnorm(get(var), 
                             mean = avg_strain, 
                             sd = sd_strain),
               ma_3 = NA,
               ou_1_3 = NA,
               ma_7 = NA,
               ou_1_7 = NA,
               ou_3_7 = NA,
               ma_14 = NA,
               ou_1_14 = NA,
               ou_3_14 = NA,
               ou_7_14 = NA,
               ma_30 = NA,
               ou_1_30 = NA,
               ou_3_30 = NA,
               ou_7_30 = NA,
               ou_14_30 = NA,
               percent_dif_10_week = (get(var) / avg_strain) - 1)
    
    for(rown in 2:length(dat_anom$date)) {
        end <- ifelse((rown - 1) <= 0, 1, rown - 1)
        three_start <- ifelse((rown - 3) <= 0, 1, rown - 3)
        seven_start <- ifelse((rown - 7) <= 0, 1, rown - 7)
        ft_start <- ifelse((rown - 14) <= 0, 1, rown - 14)
        thirty_start <- ifelse((rown - 30) <= 0, 1, rown - 30)
        
        last_val <- dat_anom %>% 
            slice(end:end) %>% 
            pull(var)
        current_val <- dat_anom %>% 
            slice(rown:rown) %>% 
            pull(var)
        
        dat_anom$ma_3[rown] <- mean(dat_anom %>% 
                                        slice(three_start:end) %>% 
                                        pull(var), 
                                    na.rm = TRUE)
        dat_anom$ma_7[rown] <- mean(dat_anom %>% 
                                        slice(seven_start:end) %>% 
                                        pull(var), 
                                    na.rm = TRUE)
        dat_anom$ma_14[rown] <- mean(dat_anom %>% 
                                         slice(ft_start:end) %>% 
                                         pull(var), 
                                     na.rm = TRUE)
        dat_anom$ma_30[rown] <- mean(dat_anom %>% 
                                         slice(thirty_start:end) %>% 
                                         pull(var), 
                                     na.rm = TRUE)
    }
    
    return(dat_anom)
}

dat_anom <- get_anomoly_data(cycles, "day_strain", "day_start")

plot_dat1 <- dat_anom %>% 
    filter(date >= Sys.Date() - days(7)) %>% 
    select(date, 
           var,
           ma_3,
           ma_7,
           ma_14,
           ma_30) 

plot_approx <- tibble(fake_col = rep(NA, 500))
for (col in names(plot_dat1)) {
    var_class <- plot_dat1 %>% 
        pull(get(col)) %>% 
        class()
    
    var_content <- plot_dat1 %>% 
        pull(get(col))
    
    if(var_class == "Date") {
        approx_data <- approx(
            as.POSIXct(var_content),
            n = 500
        )
    } else {
        approx_data <- approx(
            var_content,
            n = 500
        )
    }
    
    tmp <- tibble(approx_data$y)
    names(tmp) <- col
    
    plot_approx <- bind_cols(plot_approx, tmp)
}


    
plot_dat_long <- plot_approx %>% 
    select(-fake_col) %>% 
    rename(`Single Day` = var,
           `3 Day MA` = ma_3,
           `7 Day MA` = ma_7,
           `14 Day MA` = ma_14,
           `30 Day MA` = ma_30) %>% 
    pivot_longer(!date,
                 names_to = "category1",
                 values_to = "val1") %>% 
    mutate(category1 = factor(category1, 
                              levels = c("Single Day", 
                                         "3 Day MA", 
                                         "7 Day MA", 
                                         "14 Day MA", 
                                         "30 Day MA")))

plot_dat <- plot_dat_long %>% 
    full_join(plot_dat_long, by = "date") %>% 
    mutate(strain_perc = (val1.x / val1.y) - 1,
           upper = ifelse(strain_perc >= 0, strain_perc, 0),
           lower = ifelse(strain_perc < 0, strain_perc, 0),
           day_start = as_datetime(date)) %>% 
    rename(category1 = category1.x, 
           category2 = category1.y) %>% 
    filter((category1 == "Single Day" & category2 == "3 Day MA") | 
               (category1 == "Single Day" & category2 == "7 Day MA") |
               (category1 == "Single Day" & category2 == "14 Day MA") | 
               (category1 == "Single Day" & category2 == "30 Day MA") | 
               (category1 == "3 Day MA" & category2 == "7 Day MA") | 
               (category1 == "3 Day MA" & category2 == "14 Day MA") | 
               (category1 == "3 Day MA" & category2 == "30 Day MA") | 
               (category1 == "7 Day MA" & category2 == "14 Day MA") | 
               (category1 == "7 Day MA" & category2 == "30 Day MA") |
               (category1 == "14 Day MA" & category2 == "30 Day MA"))

ggplot(plot_dat, aes(x = day_start)) + 
    geom_ribbon(aes(ymin = 0, ymax = upper), fill = "#50C878") + 
    geom_ribbon(aes(ymin = 0, ymax = lower), fill = "#EE4B2B") + 
    geom_line(aes(y = strain_perc)) + 
    facet_grid(category1 ~ category2, switch = "y") + 
    scale_y_continuous(labels = percent_format()) + 
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "light gray"),
          panel.border = element_rect(fill = NA, color = "black"))



library(scales)

# If you've been below the 30 day moving average for so many days in a row, alert
# If you've been above the 30 day moving average for so many days in a row, alert
# If you've trended negative for so many days in a row, alert
# If you've trended positive for so many days in a row, alert
# If you've exceeded the last 10 week (for this day) moving average by so much, alert
# If you've fall below the last 10 week (for this day) moving average by so much, alert


# 2. Calories (cycles: day_cal)
## Recovery ##
# 3. Heart Rate Variability (recovery: hrv_rmssd)
# 4. Resting Heart Rate (recovery: resting_heart_rate)
# 5. Skin Temp (recovery: skin_temp_f)
# 6. SpO2 (recovery: spo2)
## Sleep ##
# 7. Total Sleep (sleep: total_sleep)
# 8. Latency (sleep: latency)
# 9. Light Sleep Duration (sleep: light_sleep_duration)
# 10. Slow Wave Sleep Duration (sleep: slow_wave_sleep_duration)
# 11. REM Sleep Duration (sleep: rem_sleep_duration)
# 12. Wake Duration (sleep: wake_duration)
# 13. Cycles Count (sleep: cycles_count)
# 14. Disturbances (sleep: disturbances)
# 15. Respiratory Rate (sleep: respiratory_rate)
## Workouts ##
# 16. Strain for yesterday's workout(s) (workout: raw_intensity_score)
