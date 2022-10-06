# Monitor Metrics on the following: 

## Strain ##
# 1. Day Strain (cycles: day_strain)
cycles_sum <- cycles %>% 
    group_by(dotw)  %>% 
    summarise(avg_strain = rollmean(day_strain, 
                                    10, 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = TRUE),
              sd_strain = rollapply(day_strain,
                                    10,
                                    align = "left",
                                    fill = NA,
                                    na.rm = TRUE,
                                    FUN = sd),
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

cycles_anom <- cycles %>% 
    filter(day_start < Sys.Date()) %>% 
    left_join(cycles_sum, by = c("day_start" = "date")) %>% 
    select(day_start, avg_strain, sd_strain, day_strain) %>% 
    arrange(day_start) %>% 
    mutate(normp = pnorm(day_strain, 
                         mean = avg_strain, 
                         sd = sd_strain),
           ma_3 = rollmean(day_strain, 
                           3, 
                           align = "right", 
                           na.rm = TRUE, 
                           fill = NA),
           ma_7 = rollmean(day_strain, 
                            7, 
                            align = "right", 
                            na.rm = TRUE, 
                            fill = NA),
           ma_14 = rollmean(day_strain, 
                            14, 
                            align = "right", 
                            na.rm = TRUE, 
                            fill = NA),
           ma_30 = rollmean(day_strain, 
                            30, 
                            align = "right", 
                            na.rm = TRUE, 
                            fill = NA),
           percent_dif_10_week = (day_strain / avg_strain) - 1) %>% 
    arrange(desc(day_start))

plot_dat1 <- cycles_anom %>% 
    filter(day_start >= Sys.Date() - days(7)) %>% 
    select(day_start, 
           day_strain,
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
    rename(`Day Strain` = day_strain,
           `3 Day MA` = ma_3,
           `7 Day MA` = ma_7,
           `14 Day MA` = ma_14,
           `30 Day MA` = ma_30) %>% 
    pivot_longer(!day_start,
                 names_to = "category1",
                 values_to = "val1") %>% 
    mutate(category1 = factor(category1, 
                              levels = c("Day Strain", 
                                         "3 Day MA", 
                                         "7 Day MA", 
                                         "14 Day MA", 
                                         "30 Day MA")))

plot_dat <- plot_dat_long %>% 
    full_join(plot_dat_long, by = "day_start") %>% 
    mutate(strain_perc = (val1.x / val1.y) - 1,
           upper = ifelse(strain_perc >= 0, strain_perc, 0),
           lower = ifelse(strain_perc < 0, strain_perc, 0),
           day_start = as_datetime(day_start)) %>% 
    rename(category1 = category1.x, 
           category2 = category1.y) %>% 
    filter((category1 == "Day Strain" & category2 == "3 Day MA") | 
               (category1 == "Day Strain" & category2 == "7 Day MA") |
               (category1 == "Day Strain" & category2 == "14 Day MA") | 
               (category1 == "Day Strain" & category2 == "30 Day MA") | 
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
    scale_y_continuous(labels = percent_format())



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
