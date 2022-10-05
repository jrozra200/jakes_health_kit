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
    left_join(cycles_sum, by = c("day_start" = "date")) %>% 
    select(day_start, dotw.x, avg_strain, sd_strain, day_strain) %>% 
    arrange(day_start) %>% 
    mutate(normp = pnorm(day_strain, 
                         mean = avg_strain, 
                         sd = sd_strain),
           ma = rollmean(day_strain, 
                         30, 
                         align = "right", 
                         na.rm = TRUE, 
                         # partial = TRUE,
                         fill = NA),
           under_ma = case_when(
               is.na(ma) | is.na(day_strain) ~ 0,
               day_strain == ma ~ 0,
               day_strain < ma ~ -1,
               day_strain > ma ~ 1
           ),
           over_under = rollsum(under_ma, 
                                7,
                                align = "right", 
                                na.rm = TRUE, 
                                # partial = TRUE,
                                fill = NA),
           percent_dif = (day_strain / avg_strain) - 1) %>% 
    arrange(desc(day_start))



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
