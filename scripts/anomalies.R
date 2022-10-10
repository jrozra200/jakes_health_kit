# Monitor Metrics on the following: 

## Strain ##
# 1. Day Strain (cycles: day_strain)
get_anomoly_data <- function(dat, vars, date_var) { 
    
    final_dat <- tibble()
    
    for (var in vars) {
        dat_sum <- dat %>% 
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
        
        dat_anom <- dat %>% 
            rename(date = date_var,
                   int_var = var) %>% 
            filter(date < Sys.Date()) %>% 
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
                                 probs = 0.25,
                                 na.rm = TRUE,
                                 align = "left",
                                 partial = TRUE,
                                 fill = NA)),
                   uq_pdtw = as.numeric(
                       rollapply(npdtw,
                                 width = 10000,
                                 FUN = quantile,
                                 probs = 0.75,
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
        names_of_var <- ifelse(names_of_var == "date", date_var, names_of_var)
        names_of_var <- ifelse(names_of_var == "int_var", var, names_of_var)
        names(dat_anom) <- names_of_var
        
        dat_anom$variable <- var
        
        final_dat <- bind_rows(final_dat, dat_anom)
    }
    return(dat_anom)
}

dat_anom <- get_anomoly_data(cycles, c("day_strain", "day_cal"), "day_start")

get_anomaly_message <- function(dat_anom, vars, date_var) {
    
    message <- tibble()
    
    for(var in vars) {
        
        yest_dat <- dat_anom %>% 
            filter(get(date_var) == Sys.Date() - days(1) & 
                       variable == var)
        
        alert_tdt_avg <- if (yest_dat$alert_tdt_avg[1] == TRUE) {
            if (yest_dat$three_day_trend_avg[1] > 0) {
                paste0("You have had 3 days in a row OVER the daily average ",
                       "(previous 10 weeks for that day of the week - i.e.: this ",
                       "previous Monday was compared to the 10 week average only ",
                       "for Mondays) for ", var, ".")
            } else {
                paste0("You have had 3 days in a row UNDER the daily average ",
                       "(previous 10 weeks for that day of the week - i.e.: this ",
                       "previous Monday was compared to the 10 week average only ",
                       "for Mondays) for ", var, ".")
            }
        } else {
            NA
        }
        
        alert_tdt <- if (yest_dat$alert_tdt[1] == TRUE) {
            if (yest_dat$three_day_trend[1] > 0) {
                paste0("Each day over the past 3 days, you've INCREASED your amount", 
                       " of ", var, " (three days ago was less than two days ago ",
                       "which was less than yesterday).")
            } else {
                paste0("Each day over the past 3 days, you've DECREASED your amount", 
                       " of ", var, " (three days ago was more than two days ago ",
                       "which was more than yesterday).")
            }
        } else {
            NA
        }
        
        alert_tdt_30 <- if (yest_dat$alert_tdt_30[1] == TRUE) {
            if (yest_dat$three_day_trend_30[1] > 0) {
                paste0("You have had 3 days in a row OVER the 30 day moving ",
                       "average (average of previous 30 days) for ", var, ".")
            } else {
                paste0("You have had 3 days in a row UNDER the 30 day moving ",
                       "average (average of previous 30 days) for ", var, ".")
            }
        } else {
            NA
        }
        
        alert_diff_10_week <- if (yest_dat$alert_diff_10_week[1] == TRUE) {
            if (yest_dat$percent_dif_10_week[1] > 0) {
                paste0("You have EXCEEDED the 75th percentile for ", var, ".")
            } else {
                paste0("You were UNDER the 25th percentile for ", var, ".")
            }
        } else {
            NA
        }
        
        var_message <- tibble(
            variable = var,
            alert = c("alert_tdt_avg", 
                      "alert_tdt", 
                      "alert_tdt_30", 
                      "alert_diff_10_week"),
            messages = c(alert_tdt_avg, 
                         alert_tdt, 
                         alert_tdt_30, 
                         alert_diff_10_week)
        ) %>% 
            filter(!is.na(messages))
        
        message <- bind_rows(message, var_message)
    }
    return(message)
}

get_anomaly_message(dat_anom, c("day_strain", "day_cal"), "day_start")

dat_anom %>% 
    select(day_start,
           day_strain,
           avg_strain,
           percent_dif_10_week,
           npdtw,
           lq_pdtw,
           uq_pdtw,
           alert_diff_10_week)

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
