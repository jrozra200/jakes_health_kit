# Anomaly Detection Functions

get_tracked_measures <- function() {
    return(
        tibble(
            dat = c(rep("cycles", 4),
                    rep("recovery", 4),
                    rep("sleep", 9)),
            var = c("day_strain",
                    "day_cal",
                    "day_avg_heart_rate",
                    "day_max_heart_rate",
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
            date_var = "date"
        )
    )
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
                            NA)
    
    alert_tdt <- ifelse(dat_anom$alert_tdt == TRUE,
                        ifelse(dat_anom$three_day_trend > 0,
                               paste0("Each day over the past 3 days, you've INCREASED your amount", 
                                      " of ", var, " (three days ago was less than two days ago ",
                                      "which was less than yesterday)."),
                               paste0("Each day over the past 3 days, you've DECREASED your amount", 
                                      " of ", var, " (three days ago was more than two days ago ",
                                      "which was more than yesterday).")),
                        NA)
    
    alert_tdt_30 <- ifelse(dat_anom$alert_tdt_30 == TRUE,
                           ifelse(dat_anom$three_day_trend_30 > 0,
                                  paste0("You have had 3 days in a row OVER the 30 day moving ",
                                         "average (average of previous 30 days) for ", var, "."),
                                  paste0("You have had 3 days in a row UNDER the 30 day moving ",
                                         "average (average of previous 30 days) for ", var, ".")),
                           NA)
    
    alert_diff_10_week <- ifelse(dat_anom$alert_diff_10_week == TRUE,
                                 ifelse(dat_anom$percent_dif_10_week > 0,
                                        paste0("You have EXCEEDED the 95th percentile for ", var, "."),
                                        paste0("You were UNDER the 5th percentile for ", var, ".")),
                                 NA)
    
    messages <- case_when(
        !is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                                "\n\t- ",
                                                alert_tdt, 
                                                "\n\t- ",
                                                alert_tdt_30, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt, 
                                                "\n\t- ",
                                                alert_tdt_30, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        !is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                                "\n\t- ",
                                                alert_tdt_30, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        !is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                                "\n\t- ",
                                                alert_tdt, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        !is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                               "\n\t- ",
                                               alert_tdt, 
                                               "\n\t- ",
                                               alert_tdt_30),
        is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt_30, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt, 
                                               "\n\t- ",
                                               alert_tdt_30),
        !is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                                "\n\t- ",
                                                alert_diff_10_week),
        !is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                               "\n\t- ",
                                               alert_tdt_30),
        !is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg, 
                                               "\n\t- ",
                                               alert_tdt),
        !is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt_avg),
        is.na(alert_tdt_avg) & 
            !is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt),
        is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            !is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ paste0(alert_tdt_30),
        is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            !is.na(alert_diff_10_week) ~ paste0(alert_diff_10_week),
        is.na(alert_tdt_avg) & 
            is.na(alert_tdt) &
            is.na(alert_tdt_30) &
            is.na(alert_diff_10_week) ~ ""
    )
    
    
    
    dat_anom$message <- messages
    
    return(dat_anom)
}
