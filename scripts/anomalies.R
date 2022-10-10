# Monitor Metrics on the following: 



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


