## HK PLOT FUNCTIONS

# UNIVERSAL VARIABLES
LABEL_SIZE <- 2.75
LEGEND_SIZE <- 4
TITLE_SIZE <- 8
AXIS_SIZE <- 5
LIT_LABEL_SIZE <- 1.75
MAIN_HEX <- "#00A3E1"

# CREATE LINE PLOTS
line_plot <- function(data, 
                      yvar, 
                      date_var, 
                      title, 
                      format_style = NULL,
                      super_little_label_size = NULL) {
    # Calculate the ranges for the plot
    range_data <- data %>% 
        head(10) %>%
        summarise(minimum = min(get(yvar)),
                  maximum = max(get(yvar)),
                  average = mean(get(yvar))) %>% 
        mutate(ll = minimum - ((maximum - minimum) / 6),
               ul = maximum + ((maximum - minimum) / 6),
               label_pos = minimum - ((maximum - minimum) / 12),
               label_bump = (maximum - minimum) * 0.05)
    
    # Get the 30 day mean
    mean_var <- data %>% 
        slice(2:31) %>% 
        pull(get(yvar)) %>% 
        mean()
    
    # Get the first date
    min_date <- data %>% 
        head(10) %>% 
        pull(get(date_var)) %>% 
        min()
    
    # Get the last date
    max_date <- data %>% 
        head(10) %>% 
        pull(get(date_var)) %>% 
        max()
    
    point_labels <- data %>% 
        head(10) %>% 
        mutate(labels_perc = percent(get(yvar), accuracy = 0.01),
               labels_com = comma(get(yvar), accuracy = 0.01))
    
    if(is.null(format_style)) {
        if(mean_var < 1) {
            point_labels <- point_labels %>% 
                pull(labels_perc)
        } else {
            point_labels <- point_labels %>% 
                pull(labels_com)
        } 
    } else {
        if (format_style == "percent") {
            point_labels <- point_labels %>% 
                pull(labels_perc)
        } else {
            point_labels <- point_labels %>% 
                pull(labels_com)
        }
    }
    
    if (!is.null(super_little_label_size)) {
        LIT_LABEL_SIZE <- LIT_LABEL_SIZE - 0.5
        AXIS_SIZE <- AXIS_SIZE - 0.5
        TITLE_SIZE <- TITLE_SIZE - 2
    }
    
    plot <- ggplot(data[1:10, ], aes(x = date, y = get(yvar))) + 
        # Add a line for the yvar
        geom_line(color = MAIN_HEX) + 
        # Add a label for the yvar
        geom_text(aes(label = point_labels, 
                      y = get(yvar) + range_data$label_bump[1]),
                  size = LIT_LABEL_SIZE,
                  check_overlap = TRUE) + 
        # Add a horizontal line for the mean yvar (30 days)
        geom_hline(yintercept = mean_var) + 
        # Add a label for the 30 day average 
        geom_text(label = paste0("30 day avg. ", title, ": ", 
                                 ifelse(is.null(format_style), 
                                        ifelse(mean_var < 1, 
                                               percent(mean_var, accuracy = 0.01),
                                               comma(mean_var)),
                                        ifelse(format_style == "percent",
                                               percent(mean_var, accuracy = 0.01),
                                               comma(mean_var)))), 
                  y = mean_var + range_data$label_bump[1],
                  x = min_date - days(1),
                  hjust = 0,
                  size = LIT_LABEL_SIZE) +
        # Add the day of the week to the bottom
        geom_text(aes(label = dotw, y = range_data$label_pos[1]),
                  size = LIT_LABEL_SIZE) +
        # Make sure the plot is scaled properly (y-axis)
        scale_y_continuous(limits = c(range_data$ll[1], range_data$ul[1]),
                           labels = ifelse(mean_var < 1,
                                           percent_format(),
                                           comma_format())) + 
        # Add a day to the front and back of the plot (x-axis)
        scale_x_date(limits = c(min_date - days(1), 
                                max_date + days(1))) + 
        # Add the title
        ggtitle(title) + 
        # Format this shit to look good
        theme(panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major.y = element_line(color = "light gray"),
              panel.grid.major.x = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = AXIS_SIZE),
              title = element_text(size = TITLE_SIZE))
    
    return(plot)
}

plot_todays_strain <- function(plot_data) {
    ggplot(plot_data, aes(x = day_start)) +
        geom_bar(aes(y = additional_strain), 
                 stat = "identity", 
                 fill = MAIN_HEX, 
                 alpha = 0.25, width = 0.8) + 
        geom_bar(aes(y = day_strain), stat = "identity", fill = MAIN_HEX) + 
        geom_errorbar(aes(ymin = avg_strain, ymax = avg_strain)) +
        geom_text(aes(label = paste0("Currently ", 
                                     percent(1 - ((avg_strain - day_strain) / 
                                                      avg_strain), 
                                             accuracy = 0.01), 
                                     " of the average strain."),
                      y = ifelse(avg_strain > additional_strain, 
                                 avg_strain / 2, 
                                 additional_strain / 2)),
                  size = 1.5) +
        geom_text(aes(label = paste0("Current strain: ", 
                                     round(day_strain, 2)), 
                      y = day_strain - 0.05),
                  hjust = 1, 
                  nudge_x = 0.25,
                  size = 1.5) +
        geom_text(aes(label = paste0("Average strain for ", dotw, "s: ", 
                                     round(avg_strain, 2)),
                      y = avg_strain - 0.05),
                  hjust = 1,
                  nudge_x = -0.25,
                  size = 1.5) +
        geom_text(aes(label = paste0("Expected additional strain: ", 
                                     round(additional_strain - day_strain, 2)), 
                      y = additional_strain - 0.05),
                  hjust = 1, 
                  nudge_x = 0.35,
                  size = 1.5) +
        coord_flip() +
        theme(panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(size = 5),
              panel.grid.major.x = element_line(color = "light gray"),
              panel.grid.major.y = element_blank(), 
              plot.background = element_rect(color = "black", fill = NA))
}


strain_bar <- function(dat, var, avg_var, title) {
    max_data <- dat %>% 
        pull(get(var)) %>% 
        max()
    label_bump <- max_data * 0.025
    
    plot <- ggplot(dat, aes(x = day_start)) +
        # Add a bar for the variable of concern
        geom_bar(aes(y = get(var)), 
                 stat = "identity",
                 fill = MAIN_HEX) + 
        # Add a label for the variable's output
        geom_text(aes(label = comma(get(var), accuracy = 0.01), 
                      y = get(var) + label_bump),
                  size = LIT_LABEL_SIZE) + 
        # Add a line for the average for this day
        geom_errorbar(aes(ymin = get(avg_var), ymax = get(avg_var))) + 
        # Add a label for the average
        geom_text(aes(label = comma(get(avg_var), accuracy = 0.01), 
                      y = get(avg_var) + label_bump),
                  size = LIT_LABEL_SIZE) + 
        # Add the day of the week on the bottom
        geom_text(aes(label = dotw, y = label_bump),
                  size = LIT_LABEL_SIZE) + 
        # Add a label for the percentage of average I've done
        geom_text(aes(label = percent((get(var) - get(avg_var)) / get(avg_var), 
                                      accuracy = 0.01), 
                      y = get(var) - label_bump),
                  size = LIT_LABEL_SIZE) +
        # Add a title
        ggtitle(paste0(title, " vs. Average")) + 
        # Add commas to the labels
        scale_y_continuous(labels = comma_format()) + 
        # Theme the shit
        theme(panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(size = AXIS_SIZE),
              title = element_text(size = TITLE_SIZE),
              panel.grid.major.y = element_line(color = "light gray"),
              panel.grid.major.x = element_blank())
    
    return(plot)
}

sleep_area_plot <- function(sleep) {
    plot <- sleep %>% 
        head(10) %>% 
        select(date,
               dotw,
               wake_duration,
               light_sleep_duration,
               slow_wave_sleep_duration,
               rem_sleep_duration,
               credit_from_naps) %>% 
        rename(Awake = wake_duration,
               Light = light_sleep_duration,
               SWS = slow_wave_sleep_duration,
               REM = rem_sleep_duration,
               Naps = credit_from_naps) %>% 
        pivot_longer(!c(date, dotw), names_to = "category", values_to = "hours") %>% 
        ggplot(aes(x = date, y = hours, fill = category, group = category)) +
        geom_area(stat = "identity", 
                  position = "fill", 
                  alpha = 0.75,
                  color = MAIN_HEX) + 
        scale_fill_brewer(palette = "Blues") + 
        scale_y_continuous(labels = percent_format()) +
        geom_text(aes(label = dotw, y = 0.05),
                  size = LIT_LABEL_SIZE) +
        theme(panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(size = AXIS_SIZE),
              legend.text = element_text(size = LEGEND_SIZE),
              panel.grid.major.y = element_line(color = "light gray"),
              panel.grid.major.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "top")
    
    return(plot)
}

plot_trendz <- function(plot_dat, cols = 1) {
    plot <- ggplot(plot_dat, 
           aes(x = date, 
               y = values, 
               color = length, 
               group = length, 
               alpha = alpha)) +
        geom_line() + 
        facet_wrap(~ measure, ncol = cols) + 
        scale_alpha(guide = "none") + 
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major.y = element_line(color = "light gray"),
              panel.grid.major.x = element_blank(),
              axis.text.y = element_blank(),
              strip.background = element_blank())
    
    return(plot)
}

workout_facet_plot <- function(dat) {
    max_data <- dat %>% 
        pull(raw_intensity_score) %>% 
        max()
    label_bump <- max_data * 0.015
    
    plot <- ggplot(dat, aes(x = date)) +
        # Add a bar for the variable of concern
        geom_bar(aes(y = raw_intensity_score, fill = name), 
                 stat = "identity") + 
        # Add a label for the variable's output
        geom_text(aes(label = comma(raw_intensity_score, accuracy = 0.01), 
                      y = label_y + label_bump),
                  size = LIT_LABEL_SIZE) +
        # Add the day of the week on the bottom
        geom_text(aes(label = dotw, y = label_bump),
                  size = LIT_LABEL_SIZE) + 
        # Add a label for the percentage of average I've done
        geom_text(aes(label = percent((raw_intensity_score / avg_strain) - 1, 
                                      accuracy = 0.01), 
                      y = label_y - label_bump),
                  size = LIT_LABEL_SIZE) +
        # Add commas to the labels
        scale_y_continuous(labels = comma_format()) + 
        # Add better colors 
        scale_fill_brewer(palette = "Blues") + 
        # Theme the shit
        theme(panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(size = AXIS_SIZE),
              title = element_blank(),
              legend.position = "top",
              panel.grid.major.y = element_line(color = "light gray"),
              panel.grid.major.x = element_blank())
    
    return(plot)
}
