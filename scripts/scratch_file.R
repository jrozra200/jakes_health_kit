cycles <- read_csv('../data/cycles.csv')
sleeps <- read_csv('../data/sleeps.csv')
recovery <- read_csv('../data/recovery.csv')
workouts <- read_csv('../data/workouts.csv')

# Convert raw strain to strain: exp((0.398850 * log(raw)) + 4.509219)

wo <- workouts %>% 
    filter(!is.na(name)) %>% 
    mutate(name = case_when(
        (name == "Powerlifting" | 
             name == "Functional Fitness") ~ "Weightlifting",
        1 == 1 ~ name
    )) %>% 
    group_by(name) %>% 
    slice_head(n = 10) %>% 
    summarise(count = length(name),
              avg_strain = mean(intensity_score),
              avg_intensity = mean(raw_intensity_score))


day_start <- unlist(
    str_split(cycles$during, ",")
)[grepl("\\[", unlist(str_split(cycles$during, ",")))]
day_start <- str_remove_all(day_start, "\\[|\\'")
day_start <- as_date(day_start, format = "%Y-%m-%dT%H:%M:%OSZ")

cycles <- cycles %>% 
    mutate(day_start = day_start,
           dotw = weekdays(day_start))

cycles_sum <- cycles %>% 
    group_by(dotw) %>% 
    slice_head(n = 10) %>% 
    summarise(avg_strain = mean(day_strain),
              avg_score = mean(scaled_strain),
              avg_kj = mean(day_kilojoules))
