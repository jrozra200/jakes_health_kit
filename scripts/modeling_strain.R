library(dplyr)
library(readr)
library(ggplot2)

workouts <- read_csv('../data/workouts.csv')

wo <- workouts %>% 
    filter(!is.na(name)) %>% 
    mutate(name = case_when(
        (name == "Powerlifting" | 
             name == "Functional Fitness") ~ "Weightlifting",
        1 == 1 ~ name
    )) %>% 
    group_by(name) %>% 
    slice_head(n = 6) %>% 
    summarise(count = length(name),
              avg_strain = mean(intensity_score),
              avg_intensity = mean(raw_intensity_score))


ggplot(workouts[workouts$intensity_score > 4, ], 
       aes(x = (raw_intensity_score), y = (intensity_score))) +
    geom_point() 

ggplot(workouts[workouts$intensity_score > 4, ], 
       aes(x = log(raw_intensity_score))) + 
    geom_histogram()

ggplot(workouts[workouts$intensity_score > 4, ], 
       aes(x = log(intensity_score))) + 
    geom_histogram()


lin_mod <- lm(log(avg_strain) ~ log(avg_intensity), wo)
summary(lin_mod)



