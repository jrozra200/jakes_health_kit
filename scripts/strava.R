library(rStrava)
library(httr)
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)

options(scipen = 999)

# strava <- read.csv("strava.config")
# 
# stoken <- config(token = strava_oauth(app_name = strava$name[1],
#                                      app_client_id = strava$client_id[1], 
#                                      app_secret = strava$client_secret[1], 
#                                      cache = TRUE, 
#                                      app_scope = "activity:read_all"))


stoken <- config(token = readRDS(".httr-oauth")[[2]])

me <- get_athlete(stoken)
activities <- get_activity_list(id = NULL, 
                                stoken = stoken, 
                                before = NULL,
                                after = NULL)

act_df <- do.call(rbind.data.frame, activities)
act_df <- data.frame(matrix(unlist(activities), nrow = length(activities), byrow = TRUE))


act_df <- tibble(list = activities) %>% 
    unnest_longer(list) %>% 
    hoist(list, "resource_state", "name", "distance")

lapply(activities, `[[`, 2) %>% 
    data.frame %>% 
    rownames_to_column("key") %>% 
    gather(x, value, -key) %>% 
    select(-x)

act_df <- tibble(list = activities) %>% 
    hoist(list, 
          name = "name", 
          distance = "distance",
          total_elevation_gain = "total_elevation_gain",
          moving_time = "moving_time",
          elapsed_time = "elapsed_time",
          sport_type = "sport_type",
          id = "id",
          start_date = "start_date",
          start_date_local = "start_date_local",
          timezone = "timezone",
          utc_offset = "utc_offset",
          location_city = "location_city",
          location_state = "location_state",
          location_country = "location_country",
          trainer = "trainer",
          average_speed = "average_speed", 
          max_speed = "max_speed",
          has_heartrate = "has_heartrate",
          average_heartrate = "average_heartrate",
          max_heartrate = "max_heartrate") %>% 
    select(-list) %>% 
    mutate(moving_time_min = moving_time / 60,
           elapsed_time_min = elapsed_time / 60,
           distance_miles = distance / 1609.344,
           elevation_miles = total_elevation_gain / 1609.344)

runs <- act_df %>% 
    filter(sport_type == "Run")




