library(tidyverse)
library(skimr)
library(ggplot2)
library(janitor)
library(imputeMissings)
library(lubridate)

alket_id <- 1
arooj_id <- 2
josh_id <- 3
damir_id <- 4
parul_id <- 5
bekbolat_id <- 6

# Clean Mobius datasets
mobius_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\dailyActivity_merged.csv")
mobius_daily_activity <- mobius_daily_activity %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(activity_date = as.Date(activity_date,"%m/%d/%Y")) %>% 
  rename(date = activity_date, steps = total_steps, distance = total_distance) %>% 
  select(-c(tracker_distance, logged_activities_distance)) %>% 
  arrange(id,date) #sort by id, then by activity_date

mobius_heartrate <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\heartrate_seconds_merged.csv")
mobius_heartrate <- mobius_heartrate %>% 
  clean_names() %>% 
  distinct() %>%  
  mutate(time = parse_date_time(time, "%m/%d/%Y %I:%M:%S %p")) %>% 
  separate( col = time, into = c("date", "time"), sep = " ") %>% 
  rename(heart_rate = value) %>% 
  mutate(time = substr(time,1,5)) %>% 
  group_by(id, date, time) %>% 
  summarize(heart_rate = mean(heart_rate)) %>% 
  arrange(id, date, time) 

mobius_met <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\minuteMETsNarrow_merged.csv")
mobius_met <- mobius_met %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(activity_minute = parse_date_time(activity_minute, "%m/%d/%Y %I:%M:%S %p")) %>% 
  separate( col = activity_minute, into = c("date", "time"), sep = " ") %>% 
  arrange(id, date, time) %>% 
  rename(mets = me_ts)

mobius_sleep <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\sleepDay_merged.csv")
mobius_sleep <- mobius_sleep %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(sleep_day = as.Date(sleep_day,"%m/%d/%Y")) %>% 
  rename(date = sleep_day, sleep_time = total_minutes_asleep, bed_time = total_time_in_bed) %>% 
  select(-total_sleep_records) %>% 
  mutate(wake_time =bed_time - sleep_time) %>% 
  arrange(id,date)

mobius_body <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\weightLogInfo_merged.csv")
mobius_body <- mobius_body %>% 
  clean_names() %>% 
  distinct() %>% 
  rename(weight = weight_kg) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         height = sqrt(weight/bmi)* 100) %>% 
  arrange(id,date) %>% 
  select(-c(log_id, weight_pounds)) # remove irrelevant columns

# Clean Akash Fitbit dataset
akash_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\fitbit_akash\\FitBit data.csv")
akash_daily_activity <- akash_daily_activity%>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(activity_date = as.Date(activity_date,"%m/%d/%Y")) %>% 
  rename(date = activity_date, steps = total_steps, distance = total_distance) %>% 
  select(-c(tracker_distance, logged_activities_distance)) %>% 
  arrange(id,date) #sort by id, then by activity_date

# Alket's FitBit ChargeHR
alket_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_charge_hr_data\\One_Year_of_FitBitChargeHR_Data.csv")
alket_daily_activity <- alket_daily_activity %>% 
  distinct() %>% # remove 1 duplicate row
  clean_names() %>% 
  mutate(date = as.Date(date, "%d-%m-%Y")) %>% 
  mutate(calories = calories * 1000) %>%  # convert kilocalories into calories
  mutate(calories_activity = case_when(
    calories_activity < 5 ~ calories_activity*1000,
    calories_activity >=5 ~ calories_activity) ) %>% 
  mutate(steps = case_when(
    steps < 100 ~ steps*1000,
    steps >= 100 ~ steps)) %>% 
  mutate(minutes_sitting = case_when(
    minutes_sitting < 10 ~ minutes_sitting * 1000,
    minutes_sitting >= 10 ~ minutes_sitting  )) %>% 
  filter(date != '2016-01-24' ) %>% 
  mutate(distance = as.numeric(gsub(",", ".", distance)) ) %>% 
  mutate(id = rep(alket_id,length(date))) %>% 
  rename(sedentary_minutes = minutes_sitting, 
         lightly_active_minutes = minutes_of_slow_activity,
         fairly_active_minutes = minutes_of_moderate_activity,
         very_active_minutes = minutes_of_intense_activity) %>% 
  select(-c(floors,calories_activity)) %>% 
  select(c(9,1,2,3,4,5,6,7,8)) %>% 
  arrange(date)
  
# Clean Arooj Samsung Health data
arooj_health <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitness_trends_dataset\\25.csv")
arooj_health <- arooj_health %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  select(-c(mood,bool_of_active)) %>%  # drop irrelevant columns
  rename(steps = step_count, calories = calories_burned,
         sleep_time = hours_of_sleep, weight = weight_kg) %>% 
  mutate(id = rep(arooj_id,length(date)), sleep_time = sleep_time * 60) %>% # insert artificial ID to recognize the person
  arrange(id, date) %>% 
  select(c(6,1,2,3,4,5)) # make the id column the first column
arooj_daily_activity <- arooj_health %>% select( c(id, date, steps, calories))
arooj_sleep <- arooj_health %>% select( c(id, date, sleep_time))
arooj_body <- arooj_health %>% select( c(id, date, weight)) 

# Clean Josh's Fitbit data
josh_fitbit <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\josh_fitbit_data\\time_Values.csv")
# Josh's activity data
josh_daily_activity <- josh_fitbit %>% 
  clean_names() %>% 
  distinct() %>% 
  separate( col = date_time, into = c("date", "time"), sep = " ") %>% 
  mutate(id = rep(josh_id, length(date))) %>% 
  select(id, date, time, steps, calories ) %>% 
  group_by(date) %>% 
  summarize(id = josh_id, steps = sum(steps), calories = sum(calories)) %>% 
  arrange(date)
# Josh's heart-rate data
josh_heartrate <- josh_fitbit %>% 
  clean_names() %>% 
  distinct() %>% 
  separate( col = date_time, into = c("date", "time"), sep = " ") %>% 
  mutate(time = substr(time,1,5)) %>% 
  rename(heart_rate = hr) %>% 
  select(date, time, heart_rate) %>% 
  drop_na() %>% 
  mutate(id = rep(josh_id, length(date))) %>% 
  select(c(4,1,2,3)) %>% 
  arrange(date,time)

# Clean Damir's Mi band data
damir_body <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Body.csv")
# delete first row with zero readings
damir_body <- damir_body %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(substr((as.POSIXct(timestamp, origin = "1970-01-01")),1,10)),
         id = damir_id)%>% 
  select(c(id, date, height, weight, bmi)) %>% 
  arrange(date)

damir_sleep <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Sleep.csv")
damir_sleep <- damir_sleep %>% 
  clean_names() %>% 
  distinct() %>% 
  filter(last_sync_time!=0) %>% # remove days with zero lastSycnTime 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sleep_time = deep_sleep_time + shallow_sleep_time,
         bed_time = wake_time + deep_sleep_time + shallow_sleep_time) %>% 
  mutate(id = rep(damir_id, length(date))) %>% 
  select(c(id, date, deep_sleep_time, shallow_sleep_time, wake_time, bed_time, 
           sleep_time)) %>% # delete irrelevant columns
  arrange(date)

damir_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Steps.csv")
damir_daily_activity <- damir_daily_activity %>% 
  clean_names() %>% 
  distinct() %>% 
  filter(last_sync_time!=0) %>% # remove days with zero lastSycnTime 
  mutate(date = as.Date(date, "%Y-%m-%d"), distance = distance/1000,
         very_active_distance = run_distance/1000) %>% 
  select(-c(last_sync_time, run_distance)) %>% 
  mutate(id = rep(damir_id, length(date))) %>% 
  select(c(6,1,2,3,4,5)) %>% 
  arrange(date)

# Clean Parul's data
parul_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\ACTIVITY\\ACTIVITY_1599810432505.csv")
parul_daily_activity <- parul_daily_activity %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         distance = distance/1000,
         very_active_distance = run_distance/1000) %>% 
  select(-c(last_sync_time, run_distance)) %>% 
  mutate(id = rep(parul_id, length(date))) %>% 
  select(c(6,1,2,3,4,5)) %>% 
  arrange(date) # sort by date

parul_heartrate <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1599810433761.csv")
parul_heartrate <- parul_heartrate %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  mutate(time = substr(time,1,5)) %>% 
  mutate(id = rep(parul_id, length(date))) %>% 
  select(c(4,1,2,3)) %>% 
  arrange(date,time)
  
parul_sleep <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\SLEEP\\SLEEP_1599810433552.csv")
parul_sleep <- parul_sleep %>% 
  distinct() %>% 
  clean_names() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sleep_time = deep_sleep_time + shallow_sleep_time,
         bed_time = deep_sleep_time + shallow_sleep_time + wake_time) %>%
  mutate(id = rep(parul_id, length(date))) %>% 
  select(c(id, date, deep_sleep_time, shallow_sleep_time, 
           sleep_time, wake_time, bed_time)) %>% 
  arrange(date)

# Clean Bekbolat's Xiaomi Mi dataset
bekbolat_daily_activity <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\ACTIVITY\\ACTIVITY_1566153601293.csv")
bekbolat_daily_activity <- bekbolat_daily_activity %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"), 
         distance = distance/1000, very_active_distance = run_distance/1000,
         id = rep(bekbolat_id, length(date)) )%>%  
  select(c(id, date,distance, very_active_distance, steps, calories)) %>% 
  arrange(date)

bekbolat_heartrate <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1566153602880.csv")
bekbolat_heartrate <- bekbolat_heartrate %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
                        id = rep(bekbolat_id, length(date)) )%>%
  mutate(time = substr(time,1,5)) %>% 
  select(c(id, date, time, heart_rate)) %>% 
  arrange(date,time)
  
bekbolat_sleep <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\SLEEP\\SLEEP_1566153602529.csv")
bekbolat_sleep <- bekbolat_sleep %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sleep_time = deep_sleep_time + shallow_sleep_time,
         bed_time = deep_sleep_time + shallow_sleep_time + wake_time,
         id = rep(bekbolat_id, length(date))) %>% 
  select(c(id, date, deep_sleep_time, shallow_sleep_time,
           sleep_time, bed_time, wake_time)) %>%
  arrange(date)

# Clean Yasser's cardio activity dataset 
yasser_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\yasser_cardio_data\\cardioActivities.csv")
yasser_daily_activity <- yasser_data %>% 
  clean_names() %>% 
  distinct() %>% 
  separate( col = date, into = c("date", "time"), sep = " ") %>% 
  mutate(id = rep(6, length(date)), 
    duration = 60*as.numeric(substr(duration,1,2)) + as.numeric(substr(duration,4,5))) %>% 
  filter(calories_burned <= 2588) %>% 
  mutate(very_active_distance = case_when(
    type == "Running" ~ distance_km,
    type == "Cycling" ~ distance_km,
    type == "Other"   ~ distance_km,
    type == "Walking" ~ 0),
    distance = distance_km,
    very_active_minutes = case_when(
      type == "Running" ~ duration,
      type == "Cycling" ~ duration,
      type == "Other" ~ duration,
      type == "Walking" ~ 0),
    light_active_distance = case_when(
      type == "Walking" ~ distance_km,
      TRUE ~ 0),
    light_active_minutes = case_when(
      type == "Walking" ~ duration,
      TRUE ~ 0),
    moderately_active_distance = 0,
    moderately_active_minutes = 0) %>% 
  group_by(date) %>% 
  summarize(id =6, distance = sum(distance), calories = sum(calories_burned),
            very_active_distance = sum(very_active_distance),
            moderately_active_distance = sum(moderately_active_distance),
            light_active_distance = sum(light_active_distance),
            very_active_minutes = sum(very_active_minutes),
            moderately_active_minutes = sum(moderately_active_minutes),
            light_active_minutes = sum(light_active_minutes))%>% 
  arrange(date)

yasser_heartrate <- yasser_data %>% 
  clean_names() %>% 
  distinct() %>%
  separate( col = date, into = c("date", "time"), sep = " ") %>% 
  select(date, time, average_heart_rate_bpm) %>%
  mutate(id = rep(6, length(date))) %>% 
  drop_na()  %>% 
  group_by(date) %>% 
  summarize(id=6, heart_rate = mean(average_heart_rate_bpm)) 



# Save the processed dataframes into CSV files
dfs <- list(mobius_activity = mobius_daily_activity,
            mobius_heartrate = mobius_heartrate,
            mobius_sleep = mobius_sleep,
            mobius_body = mobius_body,
            arooj_activity = arooj_daily_activity,
            arooj_sleep = arooj_sleep,
            arooj_body = arooj_body,
            akash_activity = akash_daily_activity,
            alket_activity = alket_daily_activity,
            josh_activity = josh_daily_activity,
            josh_heartrate = josh_heartrate,
            damir_activity = damir_daily_activity,
            damir_sleep = damir_sleep,
            damir_body = damir_body,
            parul_activity = parul_daily_activity,
            parul_heartrate = parul_heartrate,
            parul_sleep = parul_sleep,
            bekbolat_activity = bekbolat_daily_activity,
            bekbolat_heartrate = bekbolat_heartrate,
            bekbolat_sleep = bekbolat_sleep)
            # remove yasser data, as it is only recorded during cardio
            # yasser_activity = yasser_daily_activity,
            # yasser_heartrate = yasser_heartrate)
mapply(write_csv, dfs, file=paste0('processed_datasets\\',names(dfs), '.csv'))


