library(plyr)
library(tidyverse)
library(skimr)
library(ggplot2)
library(janitor)
library(lubridate)
library(gridExtra)

# AGGREGATE Activity data

# Read all activity data
files <- list.files(path = "processed_datasets", pattern = "*activity.csv",
                    full.names = T)
activity <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = 'file_name')

# Read all heartrate data
files <- list.files(path = "processed_datasets", pattern = "*heartrate.csv",
                    full.names = T)
heartrate <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = 'file_name')

# Read all sleep data
files <- list.files(path = "processed_datasets", pattern = "*sleep.csv",
                    full.names = T)
sleep <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = 'file_name')

# Read all body data
files <- list.files(path = "processed_datasets", pattern = "*body.csv",
                    full.names = T)
body <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = 'file_name')

# Assign much simpler numeric IDs to the persons under analysis
mobius_activity <- activity %>%
  filter(file_name == 'processed_datasets/mobius_activity.csv')
mobius_ids <- unique(mobius_activity$id)

dfs = list(activity, heartrate, sleep, body)
for (i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
  mutate(id = plyr::mapvalues(id, from = mobius_ids, to = c(7:39),
                        warn_missing = FALSE)) %>%
  mutate(id = case_when(id == 2891001357 ~ 40,
                        id == 6391747486 ~ 41,
                        TRUE ~ id) ) %>%
  mutate(id = as.character(id)) %>%
  select(-c(file_name)) %>%
  arrange(id,date)
}

activity <- dfs[[1]]
heartrate <- dfs[[2]]
sleep <- dfs[[3]]
body <-dfs[[4]]


# EDA of activity data
activity %>% 
  group_by(id) %>%
  summarize( num_of_records = n()) %>% 
  ggplot()+ 
  geom_col(mapping = aes(x=id, y = num_of_records, fill =id))+
  labs(title = 'How much daily data we have per consumer?', x = 'Consumer ID',
       y = 'Number of records')+
  geom_text(aes(x=id, y = num_of_records,label=num_of_records))
  
# Looks like we have high skewness in terms of the availability of records
# per consumer. On one hand, we have a height of around 4 years of data for 
# Consumer No. 4,
# where as we have as low as 8 & 9 records for Consumers 40 & 41. Nevertheless,
# we should continue for the exciting analysis ahead.

# Let's summarize average daily activity of each person
avg_daily_activity <- activity %>% 
  group_by(id) %>% 
  summarise(avg_steps = mean(steps),
            avg_cal = mean(calories),
            avg_dist = mean(distance), 
            avg_sedentary_min = mean(sedentary_minutes),
            avg_fair_active_min = mean(fairly_active_minutes),
            avg_very_active_min = mean(very_active_minutes),
            avg_light_active_min = mean(lightly_active_minutes),
            avg_fair_active_dist = mean(moderately_active_distance),
            avg_very_active_dist = mean(very_active_distance),
            avg_light_active_dist = mean(light_active_distance))


avg_cal_plot <- ggplot(avg_daily_activity)+
  geom_histogram(mapping = aes(x=avg_cal, y=..density..),
                 binwidth = 200, colour="black", fill="white")+
  geom_density(aes(x= avg_cal),alpha=.2, fill="red")+
  labs(x = 'Average daily calories burnt', y = 'Density',
       title = 'Average Daily Calories burnt by Consumers')+
  geom_vline(aes(xintercept=mean(avg_cal)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_cal), y=0.001),
            label="Distribution mean")
               
avg_steps_plot <- ggplot(avg_daily_activity)+
  geom_histogram(mapping = aes(x=avg_steps, y=..density..),
                 binwidth = 1000, colour="black", fill="white")+
  geom_density(aes(x= avg_steps),alpha=.2, fill="red")+
  labs(x = 'Average daily steps', y = 'Density',
       title = 'Average Daily Steps by Consumers')+
  geom_vline(aes(xintercept=mean(avg_steps)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_steps), y=0.00025),
            label="Distribution mean")

avg_dist_plot <- ggplot(avg_daily_activity %>% drop_na())+
  geom_histogram(mapping = aes(x=avg_dist, y=..density..),
                 binwidth = 1, colour="black", fill="white")+
  geom_density(aes(x= avg_dist),alpha=.2, fill="red")+
  labs(x = 'Average distance (km)', y = 'Density',
       title = 'Average Daily Distance by Consumers')+
  geom_vline(aes(xintercept=mean(avg_dist)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_dist), y=0.25),
            label="Distribution mean")

grid.arrange(avg_cal_plot, avg_steps_plot, avg_dist_plot, nrow =3)

# Insight:
# On an average, a consumer of smart-device walks/runs a distance of 
# 5 kilometers by taking around 7000 steps thereby burning around 
# 2.1 kilo-calories per day.

# Relationship between total active time and calories burnt.
activity %>% 
  mutate(total_active_time = fairly_active_minutes + lightly_active_minutes +
           very_active_minutes) %>% 
  filter(total_active_time != 0) %>% 
  # Remove the outlier
  filter(!near(total_active_time,625, 10)) %>% 
  ggplot(aes(x = total_active_time, y = calories))+
  geom_point() + geom_smooth() +
  labs(title = 'Calories burnt vs total active-time',
       x = 'Total active time (minutes)',
       y = 'Calories burnt')

# Insight: Calories burnt is directly proportional to the total active time of 
# the consumer


# Relation between steps and calories burnt.
activity %>% 
  ggplot(aes(x = steps, y = calories))+
  geom_point()+
  labs(title = 'Calories burnt vs Step count',
       x = 'Number of steps', y = 'Calories burnt')
# 
# Observation: 
#   There appears 2 segment of users: 
#   1. average_burners : who burn normal calories per step taken
#   2. intense_burners: who burn aggressively per step taken.
# 
# Recomendation:
#   Bellabeat products may give user customized recommendation to the consumer 
# based on what type of burner he/she is.

activity %>% 
  mutate(calorie_per_step = calories/steps) %>% 
  filter(calorie_per_step<0.5) %>% 
  ggplot(aes(x=calorie_per_step, y=..density..))+
  geom_histogram(binwidth = 0.05, colour="black", fill="white")+
  geom_density(alpha=.2, fill="red")+
  labs(x = 'Calorie burnt per step', y = 'Density',
       title = 'Distribution of calorie per step')+
  geom_vline(aes(xintercept=mean(calorie_per_step)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(calorie_per_step), y= 6,
            label=paste("Mean =",round(mean(calorie_per_step),3))))+
  geom_text(aes(x = 2*mean(calorie_per_step), y = 7.5), label = 'Intense burners')+
  geom_text(aes(x = 0.5* mean(calorie_per_step), y= 7.5), label = 'Average burners')

# Since the distribution is bimodal, consumers can be segmented using the mean 
# as shown. Average burners burn less than 0.127 calorie/step, while Intense 
# burners burn greater than 0.127 calorie/step.

# Let's plot the calories vs steps trends for both segments
activity %>% 
  mutate(calorie_per_step = calories/steps) %>% 
  mutate(burner_type = case_when(
    calorie_per_step <= 0.127 ~ 'Average',
    TRUE ~ 'Intense'
  )) %>%
  ggplot(aes(x = steps, y = calories, color = burner_type))+
  geom_point()+
  geom_smooth()


# WHAT IS THE CALORIES VS STEPS TREND for each body type

# As per CDC, a person can be categorized into 3 categories according to BMI. 
body_types <- body %>% 
  group_by(id) %>% 
  summarize(body_mass_index = round(mean(bmi))) %>% 
  drop_na() %>% 
  mutate(body_type = case_when(
    body_mass_index < 25 ~ 'Healthy',
    body_mass_index %in% c(25,30) ~ 'Overweight',
    TRUE ~ 'Obese'
  ))

activity %>% 
  merge(body_types, by = 'id') %>% 
  ggplot(aes(x = steps, y = calories, color = body_type))+
  geom_point() + geom_smooth()

# # Insight: 
# Overweight & Obese consumers burnt more aggresively as compared to healthy 
# individuals. 
# Overweight & obese are intense burners, while healthy ones are average burners. 


# STEPS vs Distance 
activity %>% 
  ggplot(aes(x = steps, y = distance))+
  geom_point() + geom_smooth()+
  labs(title = 'Distance traversed vs Steps taken by consumers')

# there is linear relation between steps and distance traversed by consumers.

# Let's look at how much consumers put the device to daily usage.
use_time_plot <- avg_daily_activity %>% 
  group_by(id) %>% 
  summarize(total_device_time = avg_sedentary_min + avg_light_active_min+
              avg_fair_active_min + avg_very_active_min) %>% 
  drop_na() %>%  #drop consumers with no time data
  ggplot() + 
  geom_histogram(mapping = aes(x=total_device_time, y=..density..),
                   binwidth = 60, colour="black", fill="white")+
  geom_density(aes(x= total_device_time),alpha=.2, fill="red")+
  geom_vline(aes(xintercept=mean(total_device_time)),
               color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(total_device_time),y = 0.003),
            label="Distribution Mean")+
  labs(title = 'Distribution of average device usage minutes by consumers',
       x = 'Average daily device usage (minutes)')

use_time_plot

# The histogram depicts the bimodal distribution of device usage, where either the
# device is used around 1400 minutes frequently or around 1000 minutes.
# On an average, a consumer uses device for around 1200 minutes, i.e., roughly 20 
# hours.


# Insight:
# 1. On an average, a consumer of smart-device uses smart-device for 20 hours, which
# may lead to inefficient overall health tracking.
# 2. A good proportion of consumers use the smart-device for around 1000 minutes
# (16.67hrs) only.
# This may be due to the frequent charging requirements of device.
# Or, the consumers simply don't put it on during some hours of the day (esp. sleep).
# 
# Recommendation:
# 1. The Bellabeat devices should have high capacity batteries requiring less
# frequent charging.
# 2. The device should generate alerts/beeps to remind user to wear it,
# in case it senses no-use for some time. This reminder facility will attract new
# customers.

# Now, we need to see how is the usage-time distributed across different intensities
# of work.
avg_sedentary_min_plot <- ggplot(avg_daily_activity %>% drop_na())+
  geom_histogram(mapping = aes(x=avg_sedentary_min, y=..density..),
                 binwidth = 60, colour="black", fill="white")+
  geom_density(aes(x= avg_sedentary_min),alpha=.2, fill="red")+
  labs(x = 'Average Sedentary Minutes', y = 'Density',
       title = 'Average Daily sedentary minutes of consumers')+
  geom_vline(aes(xintercept=mean(avg_sedentary_min)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_sedentary_min), y= 0.003),
            label="Distribution mean")

avg_light_active_min_plot <- ggplot(avg_daily_activity %>% drop_na())+
  geom_histogram(mapping = aes(x=avg_light_active_min, y=..density..),
                 binwidth = 60, colour="black", fill="white")+
  geom_density(aes(x= avg_light_active_min),alpha=.2, fill="red")+
  labs(x = 'Average Light-Active Minutes', y = 'Density',
       title = 'Distribution of Average Lightly Active minutes')+
  geom_vline(aes(xintercept=mean(avg_light_active_min)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_light_active_min), y= 0.006),
            label="Distribution mean")

avg_fair_active_min_plot <- ggplot(avg_daily_activity %>% drop_na())+
  geom_histogram(mapping = aes(x=avg_fair_active_min, y=..density..),
                 binwidth = 20, colour="black", fill="white")+
  geom_density(aes(x= avg_fair_active_min),alpha=.2, fill="red")+
  labs(x = 'Average Fairly Active Minutes', y = 'Density',
       title = 'Distribution of Average Fairly-Active minutes')+
  geom_vline(aes(xintercept=mean(avg_fair_active_min)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_fair_active_min), y= 0.03),
            label="Distribution mean")

avg_very_active_min_plot <- ggplot(avg_daily_activity %>% drop_na())+
  geom_histogram(mapping = aes(x=avg_very_active_min, y=..density..),
                 binwidth = 20, colour="black", fill="white")+
  geom_density(aes(x= avg_very_active_min),alpha=.2, fill="red")+
  labs(x = 'Average Very Active Minutes', y = 'Density',
       title = 'Distribution of Average Very-Active minutes')+
  geom_vline(aes(xintercept=mean(avg_very_active_min)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_very_active_min), y= 0.03),
            label="Distribution mean")

grid.arrange(avg_sedentary_min_plot, avg_light_active_min_plot,
             avg_fair_active_min_plot, avg_very_active_min_plot, nrow =2)

avg_daily_activity %>% 
  drop_na() %>% 
  mutate(total_device_time = avg_sedentary_min + avg_light_active_min+
           avg_fair_active_min + avg_very_active_min) %>% 
  summarize(mean_very_active_time = round(mean(avg_very_active_min)),
            mean_light_active_time = round(mean(avg_light_active_min)),
            mean_fairly_active_time = round(mean(avg_fair_active_min)),
            mean_sedentary_time = round(mean(avg_sedentary_min)),
            mean_total_device_time = round(mean(total_device_time))) %>% 
  grid.table()

# Insight:
# On an average, a smart-device consumer spends 1004 minutes (16.7 hours) in 
# sedentary activities, 182 minutes (3 hours) in light, 15 minutes in moderate, 
# while 19 minutes in intense activities.

# Recommendation:
#   Since, excessively sedentary lifestyle leads to health problems such as 
# diabetes and irregular bloodpressure. The Bellabeat device should suggest user
# a walk/light exercise after every detection of continuous 3 hours of sedentary
# time, except sleep. This suggestion feature can be used during ad campaign, 
# depicting the device as remedy to sedentary lifestyle.


# Trend of sedentary-time by weekday
daywise_sed_time_plot <- activity %>% 
  mutate(weekday = weekdays(date)) %>% 
  drop_na() %>% 
  group_by(id, weekday) %>% 
  summarize(avg_sed_time = mean(sedentary_minutes)) %>% 
  group_by(weekday) %>% 
  summarize(daywise_sed_time = mean(avg_sed_time)) %>% 
  ggplot(aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                            'Wednesday','Thursday',
                                            'Friday','Saturday',
                                            'Sunday')), 
             y = daywise_sed_time, group =1))+
  geom_line()+
  geom_point()+labs(title = 'Average Daily Sedentary minutes by Weekday',
                   x = 'Weekday', y = 'Average Sedentary time (min)')+
  geom_text(aes(label = round(daywise_sed_time,1), vjust = -0.5))

daywise_sed_time_plot

# Insight: On an avearge, consumers spend least time seated on Saturdays, 
# while they remain seated longest on Mondays. 
# Recommendation: 
# 1. Bellabeat device may remind periodically for short-walk on Mondays to reduce
# overall sedentary minutes.
# 2. The marketing campaigns can be customized for weedays accordingly.

# Looking at the mean daily calories
ggplot(avg_daily_activity) + geom_col(mapping = aes(x = id, y = avg_cal, fill =id))+
                      labs(title = "How many calories each consumer burns per day
                           on an average?",
                           x = "Consumer ID", y = "Mean Daily Calories")
# It looks consumers: 2, 4, 5, 6 burnt too less than 300 calories on an average day

# What's the exact individual trends of calories burnt by least calorie burning 
# consumers?
activity %>% 
  filter(id ==2| id ==4 | id==6 | id==5 ) %>% 
  ggplot() + geom_line(mapping = aes(x=date, y = calories))+
  facet_wrap(~id, scales = "free")
# From the above line plot, it is evident that calories burnt is zero for many
# days. We will see the exact proportion of zero calories in entire analyses 
# period

activity %>% 
  filter(id == 2 | id ==4 | id ==6 | id==5) %>%
  group_by(id) %>% 
  summarize(total_days = n(), zero_cal_days = sum(calories== 0)) %>% 
  mutate(percent_of_zero_cal = round(zero_cal_days*100/total_days,2)) %>% 
  ggplot() + 
  geom_col( mapping = aes(x= id, y = percent_of_zero_cal, fill = id)) +
  labs(title = 'How frequent are zero-calories readings?',
       subtitle = "For users having least average daily calories",
       x = 'Consumer ID',
       y = 'Proportion of records with zero-calories')+
  geom_text(aes(x= id, y = percent_of_zero_cal, 
                label = paste(percent_of_zero_cal,'%')))

# Conclusion from the bar chart: Consumer no. 5 has declined average of calories
# burnt dominantly due to missing the track of his/her activities

# ARE THERE ANY OTHER CONSUMERS WITH SUCH CASE
activity %>% 
  group_by(id) %>% 
  summarize(total_days = n(), zero_cal_days = sum(calories== 0)) %>% 
  mutate(percent_of_zero_cal = round(zero_cal_days*100/total_days,2)) %>% 
  ggplot() + 
  geom_col( mapping = aes(x= id, y = percent_of_zero_cal, fill = id)) +
  labs(title = 'How frequent are zero-calories readings?',
       subtitle = "For all users",
       x = 'Consumer ID',
       y = 'Proportion of records with zero-calories')+
  geom_text(aes(x= id, y = percent_of_zero_cal, 
                label = paste(percent_of_zero_cal,'%'), angle = 45))

# From the chart, we found 12 consumers with no-calories days

# A user may not to track his/her activity upto 35% times. 
# This may be due to variety of reasons: vacations, the watch malfunctioned,
# or they simply forgot to put the tracker on.

# ARE THERE ANY PARTICULAR DAYS WHEN THE CALORIES AREN'T TRACKED

# First, add the weekday column into our aggregated activity data
activity<-activity %>% mutate(weekday = weekdays(date))

activity %>%
  group_by(id,weekday) %>% 
  summarize(percent_of_zero_cal = 100*sum(calories==0)/n(),)  %>% 
  group_by(weekday) %>% 
  summarize(avg_percent = mean(percent_of_zero_cal))%>% 
  ggplot()+
  geom_col(mapping = aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                                        'Wednesday','Thursday',
                                                        'Friday','Saturday',
                                                        'Sunday')),
                         y = avg_percent, fill = weekday))+
  labs(title = "'Calories not tracked' vs Weekday",
       subtitle = 'Is the zero-calorie reading dependent upon the weekday?',
       x = 'Weekday', y = 'Percentage of zero-calorie occurences')+
  geom_text(aes(x= weekday, y = avg_percent, 
                label = paste(round(avg_percent,2),'%')))+
  theme(legend.position = "none")

# No there is no particularly fixed day(s) when calories go untracked.
# observation: Calories are most likely to be untracked on Tuesday & Saturday. 
# However, the average precentage of occurences is too low to conclude any
# relation between zero-calorie reading and the weekday. 

# Let's check the same for steps data
activity %>%
  group_by(id,weekday) %>% 
  summarize(percent_of_zero_steps = 100*sum(steps==0)/n(),)  %>% 
  group_by(weekday) %>% 
  summarize(avg_percent = mean(percent_of_zero_steps))%>% 
  ggplot()+
  geom_col(mapping = aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                                        'Wednesday','Thursday',
                                                        'Friday','Saturday',
                                                        'Sunday')),
                         y = avg_percent, fill = weekday))+
  labs(title = "'Steps not tracked' vs Weekday",
       subtitle = 'Is the zero-step reading dependent upon the weekday?',
       x = 'Weekday', y = 'Percentage of zero-steps occurences')+
  geom_text(aes(x= weekday, y = avg_percent, 
                label = paste(round(avg_percent,2),'%')))+
  theme(legend.position = "none")

# observation: There is no such particular day at which steps are not tracked 
# in particular. However, steps are tracked most regular on Wednesday. 

# Insight : 
# The consumers are likely to not track their steps 8 to 15% percent of times,
# when they simply don't wear the tracker or don't switch on the 
# steps tracking.
# Recommendations:
# 1. The Bellabeat device should have a reminder facility to switch on the steps
# tracking on the device. Such functionality will be an added facility to users
# who often forget to turn on steps tracker.
# 2. The company may organize weekly steps challenge, so as the users remain
# motivated and aware to track their steps.

# HOW IS THE USER-ACTIVITY AFFECTED BY THE WEEKDAY
activity_by_weekday <- activity %>% 
  group_by(id,weekday) %>% 
  summarize(avg_light_active_min = mean(lightly_active_minutes),
            avg_fair_active_min = mean(fairly_active_minutes),
            avg_very_active_min = mean(very_active_minutes),
            avg_steps = mean(steps)) %>% 
  drop_na() %>% 
  group_by(weekday) %>% 
  summarize(light_min = mean(avg_light_active_min),
            moderate_min = mean(avg_fair_active_min),
            intense_min = mean(avg_very_active_min),
            avg_steps_by_day = mean(avg_steps))

steps_vs_day_plot <- ggplot(activity_by_weekday,
       aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                          'Wednesday','Thursday',
                                           'Friday','Saturday',
                                           'Sunday')), 
           y= avg_steps_by_day, group =1))+
    geom_line() + geom_point() + labs(title = 'Average daily steps by Weekday',
                            x = 'Weekday', y = 'Average daily steps')+
  geom_text(aes(label = round(avg_steps_by_day,0), vjust = -.5))

light_activity_plot <- ggplot(activity_by_weekday,
                          aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                                                 'Wednesday','Thursday',
                                                                 'Friday','Saturday',
                                                                 'Sunday')), 
                                  y= light_min, group =1))+
  geom_line()+geom_point()+labs(title = 'Average Light-Active minutes by Weekday',
                     x = 'Weekday', y = 'Average Light-Active time (min)')+
  geom_text(aes(label = round(light_min,1)))

moderate_activity_plot <- ggplot(activity_by_weekday,
                                aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                                                   'Wednesday','Thursday',
                                                                   'Friday','Saturday',
                                                                   'Sunday')), 
                                    y= moderate_min, group =1))+
  geom_line()+geom_point()+labs(title = 'Average Fairly-Active Minutes by Weekday',
                     x = 'Weekday', y = 'Average Fairly-Active time (min)')+
  geom_text(aes(label = round(moderate_min,1)))

intense_activity_plot <- ggplot(activity_by_weekday,
                              aes(x = factor(weekday, levels = c('Monday','Tuesday',
                                                                 'Wednesday','Thursday',
                                                                 'Friday','Saturday',
                                                                 'Sunday')), 
                                  y= intense_min, group =1))+
  geom_line()+geom_point()+labs(title = 'Average Very-Active Minutes by Weekday',
                     x = 'Weekday', y = 'Average Very-Active time (min)')+
  geom_text(aes(label = round(intense_min,1)))

grid.arrange(steps_vs_day_plot, light_activity_plot, moderate_activity_plot,
             intense_activity_plot,nrow = 2)

# INSIGHT: On an average, consumers have very active Saturdays and the least
# active Sundays.
# Recommendation: Bellabeat can have marketing campaigns according to the
# weekdays on which user are least/most active.



############################################

# EDA of sleep data
sleep_summary <- sleep %>% 
  merge(data.frame(id = c(1:41)), all = TRUE) %>% 
  group_by(id) %>% 
  summarize(n_sleep_records = n_unique(date))
activity_summary <- activity %>% 
  group_by(id) %>% 
  summarize(n_activity_records = n_unique(date))
sleep_data_availability <- merge(sleep_summary,activity_summary,all = TRUE) %>% 
  group_by(id) %>% 
  summarize(sleep_record_percent = n_sleep_records*100/n_activity_records)

ggplot(sleep_data_availability) +
  geom_histogram(mapping = aes(x= sleep_record_percent, y=..density..),
                 binwidth = 10, colour="black", fill="white")+
  geom_density(aes(x= sleep_record_percent),alpha=.2, fill="red")+
  labs(x = 'Availability of daily sleep records (%)', y = 'Density',
       title = 'Distribution of Sleep Records')+
  geom_vline(aes(xintercept=mean(sleep_record_percent)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(sleep_record_percent), y= 0.03),
            label="Distribution mean")
# 
# Insight:
# Most consumers recorded their sleep only once in 10 days or they never recorded.
# On an average, consumers recorded sleep only 1/3rd of times they recorded their
# daily activity
# 
# Recommendation:
#   Bellabeat device can have a functionality to remind user in evening to switch 
# on sleep
# tracking. The reminder facility will help users to keep better track of their
# sleep cycles.

################################################

# What is the distribution of sleep time?

sleep %>% 
  group_by(id) %>% 
  summarize(avg_sleep_time = mean(sleep_time)) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x= avg_sleep_time, y=..density..),
                 binwidth = 60, colour="black", fill="white")+
  geom_density(aes(x= avg_sleep_time),alpha=.2, fill="red")+
  labs(x = 'Sleep time (minutes)', y = 'Density',
       title = 'Distribution of sleep time amongst consumers')+
  geom_vline(aes(xintercept=mean(avg_sleep_time)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_sleep_time), y= 0.005),
            label="Distribution mean")
# INSIGHT:
#   On an average, a consumer sleeps for 350 minutes daily, i.e., roughly six hours
# But, CDC recommends more than 7 hours of sleep per night. 

# What is the proportion of users having less than 7 hours of sleep?

sleep %>% 
  group_by(id) %>% 
  summarize(avg_sleep_time = mean(sleep_time)) %>% 
  summarize(n_less_sleep = sum(avg_sleep_time < 420)/n_unique(id),
            n_adequate_sleep = sum(avg_sleep_time >= 420)/n_unique(id))
# Ans: 60 percent of the consumers with recorded sleep data had average daily sleep
# than recommended 7 hours.

# Recommendation:
#   The device should suggest user to sleep more after tracking weekly sleep data
# for better sleep patterns of the consumer. The ads should emphasise
# the need of sufficient sleep for good health and the device will help them achieve
# their daily sleep goals.


# from the histogram, it is observed that few  consumers have daily average sleep 
# less than 240 minutes ~ 4 hours. Let's look at their individual sleep trends 
# to find the underlying reason.

sleep %>% 
  group_by(id) %>% 
  summarize(avg_sleep_time = mean(sleep_time)) %>% 
  filter(avg_sleep_time < 240) %>% 
  merge(sleep, all.x = TRUE) %>% 
  ggplot(aes(x = date, y = sleep_time))+
  geom_line(mapping = aes(x = date, y = sleep_time))+ 
  geom_point()+
  facet_wrap(~id, scales = "free")

# Insight: some users don't record their sleep time on most days. 
# Recommendation: Bellabeat device can have an prompt in evening hours to start 
# recording sleep, to remind him/her to record sleep
# Company can start rewards program for punctual monthly-sleep tracking users to 
# encourage recording of sleep.

# Is number of calories burnt related to sleep-time?
sleep %>% 
  merge(activity, all = TRUE) %>% 
  select(calories, sleep_time) %>% 
  drop_na() %>% 
  ggplot(aes(x = sleep_time, y = calories))+
  geom_point() + geom_smooth()
# There appears no relation between amount of sleep and calories burnt.

sleep %>% 
  select(id, date, sleep_time) %>%
  mutate(weekday = weekdays(date)) %>% 
  drop_na() %>% 
  group_by(id, weekday) %>% 
  summarize(avg_sleep_time = mean(sleep_time)) %>% 
  View()

all_weekdays_ids <- (sleep %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(id) %>% 
  summarize(n_unique_weekdays = n_unique(weekday)) %>% 
  filter(n_unique_weekdays == 7) %>% 
  select(id))[['id']]

# HOW SLEEP-DURATION VARIES WITH WEEKDAYS
sleep %>% 
  select(id, date, sleep_time) %>%
  mutate(weekday = weekdays(date)) %>% 
  drop_na() %>% 
  group_by(id, weekday) %>% 
  summarize(avg_sleep_time = mean(sleep_time)) %>% 
  # delete sleep records for consumers with any weekday avg sleep time missing
  filter(id %in% all_weekdays_ids) %>% 
  group_by(weekday) %>% 
  summarize(daywise_sleep = mean(avg_sleep_time)) %>%
  ggplot()+
  geom_col(mapping = aes(factor(weekday, levels = c('Monday','Tuesday',
                                                    'Wednesday','Thursday',
                                                    'Friday','Saturday',
                                                    'Sunday')),
                          y = daywise_sleep, fill = weekday))+
  theme(legend.position = "none")+
  geom_text(aes(y = daywise_sleep, x = weekday, label = round(daywise_sleep,0)),
            vjust = -0.5)+
  labs(title = 'How sleep-duration varies with the day of week',
       subtitle = 'Average daily sleep-time vs weekday',
       x = 'Weekday', y = 'Average sleep-time (minutes)')

# Consumers sleep longest on Saturday & Sundays, which is obvious due to the 
# greater amount of time leisure available on weekend.


# DISTRIBUTION OF WAKE_TIME
sleep %>% 
  filter(sleep_time != 0) %>% 
  select(id,wake_time, bed_time, sleep_time) %>% 
  drop_na() %>% 
  group_by(id) %>% 
  summarize(avg_wake_time = mean(wake_time), avg_bed_time = mean(bed_time)) %>% 
  ggplot() +
  geom_histogram(aes(x= avg_wake_time, y=..density..),binwidth = 10, 
                 colour="black", fill="white")+
  geom_density(aes(x= avg_wake_time), alpha=.2, fill="red")+
  labs(x = 'Average wake-time of consumer (minutes)', y = 'Density',
       title = 'Distribution of Wake_time')+
  geom_vline(aes(xintercept=mean(avg_wake_time)),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(avg_wake_time), y= 0.03),
            label="Distribution mean") 

# On an average, a consumer spends 40 minutes being on bed but not sleeping.

sleep %>% 
  filter(sleep_time != 0) %>% 
  select(id,wake_time, bed_time, sleep_time) %>% 
  drop_na() %>% 
  group_by(id) %>% 
  summarize(avg_wake_time = mean(wake_time), avg_bed_time = mean(bed_time)) %>% 
  mutate(wake_time_percent = avg_wake_time*100/avg_bed_time) %>% 
  ggplot() +
  geom_histogram(aes(x= wake_time_percent, y=..density..),binwidth = 10, 
                 colour="black", fill="white")+
  geom_density(aes(x= wake_time_percent), alpha=.2, fill="red")+
  labs(x = 'Proportion of bed-time wasted (%)', y = 'Density',
       title = 'Distribution of Percentage of bed-time wasted')+
  geom_vline(aes(xintercept=mean(wake_time_percent)),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(wake_time_percent), y= 0.12),
            label="Distribution mean")

# A consumer wastes 8% of his/her bed time on an average while awake.

# Recommendation:
# 1. The company can emphasise on meditation practices to relax for
# better sleep of consumers.
# 2. The Bellabeat device may send reminder to cut-off from digital displays around
# 2 hours prior to usual bed-time

# Let's look at the quality of sleep achieved by consumers

sleep %>% 
  select(id,deep_sleep_time, sleep_time, shallow_sleep_time) %>% 
  drop_na() %>% 
  mutate(n = n_unique(id)) %>% View()

# We have only 3 consumers' data on deep vs shallow sleep times.
# The deep sleep is important (25% avg) for a user's body and mind.
# Tracking by Bellabeat device will help user detect the quality of their sleep


### EDA of heartrate data
hourly_heartrate <- heartrate %>% 
  mutate(hour = format(strptime(time, "%H:%M:%S"), "%H")) %>% 
  group_by(id, date, hour) %>% 
  summarize(hourly_heart_rate = mean(heart_rate))

daily_heartrate <- heartrate %>% 
  group_by(id, date) %>% 
  summarize(avg_heartrate = mean(heart_rate),
            min_heartrate = min(heart_rate),
            max_heartrate = max(heart_rate),
            dev_heartrate = sd(heart_rate))

hourly_hr_availability <- hourly_heartrate %>% 
  group_by(id, date) %>% 
  summarize(hours_count = n())

daily_hr_availability <- daily_heartrate %>% 
  group_by(id) %>% 
  summarize(hr_days_count = n())
  
daily_activity_availability <- activity %>% 
  group_by(id) %>% 
  summarize(activity_days_count = n())

# HOW MUCH DAILY HEARTRATE RECORDS ARE AVAILABLE?
hr_vs_activity_availability <- 
  merge(daily_hr_availability, daily_activity_availability, all = TRUE) %>% 
  mutate(hr_days_count = coalesce(hr_days_count, 0)) %>% 
  mutate(hr_by_activity_percent = hr_days_count*100/activity_days_count) %>% 
  ggplot(aes(x = hr_by_activity_percent, y= ..density..)) +
  geom_histogram(binwidth = 10, 
                 colour="black", fill="white")+
  geom_density( alpha=.2, fill="red")+
  labs(x = 'Availability of heartrate data (%)', y = 'Density',
       title = 'How much daily heartrate data is available?',
       subtitle = 'Distribution of percentage of heartrate data available')+
  geom_vline(aes(xintercept=mean(hr_by_activity_percent)),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 10)+
  geom_text(aes(x=mean(hr_by_activity_percent), y= 0.05),
            label="Distribution mean")

hr_vs_activity_availability

# Insight: Most of the consumers, never record their daily heartrate.
#   On an average, a consumer records daily heartrate one-fourth times as compared
# to recording the daily activity.

# ARE THE AVAILABLE HR RECORDS COMPLETE?
hourly_hr_availability %>% 
  group_by(id) %>% 
  summarize(avg_hours_count = mean(hours_count)) %>% 
  ggplot(aes(x = avg_hours_count, y= ..density..)) +
  geom_histogram(binwidth = 4, 
                 colour="black", fill="white")+
  geom_density( alpha=.2, fill="red")+
  labs(x = 'Hours of heartrate available', y = 'Density',
       title = 'Is available daily heartrate data complete?',
       subtitle = 'Distribution of hours of heartrate available')+
  geom_vline(aes(xintercept=mean(avg_hours_count)),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 12)+
  geom_text(aes(x=mean(avg_hours_count), y= 0.05,
            label=paste("Mean =", round(mean(avg_hours_count)))))

# Insight: On an average, 16 hours of heartbeat is recorded by a consumer, when 
# he/she records their heartrate.

# Recommendation:
#   Bellabeat may emphasise the need of pulse-rate monitoring for better control
# during workouts in it ad campaign. Especially, the obese-persons can be targetted 
# whose workout regime should have heartrate as important factor for efficient 
# fat burning.

# Is average daily heartrate related to daily calories burnt?

daily_heartrate %>% 
  merge(activity, all = TRUE) %>% 
  select(id, avg_heartrate, min_heartrate, max_heartrate, dev_heartrate,
         calories) %>% 
  drop_na() %>% 
  ggplot(aes(x = calories, y = dev_heartrate))+
  geom_point() + geom_smooth()

# The higher standard deviation in heartrate is associated with higher amount
# of calories burnt.
# This may be due to the higher pulserate variations in consumers induldged in
# very intense activities, thereby burning more calories.



