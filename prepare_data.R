library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)

# Mobius Fitbit dataset
# Daily Activity data for 33 persons
mobius_activity_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\dailyActivity_merged.csv")
View(mobius_activity_data)
# Heart rate data for only 14 persons
mobius_HR_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\heartrate_seconds_merged.csv")
View(mobius_HR_data)
# MET data for 33 persons
mobius_MET_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\minuteMETsNarrow_merged.csv")
View(mobius_MET_data)
# Sleep data of 24 persons
mobius_sleep_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\sleepDay_merged.csv")
View(mobius_sleep_data)
# Weight log # for 8 persons
mobius_weight_log <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_fitness_tracker_data\\weightLogInfo_merged.csv")
View(mobius_weight_log)

# Read CSV of Fitness Trends Dataset
arooj_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitness_trends_dataset\\25.csv")
skim_without_charts(arooj_data)
View(arooj_data)


# Read FitBitChargeHR data
alket_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Fitbit_charge_hr_data\\One_Year_of_FitBitChargeHR_Data.csv")
skim_without_charts(alket_data)
View(alket_data)

# Read Mi-band dataset
damir_body_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Body.csv")
View(damir_body_data)
damir_sleep_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Sleep.csv")
View(damir_sleep_data)
damir_steps_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Mi_band_4_years_data\\Steps.csv")
View(damir_steps_data)

# Read Bekbolat dataset
bekbolat_activity_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\ACTIVITY\\ACTIVITY_1566153601293.csv")
View(bekbolat_activity_data)
bekbolat_HR_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1566153602880.csv")
View(bekbolat_HR_data)
bekbolat_sleep_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\Xiaomi_mi_tracker_data\\SLEEP\\SLEEP_1566153602529.csv")
View(bekbolat_sleep_data)

# Read Akash Fitbit dataset
akash_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\fitbit_akash\\FitBit data.csv")
View(akash_data)

# Read Yasser's cardio activity dataset (7 years)
yasser_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\yasser_cardio_data\\cardioActivities.csv")
View(yasser_data)

# Read Parul Garg dataset
parul_activity_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\ACTIVITY\\ACTIVITY_1599810432505.csv")
View(parul_activity_data)
parul_HR_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1599810433761.csv")
View(parul_HR_data)
parul_sleep_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\parul_fitbit_data\\SLEEP\\SLEEP_1599810433552.csv")
View(parul_sleep_data)

# Read Josh dataset
josh_data <- read_csv("C:\\Users\\shubh\\Documents\\GDAC\\Course 8\\josh_fitbit_data\\time_Values.csv")
View(josh_data)
