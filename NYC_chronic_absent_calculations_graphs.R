# Graphs of Chronic Absenteeism Rates Over Time Disaggregated by Demogrpahics

# has not been added to github yet

### Setup ####

# clear the working environment
rm(list = ls())

#run required packages 
library(data.table)
library(tidyverse)
library(broom)
library(lubridate)

### Import clean data #### 
attendance_13_22_k5 <- read.csv("clean_attendance_13_22_k5.csv")

### Arrange data and calculate aggregrate means ####

test_pct <- attendance_13_22_k5 %>% 
  group_by(DBN, School.Name, Year, Demographic.Category, Demographic.Variable) %>% 
  # chronic absent percent and total per school, per year, per category and variable
  summarize(chronic_absent_pct = round(mean(`X..Chronically.Absent.1`, na.rm = T),1), 
            chronic_absent_n = sum(`X..Chronically.Absent`, na.rm=T), 
            .groups = 'drop') 
# need to add demographics so we can know total number of students

# graph some things first

# new data frame with chronic absenteeism means aggregated by category, variable, year
df_absent <- aggregate(chronic_absent_pct ~ Demographic.Category + Demographic.Variable
                       + Year , test_pct , mean)

## MAKE THESE CHANGES AND RE-SAVE ## 
df_absent <- df_absent %>% 
  mutate(Year = ymd(paste0(substr(Year, 1, 4), "-01-01")), # change to data format, taking first year
         chronic_absent_pct = round(chronic_absent_pct,2)) # round chronic absenteeism rate

saveRDS(df_absent, "df_absent.rds")

### Graphs #### 

df_absent %>% 
  filter(Demographic.Category == "Ethnicity") %>% 
  mutate(Year = ymd(paste0(substr(Year, 1, 4), "-01-01"))) %>% # change to data format, taking first year
  ggplot(aes(x =Year, y=chronic_absent_pct, color=Demographic.Variable)) +
  geom_line() +
  geom_point() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    limits = c(0, 60),       # Start y-axis at 0
    expand = c(0, 0)         # Prevent padding
  ) +
  labs(
    title = "Chronic Absenteeism Over Time by Ethnicity",
    x = "Year",
    y = "Chronic Absenteeism (%)",
    color = "Ethnicity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )