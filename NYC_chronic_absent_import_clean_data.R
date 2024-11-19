# This code loads (from .csv), merges, and cleans data from the city of New York 
# open data on attendance and demographic characteristics for K-5 schools from 
# schools years beginning in  2013 to 2022. It is cleaned to focus on Chronic
# Absenteeism, but also includes measures on poverty, disability, ELL, race/ethnicity, 
# and gender. 
# NOTE: demographic variables (poverty, disability, etc.) are available
# at the school level ONLY. So, the percentage of Hispanic students will be the 
# same for any school in any given year, regardless of grade. Open NYC data 
# does not have demographic data disaggregated by grade. Only the chronic absenteeism
# rates are available by grade level within schools. 
# Also NOTE: chronic absenteeism data is available from 2013-2022. demographic data 
# is available from 2013-2021 (one year less). 

### Setup ####

# clear the working environment
rm(list = ls())

#run required packages 
library(data.table)
library(tidyverse)
library(broom)
# do I need these?
library(pastecs)
library(foreign)
library(labelled)
library(haven)

### Load Data ####
  # Load data in from .csv files
  # these are also available as API data from OpenNYC but the data sometimes changes structure...
## ATTENDANCE DATA 
attendance_13_18 <- read.csv("school_attendance_2013-2018.csv")
  # The latter data was released in an Excel workbook with the demographic categories in different sheets
  # I saved them as individual .csv files and will glue them back together so it mirrors the former data
attendance_18_22_all <- read.csv("school_attendance_2018-2023_all_students.csv")
attendance_18_22_SWD <- read.csv("school_attendance_2018-2023_SWD.csv")
attendance_18_22_ethnicity <- read.csv("school_attendance_2018-2023_ethnicity.csv")
attendance_18_22_gender <- read.csv("school_attendance_2018-2023_gender.csv")
attendance_18_22_poverty <- read.csv("school_attendance_2018-2023_poverty.csv")
attendance_18_22_ELL <- read.csv("school_attendance_2018-2023_ELL.csv")
attendance_18_22_STH <- read.csv("school_attendance_2018-2023_STH.csv")

  # list of data frames: add a column for demographic category and bind together
data_frames <- list(attendance_18_22_all, attendance_18_22_SWD,attendance_18_22_ethnicity,
                    attendance_18_22_gender, attendance_18_22_poverty, attendance_18_22_ELL, 
                    attendance_18_22_STH)
  # names of demographic categories
names <- list("all_students", "SWD", "ethnicity", "gender", "poverty", "ELL", "STH")
  # function to add the demographic category
add_category_column <- function(df, category) {
    df %>%
      mutate(demographic_category = category)
  }
  # apply the function to each data frame in the list
modified_dfs <- modified_dfs <- map2(data_frames, names, add_category_column)
  # bind everything together
attendance_18_21 <- bind_rows(modified_dfs)

## DEMOGRAPHIC DATA 
demographics_13_17 <- read.csv("demographic_snapshot_school_2013-2017.csv")
# update this: new data that goes until 2024
demographics_17_21 <- read.csv("demographic_snapshot_school_2017-2021.csv")
summary(attendance_13_18)
summary(attendance_18_22)
summary(demographics_13_17) # the variable names are sometimes different and whether they are presented as proportions or percents
summary(demographics_17_21)

table(attendance_13_18$Demographic.Category)

