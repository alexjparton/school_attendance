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
attendance_18_22_all <- read.csv("school_attendance_2018-2022_all_students.csv")
attendance_18_22_SWD <- read.csv("school_attendance_2018-2022_SWD.csv")
attendance_18_22_ethnicity <- read.csv("school_attendance_2018-2022_ethnicity.csv")
attendance_18_22_gender <- read.csv("school_attendance_2018-2022_gender.csv")
attendance_18_22_poverty <- read.csv("school_attendance_2018-2022_poverty.csv")
attendance_18_22_ELL <- read.csv("school_attendance_2018-2022_ELL.csv")
attendance_18_22_STH <- read.csv("school_attendance_2018-2022_STH.csv")

  # list of data frames: add a column for demographic category and bind together
data_frames <- list(attendance_18_22_all, attendance_18_22_SWD,attendance_18_22_ethnicity,
                    attendance_18_22_gender, attendance_18_22_poverty, attendance_18_22_ELL, 
                    attendance_18_22_STH)
  # names of demographic categories
cat_names <- list("all_students", "SWD", "ethnicity", "gender", "poverty", "ELL", "STH")
  # function to add the demographic category
add_category_column <- function(df, category) {
    df %>%
      mutate(demographic_category = category)
  }
  # apply the function to each data frame in the list
modified_dfs <- modified_dfs <- map2(data_frames, cat_names, add_category_column)
  # bind everything together
attendance_18_22 <- bind_rows(modified_dfs)

  # For merging these df's, we need to match up all the variables 
table(attendance_18_22$Category) # this newer data includes a few more entries, e.g. "Neither male nor female"
table(attendance_13_18$Demographic.Variable) # I will match to this so we have data for all years

  # clean up (remove)
rm(list=c("attendance_18_22_all", "attendance_18_22_ELL", "attendance_18_22_ethnicity", "attendance_18_22_ethnicity", 
          "attendance_18_22_gender", "attendance_18_22_gender", "attendance_18_22_poverty", "attendance_18_22_STH", 
          "attendance_18_22_SWD", "add_category_column", "cat_names", "data_frames", 
          "modified_dfs"))

  # remove extra categories from newer data
attendance_18_22_trimmed <- attendance_18_22 %>% 
  filter(!Category %in% c("Neither male nor female", "Not STH", "STH"))
  # check again
table(attendance_18_22_trimmed$Category)
table(attendance_13_18$Demographic.Variable) # the categories match up

  # merge all years
    # first, change name of "Demographic.Variable" to Category
colnames(attendance_13_18)[6] <- "Category"  
    # check column names for both df's
matrix(colnames(attendance_13_18))
matrix(colnames(attendance_18_22_trimmed))
    # remove "Demographic.Category" and "demographic category"
    # also remove "X..Contributing.20..Total.Days", and "X..Contributing.10..Total.Days.and.1..Pres.Day"
attendance_13_18 <- attendance_13_18[,c(-5,-11)]
attendance_18_22_trimmed <- attendance_18_22_trimmed[,c(-10,-13)]
    # check column names for both df's (again)
matrix(colnames(attendance_13_18))
matrix(colnames(attendance_18_22_trimmed))
    # rearrange to match columns
attendance_18_22_trimmed <- attendance_18_22_trimmed[,c(1:3,5,4,6:11)] %>% 
  filter(Year != "2018-19") # and remove overlapping year
    # row bind together 
attendance_13_22 <- rbind(attendance_13_18, attendance_18_22_trimmed)
table(attendance_13_22$Year) # over 13,000 observations for each year
table(attendance_13_22$Grade) # some schools have higher grades
    # limit observations to grades K-5
attendance_13_22_k5 <- attendance_13_22 %>% 
  filter(Grade %in% c("0K", "1", "2", "3", "4","5")) %>% 
  mutate(across(6:11, ~ gsub(",", "", .))) %>%  # Remove commas
  mutate(across(6:11, as.numeric)) %>% # convert to numeric
  arrange(DBN, Category, Year) # arrange data 
    # clean up (remove)
rm(list=c("attendance_13_18", "attendance_18_22", "attendance_18_22_trimmed"))
    
# test code
test <-attendance_13_22_k5 %>% 
  filter(Grade %in% c("0K", "1", "2", "3", "4","5")) %>% 
  mutate(across(5:10, ~ gsub(",", "", .))) %>%  # Remove commas
  mutate(across(5:10, as.numeric)) %>% 
  arrange(DBN, Category, Year)
test_pct <- test %>% 
  group_by(DBN, Category, Year) %>% 
  summarize(chronic_absent_pct = mean(`X..Chronically.Absent.1`, na.rm = T), 
            .groups = 'drop') 


  
rm(attendance_13_21)  


## DEMOGRAPHIC DATA 
demographics_13_17 <- read.csv("demographic_snapshot_school_2013-2017.csv")
# update this: new data that goes until year starting in  2023
demographics_17_21 <- read.csv("demographic_snapshot_school_2017-2021.csv")
summary(attendance_13_18)
summary(attendance_18_22)
summary(demographics_13_17) # the variable names are sometimes different and whether they are presented as proportions or percents
summary(demographics_17_21)



