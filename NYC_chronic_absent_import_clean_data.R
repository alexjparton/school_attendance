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
data_frames <- list(attendance_18_22_all, attendance_18_22_ELL, attendance_18_22_ethnicity,
                    attendance_18_22_gender, attendance_18_22_poverty, attendance_18_22_SWD,
                    attendance_18_22_STH)
  # names of demographic categories
  # same order as data_frames listed above
dem_cat_names <- list("All Students", "ELL Status", "Ethnicity", "Gender", "Poverty", "SWD Status",
                  "STH Status")
  # function to add the demographic category
add_category_column <- function(df, category) {
    df %>%
      mutate(Demographic.Category = category)
  }
  # apply the function to each data frame in the list
modified_dfs <- map2(data_frames, dem_cat_names, add_category_column)
  # bind everything together
attendance_18_22 <- bind_rows(modified_dfs)
  # change "Category" column to match older df: "Demographic.Variable"
colnames(attendance_18_22)[4] <- "Demographic.Variable"
  # For merging these df's, we need to match up all the variables 
table(attendance_18_22$Demographic.Variable) # this newer data includes a few more entries, e.g. "Neither male nor female"
table(attendance_13_18$Demographic.Variable) 

  # clean up (remove)
rm(list=c("attendance_18_22_all", "attendance_18_22_ELL", "attendance_18_22_ethnicity", "attendance_18_22_ethnicity", 
          "attendance_18_22_gender", "attendance_18_22_gender", "attendance_18_22_poverty", "attendance_18_22_STH", 
          "attendance_18_22_SWD", "add_category_column", "dem_cat_names", "data_frames", 
          "modified_dfs"))
  # remove extra categories from newer data
attendance_18_22_trimmed <- attendance_18_22 %>% 
  filter(!Demographic.Variable %in% c("Neither male nor female")) # remove these because of low N's
  # check again
table(attendance_18_22_trimmed$Demographic.Variable) # newer data has STH and Not STH - OK. 
table(attendance_13_18$Demographic.Variable) 
  # merge all years
    # check column names for both df's
matrix(colnames(attendance_13_18))
matrix(colnames(attendance_18_22_trimmed))
    # remove "X..Contributing.20..Total.Days", and "X..Contributing.10..Total.Days.and.1..Pres.Day"
attendance_13_18_finalvars <- attendance_13_18[,-11]
attendance_18_22_finalvars <- attendance_18_22_trimmed[,-10]
    # check column names for both df's (again)
matrix(colnames(attendance_13_18_finalvars))
matrix(colnames(attendance_18_22_finalvars))
    # rearrange to match columns
attendance_18_22_formerge <- attendance_18_22_finalvars[,c(1:3,5,12,4,6:11)] 
    # remove 2018-19 from earlier data to retain STH variable for max years
attendance_13_17_formerge <- attendance_13_18_finalvars %>% 
  filter(Year != "2018-19") 
    # row bind together 
attendance_13_22 <- rbind(attendance_13_17_formerge, attendance_18_22_formerge)
table(attendance_13_22$Year) # over 13,000 observations for each year (more when STH is included in later years)
table(attendance_13_22$Grade) # some schools have higher grades

    #clean up (remove)
rm(list=setdiff(ls(envir = .GlobalEnv), "attendance_13_22"), envir = .GlobalEnv)

# SAVE this to a csv. for easy retrieval 
write.csv(attendance_13_22, "clean_attendance_13_22_all_grades.csv")
    
  # limit observations to grades K-5
attendance_13_22_k5 <- attendance_13_22 %>% 
  filter(Grade %in% c("0K", "1", "2", "3", "4","5")) %>% 
  mutate(across(7:12, ~ gsub(",", "", .))) %>%  # Remove commas
  mutate(across(7:12, as.numeric)) %>% # convert to numeric
  arrange(DBN,Year, Demographic.Category) # arrange data 

# SAVE this to a csv. for easy retrieval
write.csv(attendance_13_22_k5, "clean_attendance_13_22_k5.csv")
  
### Checkpoint ####

# Calculate chronic absentee rates and put it in a dataframe
  # test code

test_pct <- attendance_13_22_k5 %>% 
  group_by(DBN, School.Name, Year, Demographic.Category, Demographic.Variable) %>% 
  # chronic absent percent and total per school, per year, per category and variable
  summarize(chronic_absent_pct = round(mean(`X..Chronically.Absent.1`, na.rm = T),1), 
            chronic_absent_n = sum(`X..Chronically.Absent`, na.rm=T), 
            .groups = 'drop') 
  # need to add demographics so we can know total number of students


  
## DEMOGRAPHIC DATA 
demographics_13_17 <- read.csv("demographic_snapshot_school_2013-2017.csv")
# update this: new data that goes until year starting in  2023
demographics_17_21 <- read.csv("demographic_snapshot_school_2017-2021.csv")
summary(attendance_13_18)
summary(attendance_18_22)
summary(demographics_13_17) # the variable names are sometimes different and whether they are presented as proportions or percents
summary(demographics_17_21)



