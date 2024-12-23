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

### ATTENDANCE DATA Load & Clean ####
# Load data in from .csv files
# these are also available as API data from OpenNYC but the data sometimes changes structure...
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
  
### DEMOGRAPHIC DATA Load & Clean ####
demographics_13_17 <- read.csv("demographic_snapshot_school_2013-2017.csv")
demographics_17_21 <- read.csv("demographic_snapshot_school_2017-2021.csv")
demographics_19_23 <- read.csv("demographic_snapshot_school_2019-2023.csv")

  # intitial check of data discrepancies
str(demographics_13_17) # the variable names are sometimes different 
str(demographics_17_21) # also, some vars are not numeric or are displayed in proportions (decimals) rather than rates (%)
str(demographics_19_23) # for example, in the latter two data sets, poverty is a character (includes "%")

  # check out the column names
matrix(colnames(demographics_13_17))
matrix(colnames(demographics_13_17)) # I use matrix here so it prints in one column
matrix(colnames(demographics_19_23))

  # select variables for each df and clean
print(cat(paste0("'", colnames(demographics_13_17), "',"))) # print these so I can copy them and take what I want

dem_13_17_select <- demographics_13_17 %>% 
  select('DBN', 
         'School.Name', 
         'Year',
         'Grade.K', # number of students per/grade
         'Grade.1', 
         'Grade.2',
         'Grade.3',
         'Grade.4',
         'Grade.5', 
         'X..Female.1', # variables followed by ".1" are rates
         'X..Male.1', # N's are not helpful here because data is only at school level
         'X..Asian.1', # and schools vary in how many grades they have
         'X..Black.1', 
         'X..Hispanic.1',
         'X..Multiple.Race.Categories.Not.Represented.1',
         'X..White.1',
         'X..Students.with.Disabilities.1',
         'X..English.Language.Learners.1',
         'X..Poverty.1') %>% 
  rename("X..Other.1" = "X..Multiple.Race.Categories.Not.Represented.1") %>% # Other category to synchronize with attendance data
  rowwise() %>%                                                              # We do know the number of students per grade 
  mutate(Total.Students = sum(c_across(Grade.K:Grade.5), na.rm = TRUE)) %>% # arrange by row to create new total students variable
  ungroup() %>%  # ungroup data (no longer rowwise())  
  filter(Total.Students>100) # remove any schools that have less than 100 students. This is probably mostly (or all) schools that do not have K-5 at all. 

  # same thing for other two df's
print(cat(paste0("'", colnames(demographics_17_21), "',")))

  # list to change to whole number (percentages) from proportions
multiply <- c("X..Female.1", 
              "X..Male.1", 
              "X..Asian.1",
              "X..Black.1",
              "X..Hispanic.1",
              "X..Other.1",
              "X..White.1", 
              "X..Students.with.Disabilities.1",
              "X..English.Language.Learners.1")
    #2018 data
dem_18_select <- demographics_17_21 %>% 
  filter(Year == "2018-19") %>%  # only need this year from this data
  select('DBN',
         'School.Name',
         'Year',
         'Grade.K',
         'Grade.1',
         'Grade.2',
         'Grade.3',
         'Grade.4',
         'Grade.5',
         'X..Female.1',
         'X..Male.1',
         'X..Asian.1',
         'X..Black.1', 
         'X..Hispanic.1',
         'X..Multi.Racial.1',
         'X..Native.American.1',
         'X..White.1',
         'X..Students.with.Disabilities.1',
         'X..English.Language.Learners.1',
         'X..Poverty.1') %>% 
  mutate(X..Poverty.1 = gsub("%", "", X..Poverty.1), # get rid of "%" from poverty rate
         # convert all "Above 95%" to 100
         X..Poverty.1 = ifelse(X..Poverty.1 == "Above 95", 100, as.numeric(X..Poverty.1))) %>% 
  rowwise() %>% # arrange by row to create new total students variable
  mutate(Total.Students = sum(c_across(Grade.K:Grade.5), na.rm = TRUE),
         # combine multi-racial and Native American to Other category to synchronize with attendance data
         X..Other.1 = sum(c_across(c(`X..Multi.Racial.1`, `X..Native.American.1`)), na.rm = TRUE)) %>%
  ungroup() %>% # ungroup data (no longer rowwise())
  mutate_at(vars(all_of(multiply)), ~ . * 100) %>%  # multiply by 100 to make percentages
  select(-c('X..Multi.Racial.1', # these categories were combined into Other. Remove them
            'X..Native.American.1')) %>% 
  filter(Total.Students>100)  # remove any schools that have less than 100 students. This is probably mostly (or all) schools that do not have K-5 at all.

    # 2019-2022
print(cat(paste0("'", colnames(demographics_19_23), "',")))

dem_19_22_select <- demographics_19_23 %>% 
  filter(Year != "2023-24") %>%  # there is not corresponding attendance data for 2023-24. Exclude it. 
  select('DBN',
         'School.Name',
         'Year',
         'Grade.K',
         'Grade.1',
         'Grade.2',
         'Grade.3',
         'Grade.4',
         'Grade.5',
         'X..Female.1',
         'X..Male.1',
         'X..Asian.1',
         'X..Black.1', 
         'X..Hispanic.1',
         'X..Multi.Racial.1',
         'X..Native.American.1',
         'X..White.1',
         'X..Students.with.Disabilities.1',
         'X..English.Language.Learners.1',
         'X..Poverty.1') %>% 
  mutate(X..Poverty.1 = ifelse(X..Poverty.1 == "Above 95%", 100, X..Poverty.1),
         across(c(4:20), ~ as.numeric(gsub("%", "", .)))) %>%
  rowwise() %>% # arrange by row to create new total students variable
  mutate(Total.Students = sum(c_across(Grade.K:Grade.5), na.rm = TRUE),
         # combine multi-racial and Native American to Other category to synchronize with attendance data
         X..Other.1 = sum(c_across(c(`X..Multi.Racial.1`, `X..Native.American.1`)), na.rm = TRUE)) %>%
  ungroup() %>% # ungroup data (no longer rowwise())
  select(-c('X..Multi.Racial.1', # these categories were combined into Other. Remove them
            'X..Native.American.1')) %>% 
  filter(Total.Students>100)  # remove any schools that have less than 100 students. This is probably mostly (or all) schools that do not have K-5 at all.

# rearrange columns to match
matrix(colnames(dem_13_17_select))
dem_13_17_select <- dem_13_17_select[,c(1:9,20,10:19)] 
matrix(colnames(dem_18_select))
dem_18_select <- dem_18_select[,c(1:9,19,10:14,20,15:18)]
matrix(colnames(dem_19_22_select))
dem_19_22_select <- dem_19_22_select[,c(1:9,19,10:14,20,15:18)]

# row bind all years of clean demographic data
demographics_13_22 <- rbind(dem_13_17_select, dem_18_select, dem_19_22_select)

### Checkpoint ####

# graphs in new R script