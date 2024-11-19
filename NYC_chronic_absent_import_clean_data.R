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
# Load data in from .csv files - these are also available as API data from OpenNYC
attendance_13_18 <- read.csv("school_attendance_2013-2018.csv")
  # replace this with data that has all the dem categories
#attendance_18_22 <- read.csv("school_attendance_2018-2022.csv")
demographics_13_17 <- read.csv("demographic_snapshot_school_2013-2017.csv")
demographics_17_21 <- read.csv("demographic_snapshot_school_2017-2021.csv")
summary(attendance_13_18)
summary(attendance_18_22)
summary(demographics_13_17) # the variable names are sometimes different and whether they are presented as proportions or percents
summary(demographics_17_21)

table(attendance_13_18$Demographic.Category)

