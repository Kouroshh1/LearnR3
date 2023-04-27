# Load packages

library(tidyverse)
library(NHANES)

# looking at data

glimpse(NHANES)

# Selecting columns

select(NHANES, Age)

select(NHANES, Age, Weight, BMI)

select(NHANES, -HeadCirc) # removing column HeadCirc

select(NHANES, starts_with("BP"))

select(NHANES, ends_with("Day"))

select(NHANES, contains("Age"))

# create smaller NHANES dataset

nhanes_small <- select(NHANES, Age, Gender, BMI, Diabetes, PhysActive, BPSysAve, BPDiaAve, Education)

nhanes_small

# Renaming columns

nhanes_small <- rename_with(nhanes_small, snakecase::to_snake_case)

nhanes_small

# Renmaing specific

nhanes_small <- rename(nhanes_small, sex = gender)

# Trying out the pipe

colnames(nhanes_small)
nhanes_small %>% colnames()

nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)


nhanes_small %>%
  select(bp_sys_ave, education) %>%
  rename(bp_sys = bp_sys_ave, bp_dia = bp_dias_ave)

select(nhanes_small, bmi, contains("age"))

nhanes_small %>% select(bmi, contains("age"))

nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)



# Filtering

nhanes_small %>% filter(phys_active == "No")

nhanes_small %>% filter(phys_active != "No") # only yes

nhanes_small %>% filter(bmi >= 25)

# Combining logical operators

nhanes_small %>% filter(bmi >= 25 & phys_active == "No")

nhanes_small %>% filter(bmi >= 25 | phys_active == "No")


# Arrange data

nhanes_small %>% arrange(age)

nhanes_small %>% arrange(desc(age))


nhanes_small %>% arrange(education, age)

# Transform data

nhanes_small %>% mutate(age = age * 12, logged_bmi = log(bmi))

nhanes_small %>% mutate(old = if_else(age >= 30, "Yes", "No"))





# 1. BMI between 20 and 40 with diabetes
nhanes_small %>%
  # Format should follow: variable >= number or character
  filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes")

# Pipe the data into mutate function and:
nhanes_modified <- nhanes_small %>% mutate(
  mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave) / 3,
  young_child = if_else(age < 6, "Yes", "No")
)


# creating summary statistics

nhanes_small %>%  summarise(max_bmi = max(bmi, na.rm = TRUE),
                            min_bmi = min(bmi, na.rm = TRUE)) #

nhanes_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE),min_bmi = min(bmi, na.rm = TRUE)) %>%
 ungroup()

# saving data

readr::write_csv(nhanes_small,
                 here::here("data/nhanes_small.csv"))











